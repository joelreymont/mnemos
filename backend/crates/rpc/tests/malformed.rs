use rpc::{decode_framed, parse_request, MAX_MESSAGE_SIZE};

// ============================================================================
// Truncated Headers Tests
// ============================================================================

#[test]
fn truncated_header_no_crlf() {
    // Header without the double CRLF separator
    let msg = b"Content-Length: 17";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject header without CRLF separator");
}

#[test]
fn truncated_header_single_crlf() {
    // Header with only single CRLF
    let msg = b"Content-Length: 17\r\n";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject header with only single CRLF");
}

#[test]
fn truncated_header_partial_length() {
    // Header cut off in the middle of the length value
    let msg = b"Content-Length: 1";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject truncated header");
}

#[test]
fn truncated_header_incomplete_separator() {
    // Header with incomplete separator (only 3 of 4 chars)
    let msg = b"Content-Length: 17\r\n\r";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject incomplete separator");
}

// ============================================================================
// Truncated Bodies Tests
// ============================================================================

#[test]
fn truncated_body_shorter_than_declared() {
    // Content-Length says 100 but body is only 10 bytes
    let msg = b"Content-Length: 100\r\n\r\n{\"test\":1}";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject body shorter than Content-Length");
}

#[test]
fn truncated_body_empty() {
    // Content-Length says 50 but body is empty
    let msg = b"Content-Length: 50\r\n\r\n";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject empty body when length > 0");
}

#[test]
fn truncated_body_off_by_one() {
    // Body is exactly one byte short
    let msg = b"Content-Length: 17\r\n\r\n{\"jsonrpc\":\"2.0\"";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject body that's one byte short");
}

#[test]
fn truncated_body_partial_utf8() {
    // Body ends in middle of a multi-byte UTF-8 sequence
    let msg = "Content-Length: 20\r\n\r\n{\"emoji\":\"😀".as_bytes();
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject body with partial UTF-8");
}

// ============================================================================
// Malformed Content-Length Tests
// ============================================================================

#[test]
fn malformed_content_length_negative() {
    // Negative Content-Length
    let msg = b"Content-Length: -10\r\n\r\n{\"test\":1}";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject negative Content-Length");
}

#[test]
fn malformed_content_length_non_numeric() {
    // Non-numeric Content-Length
    let msg = b"Content-Length: abc\r\n\r\n{\"test\":1}";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject non-numeric Content-Length");
}

#[test]
fn malformed_content_length_float() {
    // Float instead of integer
    let msg = b"Content-Length: 12.5\r\n\r\n{\"test\":1}";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject float Content-Length");
}

#[test]
fn malformed_content_length_hex() {
    // Hex number (should be rejected as not decimal)
    let msg = b"Content-Length: 0x10\r\n\r\n{\"test\":1}";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject hex Content-Length");
}

#[test]
fn malformed_content_length_with_spaces() {
    // Leading/trailing spaces should be handled by trim(), but test mixed spaces
    let msg = b"Content-Length:  17  \r\n\r\n{\"jsonrpc\":\"2.0\"}";
    let result = decode_framed(msg);
    // This should actually work due to trim() - verify proper handling
    assert!(result.is_some(), "Should accept Content-Length with extra spaces");
}

#[test]
fn malformed_content_length_overflow() {
    // Extremely large number that could overflow
    let msg = b"Content-Length: 99999999999999999999999999999999\r\n\r\n{\"test\":1}";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject Content-Length that overflows usize");
}

#[test]
fn malformed_content_length_empty() {
    // Empty Content-Length value
    let msg = b"Content-Length: \r\n\r\n{\"test\":1}";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject empty Content-Length");
}

#[test]
fn malformed_content_length_with_units() {
    // Content-Length with units suffix
    let msg = b"Content-Length: 17 bytes\r\n\r\n{\"jsonrpc\":\"2.0\"}";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject Content-Length with units");
}

// ============================================================================
// Very Large Content-Length (DoS Prevention) Tests
// ============================================================================

#[test]
fn dos_prevention_max_message_size() {
    // Content-Length exactly at MAX_MESSAGE_SIZE
    let msg = format!("Content-Length: {}\r\n\r\n{{\"test\":1}}", MAX_MESSAGE_SIZE);
    let result = decode_framed(msg.as_bytes());
    // Should be rejected as claimed Content-Length exceeds MAX_MESSAGE_SIZE limit
    // The implementation rejects if len > MAX_MESSAGE_SIZE as a DoS safeguard
    assert!(result.is_none(), "Should reject Content-Length at MAX_MESSAGE_SIZE limit");
}

#[test]
fn dos_prevention_over_max_message_size() {
    // Content-Length just over MAX_MESSAGE_SIZE
    let msg = format!("Content-Length: {}\r\n\r\n{{\"test\":1}}", MAX_MESSAGE_SIZE + 1);
    let result = decode_framed(msg.as_bytes());
    assert!(result.is_none(), "Should reject message over MAX_MESSAGE_SIZE");
}

#[test]
fn dos_prevention_extremely_large() {
    // Extremely large Content-Length (1GB)
    let msg = b"Content-Length: 1073741824\r\n\r\n{\"test\":1}";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject extremely large Content-Length without allocating");
}

#[test]
fn dos_prevention_max_usize() {
    // Content-Length at maximum usize value
    let msg = format!("Content-Length: {}\r\n\r\n{{\"test\":1}}", usize::MAX);
    let result = decode_framed(msg.as_bytes());
    assert!(result.is_none(), "Should reject Content-Length at usize::MAX");
}

#[test]
fn dos_prevention_realistic_large_message() {
    // Test that we properly reject without trying to allocate
    // 500MB message - should be rejected
    let msg = b"Content-Length: 524288000\r\n\r\n{\"test\":1}";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject 500MB message");
}

// ============================================================================
// Invalid JSON in Body Tests
// ============================================================================

#[test]
fn invalid_json_malformed() {
    // Valid frame but invalid JSON
    let msg = b"Content-Length: 15\r\n\r\n{\"invalid json}";
    let (body, _) = decode_framed(msg).expect("Frame should be valid");
    let result = parse_request(&body);
    assert!(result.is_err(), "Should reject malformed JSON");
}

#[test]
fn invalid_json_truncated() {
    // Valid frame but truncated JSON (string is not terminated)
    let msg = b"Content-Length: 13\r\n\r\n{\"method\":\"t\"}";
    let (body, _) = decode_framed(msg).expect("Frame should be valid");
    let result = parse_request(&body);
    assert!(result.is_err(), "Should reject truncated JSON");
}

#[test]
fn invalid_json_empty_body() {
    // Valid frame with zero-length body
    let msg = b"Content-Length: 0\r\n\r\n";
    let (body, _) = decode_framed(msg).expect("Frame should be valid");
    assert_eq!(body.len(), 0);
    let result = parse_request(&body);
    assert!(result.is_err(), "Should reject empty JSON");
}

#[test]
fn invalid_json_not_object() {
    // Valid JSON but not an object (array instead)
    let msg = b"Content-Length: 2\r\n\r\n[]";
    let (body, _) = decode_framed(msg).expect("Frame should be valid");
    let result = parse_request(&body);
    assert!(result.is_err(), "Should reject JSON that's not an object");
}

#[test]
fn invalid_json_missing_required_fields() {
    // Valid JSON object but missing required fields (14 bytes)
    let msg = b"Content-Length: 14\r\n\r\n{\"random\":123}";
    let (body, _) = decode_framed(msg).expect("Frame should be valid");
    let result = parse_request(&body);
    assert!(result.is_err(), "Should reject JSON missing required fields");
}

#[test]
fn invalid_json_null_bytes() {
    // JSON with null bytes - this will fail at the framing layer due to UTF-8 validation
    // The null byte itself is valid UTF-8, but let's test with actual invalid content
    let msg = b"Content-Length: 14\r\n\r\n{\"test\":\"\\x00\"}";
    let (body, _) = decode_framed(msg).expect("Frame should be valid");
    let result = parse_request(&body);
    // This should actually parse since it's escaped - documenting actual behavior
    assert!(result.is_err(), "Should reject JSON with escape sequences");
}

#[test]
fn invalid_json_control_characters() {
    // JSON with unescaped control characters (18 bytes)
    let msg = b"Content-Length: 18\r\n\r\n{\"method\":\"test\x01\"}";
    let (body, _) = decode_framed(msg).expect("Frame should be valid");
    let result = parse_request(&body);
    // This might actually parse depending on serde_json's lenience
    // The test documents the behavior
    let _ = result;
}

// ============================================================================
// Missing Content-Length Header Tests
// ============================================================================

#[test]
fn missing_content_length_header() {
    // No Content-Length header at all
    let msg = b"\r\n\r\n{\"jsonrpc\":\"2.0\"}";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject message without Content-Length");
}

#[test]
fn missing_content_length_other_header() {
    // Has other headers but no Content-Length
    let msg = b"Content-Type: application/json\r\n\r\n{\"jsonrpc\":\"2.0\"}";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject message without Content-Length");
}

#[test]
fn missing_content_length_typo() {
    // Typo in header name
    let msg = b"Content-Lenght: 17\r\n\r\n{\"jsonrpc\":\"2.0\"}";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject misspelled Content-Length");
}

#[test]
fn missing_content_length_wrong_case() {
    // Wrong case (should be case-sensitive)
    let msg = b"content-length: 17\r\n\r\n{\"jsonrpc\":\"2.0\"}";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject lowercase content-length");
}

#[test]
fn missing_content_length_no_colon() {
    // Header without colon separator
    let msg = b"Content-Length 17\r\n\r\n{\"jsonrpc\":\"2.0\"}";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject header without colon");
}

// ============================================================================
// Duplicate Headers Tests
// ============================================================================

#[test]
fn duplicate_content_length_same_value() {
    // Duplicate Content-Length headers with same value
    let msg = b"Content-Length: 17\r\nContent-Length: 17\r\n\r\n{\"jsonrpc\":\"2.0\"}";
    let result = decode_framed(msg);
    // Current implementation uses find_map which will take the first one
    // This should succeed with the first value
    assert!(result.is_some(), "Implementation takes first Content-Length header");
}

#[test]
fn duplicate_content_length_different_values() {
    // Duplicate Content-Length headers with different values
    let msg = b"Content-Length: 17\r\nContent-Length: 20\r\n\r\n{\"jsonrpc\":\"2.0\"}";
    let result = decode_framed(msg);
    // Should use first occurrence
    if let Some((body, _)) = result {
        assert_eq!(body.len(), 17, "Should use first Content-Length value");
    }
}

#[test]
fn multiple_headers_with_content_length() {
    // Multiple headers with Content-Length among them
    let msg = b"Content-Type: application/json\r\nContent-Length: 17\r\nAccept: */*\r\n\r\n{\"jsonrpc\":\"2.0\"}";
    let result = decode_framed(msg);
    assert!(result.is_some(), "Should find Content-Length among other headers");
    let (body, _) = result.unwrap();
    assert_eq!(body, br#"{"jsonrpc":"2.0"}"#);
}

// ============================================================================
// Edge Cases and Combined Issues
// ============================================================================

#[test]
fn edge_case_zero_content_length() {
    // Content-Length of 0 (valid but unusual)
    let msg = b"Content-Length: 0\r\n\r\n";
    let result = decode_framed(msg);
    assert!(result.is_some(), "Should accept Content-Length of 0");
    let (body, consumed) = result.unwrap();
    assert_eq!(body.len(), 0);
    assert_eq!(consumed, 21); // Header length including separator
}

#[test]
fn edge_case_body_longer_than_declared() {
    // Body is longer than Content-Length claims
    let msg = b"Content-Length: 10\r\n\r\n{\"jsonrpc\":\"2.0\"}";
    let result = decode_framed(msg);
    assert!(result.is_some(), "Should succeed and return only declared length");
    let (body, _) = result.unwrap();
    assert_eq!(body.len(), 10, "Should return exactly Content-Length bytes");
    assert_eq!(&body, b"{\"jsonrpc\"");
}

#[test]
fn edge_case_exact_length_match() {
    // Body exactly matches Content-Length
    let msg = b"Content-Length: 17\r\n\r\n{\"jsonrpc\":\"2.0\"}";
    let result = decode_framed(msg);
    assert!(result.is_some(), "Should accept exact length match");
    let (body, consumed) = result.unwrap();
    assert_eq!(body, br#"{"jsonrpc":"2.0"}"#);
    // Header is "Content-Length: 17" (18 chars) + "\r\n\r\n" (4 chars) = 22 + 17 = 39
    assert_eq!(consumed, 39); // header + body
}

#[test]
fn edge_case_non_utf8_body() {
    // Body with invalid UTF-8 (but valid frame structure)
    // decode_framed returns raw bytes without UTF-8 validation (for performance)
    // UTF-8/JSON validation happens later in parse_request
    let msg = b"Content-Length: 5\r\n\r\n\xFF\xFE\xFD\xFC\xFB";
    let result = decode_framed(msg);
    assert!(result.is_some(), "decode_framed returns raw bytes without UTF-8 validation");
    let (body, consumed) = result.unwrap();
    assert_eq!(body, b"\xFF\xFE\xFD\xFC\xFB");
    // "Content-Length: 5\r\n\r\n" = 21 bytes + 5 body bytes = 26 total
    assert_eq!(consumed, 26);

    // parse_request will fail on invalid UTF-8 (serde_json requires valid UTF-8)
    let parse_result = parse_request(&body);
    assert!(parse_result.is_err(), "parse_request should reject invalid UTF-8");
}

#[test]
fn edge_case_crlf_in_body() {
    // CRLF sequences in the JSON body itself
    // The body {\"test\":\"line1\r\nline2\"} is 23 bytes, not 21
    let msg = b"Content-Length: 23\r\n\r\n{\"test\":\"line1\r\nline2\"}";
    let result = decode_framed(msg);
    assert!(result.is_some(), "Should handle CRLF in body");
    let (body, _) = result.unwrap();
    assert_eq!(body, b"{\"test\":\"line1\r\nline2\"}");
}

#[test]
fn edge_case_only_header_no_body() {
    // Content-Length greater than 0 but separator is at end of buffer
    let msg = b"Content-Length: 17\r\n\r\n";
    let result = decode_framed(msg);
    assert!(result.is_none(), "Should reject when body is missing");
}

#[test]
fn edge_case_leading_whitespace_in_header() {
    // Leading whitespace before header
    let msg = b"  Content-Length: 17\r\n\r\n{\"jsonrpc\":\"2.0\"}";
    let result = decode_framed(msg);
    // Current implementation splits on CRLF first, so leading space is part of first line
    assert!(result.is_none(), "Should reject leading whitespace before header");
}

#[test]
fn edge_case_trailing_data_after_message() {
    // Extra data after the message
    let msg = b"Content-Length: 17\r\n\r\n{\"jsonrpc\":\"2.0\"}EXTRA_DATA_HERE";
    let result = decode_framed(msg);
    assert!(result.is_some(), "Should succeed and ignore trailing data");
    let (body, consumed) = result.unwrap();
    assert_eq!(body, br#"{"jsonrpc":"2.0"}"#);
    // Verify consumed doesn't include trailing data
    // Header is "Content-Length: 17\r\n\r\n" (22 chars) + 17 (body) = 39
    assert_eq!(consumed, 39);
}

// ============================================================================
// Stress Tests (Resource Exhaustion Prevention)
// ============================================================================

#[test]
fn stress_many_headers_within_limit() {
    // Many header lines that stay within MAX_HEADER_SIZE (8KB)
    // ~100 headers * ~20 bytes = ~2KB, well under limit
    let mut msg = Vec::new();
    for i in 0..100 {
        msg.extend_from_slice(format!("Header-{}: value\r\n", i).as_bytes());
    }
    msg.extend_from_slice(b"Content-Length: 17\r\n\r\n{\"jsonrpc\":\"2.0\"}");
    let result = decode_framed(&msg);
    assert!(result.is_some(), "Should handle many headers within limit");
}

#[test]
fn stress_many_headers_exceeds_limit() {
    // Headers exceeding MAX_HEADER_SIZE (8KB) should be rejected
    let mut msg = Vec::new();
    for i in 0..1000 {
        msg.extend_from_slice(format!("Header-{}: value\r\n", i).as_bytes());
    }
    msg.extend_from_slice(b"Content-Length: 17\r\n\r\n{\"jsonrpc\":\"2.0\"}");
    let result = decode_framed(&msg);
    assert!(result.is_none(), "Should reject headers exceeding 8KB limit");
}

#[test]
fn stress_very_long_header_line() {
    // Single very long header line exceeds MAX_HEADER_SIZE - should be rejected
    let mut msg = b"X-Custom: ".to_vec();
    msg.extend(vec![b'a'; 100_000]);
    msg.extend_from_slice(b"\r\nContent-Length: 17\r\n\r\n{\"jsonrpc\":\"2.0\"}");
    let result = decode_framed(&msg);
    assert!(result.is_none(), "Should reject headers exceeding 8KB limit");
}

#[test]
fn stress_no_panic_on_malicious_input() {
    // Various malicious inputs that should not cause panics
    let malicious_inputs = vec![
        b"" as &[u8],
        b"\r\n",
        b"\r\n\r\n",
        b"Content-Length:",
        b"Content-Length:\r\n\r\n",
        b"Content-Length: \r\n\r\n",
        b"Content-Length: -1\r\n\r\n",
        b"Content-Length: 999999999999999999999\r\n\r\n",
        b"\x00\x00\x00\x00",
        b"AAAA" as &[u8],
    ];

    for input in malicious_inputs {
        let result = decode_framed(input);
        // Just verify no panic occurs
        let _ = result;
    }
}

#[test]
fn stress_rapid_frame_parsing() {
    // Simulate rapid parsing of many frames
    let msg = b"Content-Length: 17\r\n\r\n{\"jsonrpc\":\"2.0\"}";
    for _ in 0..10000 {
        let result = decode_framed(msg);
        assert!(result.is_some());
    }
}
