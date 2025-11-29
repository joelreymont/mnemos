use rpc::decode_framed;

#[test]
fn decode_multiple_messages() {
    let msg = b"Content-Length: 17\r\n\r\n{\"jsonrpc\":\"2.0\"}Content-Length: 17\r\n\r\n{\"jsonrpc\":\"2.0\"}";
    let (body1, consumed1) = decode_framed(msg).unwrap();
    assert_eq!(body1, br#"{"jsonrpc":"2.0"}"#);
    let (body2, _consumed2) = decode_framed(&msg[consumed1..]).unwrap();
    assert_eq!(body2, br#"{"jsonrpc":"2.0"}"#);
}
