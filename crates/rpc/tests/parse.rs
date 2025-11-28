use rpc::{parse_request, decode_framed};

#[test]
fn parse_simple_request() {
    let req = parse_request(br#"{"jsonrpc":"2.0","id":1,"method":"ping"}"#).unwrap();
    assert_eq!(req.method, "ping");
    assert_eq!(req.id.unwrap(), 1);
}

#[test]
fn decode_framed_request() {
    let msg = "Content-Length: 17\r\n\r\n{\"jsonrpc\":\"2.0\"}";
    let (body, _) = decode_framed(msg.as_bytes()).unwrap();
    assert_eq!(String::from_utf8(body).unwrap(), "{\"jsonrpc\":\"2.0\"}");
}
