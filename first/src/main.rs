use std::io;
use std::io::{Read, Write};
use std::net::{Incoming, SocketAddr, TcpListener, TcpStream};

use RedisValue::RedisError;

use crate::RedisValue::{RedisArray, RedisBulk, RedisInteger, RedisString};

const CRLF: &str = "\r\n";
const RNIL: &str = "\0\0";

#[derive(PartialEq, Debug)]
enum RedisValue {
	RedisString(String),
	RedisError(String),
	RedisInteger(i64),
	RedisBulk(String),
	RedisArray(Vec<RedisValue>),
}

impl RedisValue {
	fn encode(&self) -> String {
		return match self {
			RedisString(value) => format!("+{}{CRLF}", value),
			RedisError(message) => format!("-{}{CRLF}", message),
			RedisInteger(number) => format!(":{}{CRLF}", number),
			RedisBulk(bin) => format!("${}{}{}{}", "$", bin.len(), CRLF, bin),
			RedisArray(v) => {
				let content = v
					.iter()
					.map(|el| el.encode())
					.collect::<Vec<String>>()
					.join("");
				let array_size = v.len();
				return format!("*{}{CRLF}{}{CRLF}", array_size, content);
			}
		};
	}

	pub fn decode(s: &str) -> Result<RedisValue, String> {
		let mut chars = s.chars();

		{
			//	Validate end of message
			chars.next_back()
				 .filter(|t| *t == '\n')
				 .ok_or("Messages should end with \r\n")?;

			chars.next_back()
				 .filter(|t| *t == '\r')
				 .ok_or("Messages should end with \r\n")?;
		}
		let head = chars.next()
						.ok_or("Message head identifier not found")?;

		let body = chars.as_str().to_owned();


		return match head {
			'+' => Ok(RedisString(body)),
			'-' => Ok(RedisError(body)),
			'$' => Ok(RedisBulk(body)),
			':' => {
				return match body.parse::<i64>() {
					Ok(number) => Ok(RedisInteger(number)),
					Err(e) => Err(e.to_string())
				};
			}
			'*' => {

				let values: Result<Vec<RedisValue>, _> = body
					.split_inclusive(CRLF)
					.skip( 1 )
					.map(|el| RedisValue::decode(el))
					.collect();

				return values.map(|v| RedisArray(v));
			}
			other => {
				let message = format!("Could not decode message: \
				{} is not a valid message head", other);

				return Err(message);
			}
		};
	}
}

// fn read_body(body: &str) -> Result<RedisValue, String> {
// 	let mut size = 0;
// 	for l in body.split("\n") {
// 		if l.starts_with("Content-Length") {
// 			let sizeplit = l.split(":");
// 			for s in sizeplit {
// 				if !(s.starts_with("Content-Length")) {
// 					size = s.trim().parse::<usize>().unwrap(); //Get Content-Length
// 				}
// 			}
// 		}
// 	}
// 	let mut buffer = vec![0; size]; //New Vector with size of Content
// 	reader.read_exact(&mut buffer).unwrap(); //Get the Body Conten
// }

fn write_response(mut stream: TcpStream) {
	get_response(&mut stream)
		.map_err(|e| println!("{}", e))
		.map(|e| println!("{}", e));


	stream.write("HTTP/1.1 200 OK\r\n\r\n".as_bytes());
}

fn get_response(stream: &mut TcpStream) -> Result<String, String> {
	let mut buffer = [0u8; 1024];
	stream.read(&mut buffer)
		  .map_err(|_| "Failed to read from stream")?;

	let mut headers = [httparse::EMPTY_HEADER; 10];
	// httparse::parse_headers(&buffer, &mut headers).map_err( | e | e.to_string())?;

	let mut request = httparse::Request::new(&mut headers);
	let idx = request.parse(&buffer).map_err(|e| "")?.unwrap();
	let length = request.headers.iter()
						.find(|h| h.name == "Content-Length")
						.ok_or("no content-length")?;
	let result = String::from_utf8_lossy(length.value)
		.parse::<usize>()
		.map_err(|e| "parse error")?;


	let body = String::from_utf8_lossy(&buffer[idx..idx + result]).to_string();

	let value = RedisValue::decode(&body)?;

	println!("{}",body);
	println!("{:#?}",value);

	let msg = "done";
	return Result::Ok(msg.to_owned());


	// let body = content[idx..].chars()
	// 						 .skip_while(|c| *c != '\n')
	// 						 .collect::<String>();
	//
	//
	// let size = body.chars()
	// 			   .take_while(|c| c.is_numeric())
	// 			   .collect::<String>()
	// 			   .parse::<usize>()?;
	//
	// println!("||");
	// println!("{}", &body[..size]);
	// let value = RedisValue::decode(&body[..3])
	// 	.expect("Error decoding");
	// println!("{:?}", value);
}


fn main() {
	let listener = TcpListener::bind("localhost:8080")
		.expect("Unable to create tcp listener.");

	for x in listener.incoming() {
		x.map(write_response);
	}
}

#[cfg(test)]
mod tests {
	use proptest::proptest;

	use super::*;

	#[test]
	fn test_redis_string() {
		let encoded = RedisString(String::from("OK")).encode();
		let expected = "+OK\r\n";
		assert_eq!(encoded, expected);
	}

	#[test]
	fn test_redis_integer() {
		let encoded = RedisInteger(100).encode();
		let expected = ":100\r\n";
		assert_eq!(encoded, expected);
	}

	#[test]
	fn test_redis_array() {
		let vec = RedisArray(vec![
			RedisString(String::from("Hello World")),
			RedisInteger(23),
		]);
		let encoded = vec.encode();
		let expected = "*2\r\n+Hello World\r\n:23\r\n\r\n";

		assert_eq!(encoded, expected);

		let decoded = RedisValue::decode(expected).unwrap();
		assert_eq!(vec, decoded);
	}

	proptest! {

    #[test]
    fn prop_redis_integer( i : i64 ) {
		let base = RedisInteger( i );
		let decoded = RedisValue::decode( &base.encode() ).unwrap();
        assert_eq!(base, decoded);
    }

    }
}
