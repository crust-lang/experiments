use std::io::{self, Read, Write};

fn main() {
    loop {
        io::stdout().write(b"lispy> ");
        io::stdout().flush();
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer).unwrap();
        buffer.pop();
        println!("No you're a {}", buffer);
    }
}
