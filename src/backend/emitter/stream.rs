pub struct Stream {
    stream: Vec<u8>,
}

impl Stream {
    pub fn new() -> Self {
        Self { stream: Vec::new() }
    }

    pub fn write(&mut self, v: &str) {
        self.stream.extend_from_slice(v.as_bytes());
    }

    pub fn to_string(self) -> String {
        String::from_utf8(self.stream).unwrap()
    }
}
