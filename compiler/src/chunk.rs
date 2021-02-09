use crate::Value;

#[derive(Default)]
pub struct Chunk {
    // Some op codes, some args with other types
    pub(crate) code: Vec<u8>,
    // line numbers of each code point
    // TODO PERF: holy wow this is a terrible representation, contains many many consecutives, is much larger than the code itself
    pub(crate) lines: Vec<usize>,
    // any constants encountered along the way
    pub(crate) constants: Vec<Value>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Default::default()
    }

    pub fn write_chunk(&mut self, code: u8, line: usize) {
        self.code.push(code);
        self.lines.push(line);
    }

    pub fn add_constant<T: Into<Value>>(&mut self, val: T) -> usize {
        self.constants.push(val.into());
        self.constants.len() - 1
    }

    // By inlining we have a zero-overhead way of exposing READING the bytes without
    // exposing the field
    #[inline(always)]
    pub fn read_byte(&self, ip: usize) -> u8 {
        self.code[ip]
    }

    #[inline(always)]
    pub fn get_constant(&self, ind: usize) -> Value {
        self.constants[ind]
    }
}
