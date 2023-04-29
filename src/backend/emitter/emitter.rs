use crate::frontend::parser::{ast::TiProg, parser::TiParser};

use super::stream::Stream;

pub trait TiEmit {
    fn emit(&mut self, ast: &TiProg, stream: &mut Stream);
}

pub struct TiEmitter<'a, T: TiEmit> {
    ti_parser: TiParser<'a>,
    ti_emitter: T,
}

impl<'a, T: TiEmit> TiEmitter<'a, T> {
    pub fn new(ti_parser: TiParser<'a>, ti_emitter: T) -> Self {
        Self {
            ti_parser,
            ti_emitter,
        }
    }

    pub fn emit(&mut self) -> Stream {
        let mut stream = Stream::new();
        self.ti_emitter.emit(&self.ti_parser.ti_ast, &mut stream);
        stream
    }
}
