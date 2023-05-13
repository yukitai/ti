use std::rc::Rc;

use crate::{
    error::reporter::TiReporter,
    frontend::parser::{ast::*, parser::TiParser},
    types::types::TiType,
};

pub struct Analyzer<'a> {
    pub ti_parser: TiParser<'a>,
    ti_infer_union_map: Vec<Rc<TiType>>,
}

impl<'a> Analyzer<'a> {
    pub fn new(ti_parser: TiParser<'a>) -> Self {
        Self {
            ti_parser,
            ti_infer_union_map: Vec::new(),
        }
    }
    pub fn analyze(&mut self, ti_reporter: &mut TiReporter<'a>) {}
}
