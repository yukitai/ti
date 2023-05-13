use std::{fmt::Display, rc::Rc};

use crate::error::error::{ti_where_expand, TiWhere};

#[derive(Debug, PartialEq)]
pub enum TiTokenType {
    Ident(Rc<String>),
    LlStr(Rc<String>),
    LlNum(f64),
    LlBool(bool),
    LlNil,

    LBrace,    // (
    RBrace,    // )
    LBracket,  // [
    RBracket,  // ]
    LBra,      // {
    RBra,      // }
    FatArrow,  // =>
    ThinArrow, // ->
    Colon,     // :
    Semi,      // ;
    Comma,     // ,

    KLet,
    KFor,
    KIn,
    KWhile,
    KFn,
    KType,
    KIf,
    KElse,
    KBreak,
    KContinue,
    KReturn,
    KImpl,

    OAdd,    // +
    OSub,    // -
    OMul,    // *
    OPow,    // **
    ODiv,    // /
    OBitXor, // ^
    OBitAnd, // &
    OBitOr,  // |
    OAnd,    // &&
    OOr,     // ||
    ONot,    // !
    OEq,     // ==
    OAssign, // =
    ONeq,    // !=
    OGeq,    // >=
    OGrt,    // >
    OLeq,    // <=
    OLes,    // <
    OMember, // .
    OField,  // ::

    TNum,
    TStr,
    TBool,

    Eof,
}

#[derive(Debug)]
pub struct TiToken {
    pub ti_token_type: TiTokenType,
    pub ti_where: TiWhere,
}

impl TiToken {
    pub fn new(ti_token_type: TiTokenType, ti_where: TiWhere) -> Self {
        Self {
            ti_token_type,
            ti_where,
        }
    }
}

#[derive(Debug)]
pub struct TiTokenStream {
    ti_tokens: Vec<TiToken>,
    curr: usize,
    mark: Vec<usize>,
}

impl TiTokenStream {
    pub fn new(ti_tokens: Vec<TiToken>) -> Self {
        Self {
            ti_tokens,
            curr: 0,
            mark: Vec::with_capacity(8),
        }
    }

    pub fn mark(&mut self) {
        self.mark.push(self.curr);
    }

    pub fn mark_last(&mut self) {
        self.mark.push(self.curr - 1);
    }

    pub fn unmark(&mut self) {
        self.mark.pop();
    }

    pub fn range(&self) -> TiWhere {
        ti_where_expand(
            &self.ti_tokens[*self.mark.last().unwrap()].ti_where,
            &self.ti_tokens[self.curr - 1].ti_where,
        )
    }

    pub fn here(&self) -> TiWhere {
        self.peek().ti_where
    }

    pub fn last(&self) -> TiWhere {
        self.ti_tokens[self.curr - 1].ti_where
    }

    pub fn forward(&mut self) {
        self.curr += 1;
    }

    pub fn backward(&mut self) {
        self.curr -= 1;
    }

    pub fn is_eof(&self) -> bool {
        self.curr >= self.ti_tokens.len()
    }

    pub fn peek(&self) -> &TiToken {
        &self.ti_tokens[self.curr]
    }

    pub fn next(&mut self) -> &TiToken {
        let curr = &self.ti_tokens[self.curr];
        self.curr += 1;
        curr
    }

    pub fn assert_next(&mut self, next: TiTokenType) -> bool {
        if !self.is_eof() && self.next().ti_token_type == next {
            true
        } else {
            self.backward();
            false
        }
    }

    pub fn assert_next_ident(&mut self) -> Option<Rc<String>> {
        if let TiTokenType::Ident(ident) = &self.next().ti_token_type {
            Some(ident.clone())
        } else {
            self.backward();
            None
        }
    }
}

impl Display for TiTokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for token in &self.ti_tokens {
            write!(f, "{:?} ", token.ti_token_type)?;
        }
        Ok(())
    }
}
