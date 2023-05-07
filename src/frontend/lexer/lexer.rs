use std::rc::Rc;

use crate::error::{
    error::{TiError, TiErrorType::*, TiSyntaxError::LexerError, TiWhere},
    reporter::TiReporter,
};

use super::token::{TiToken, TiTokenStream, TiTokenType::*};

pub struct TiLexer<'a> {
    ti_source: &'a str,
    ti_bytes: &'a [u8],
    mark: usize,
    curr: usize,
    line: usize,
}

impl<'a> TiLexer<'a> {
    pub fn new(ti_source: &'a str) -> Self {
        Self {
            ti_source,
            ti_bytes: ti_source.as_bytes(),
            mark: 0,
            curr: 0,
            line: 1,
        }
    }

    fn here(&self) -> TiWhere {
        (self.line, self.curr - 1, self.curr)
    }

    fn range(&self) -> TiWhere {
        (self.line, self.mark, self.curr)
    }

    fn mark(&mut self) {
        self.mark = self.curr - 1;
    }

    fn forward(&mut self) {
        self.curr += 1;
    }

    fn backward(&mut self) {
        self.curr -= 1;
    }

    fn is_eof(&self) -> bool {
        self.curr >= self.ti_source.len()
    }

    fn peek(&self) -> u8 {
        self.ti_bytes[self.curr]
    }

    fn next(&mut self) -> u8 {
        let curr = self.ti_bytes[self.curr];
        self.curr += 1;
        curr
    }

    fn last(&self) -> u8 {
        self.ti_bytes[self.curr - 1]
    }

    pub fn tokenize(mut self, ti_reporter: &mut TiReporter<'a>) -> TiTokenStream {
        let mut tokens = Vec::new();
        while !self.is_eof() {
            let curr = self.next();
            match curr {
                b' ' | b'\t' => {}
                b'(' => tokens.push(TiToken::new(LBrace, self.here())),
                b')' => tokens.push(TiToken::new(RBrace, self.here())),
                b'[' => tokens.push(TiToken::new(LBracket, self.here())),
                b']' => tokens.push(TiToken::new(RBracket, self.here())),
                b'{' => tokens.push(TiToken::new(LBra, self.here())),
                b'}' => tokens.push(TiToken::new(RBra, self.here())),
                b':' => {
                    if self.peek() == b':' {
                        self.mark();
                        self.forward();
                        tokens.push(TiToken::new(OField, self.range()))
                    } else {
                        tokens.push(TiToken::new(Colon, self.here()))
                    }
                }
                b',' => tokens.push(TiToken::new(Comma, self.here())),
                b';' => tokens.push(TiToken::new(Semi, self.here())),
                b'.' => tokens.push(TiToken::new(OMember, self.here())),
                b'+' => {
                    if self.peek() == b'=' {
                        self.mark();
                        self.forward();
                        // tokens.push(TiToken::new(OEq, self.range()));
                    } else {
                        tokens.push(TiToken::new(OAdd, self.here()));
                    }
                }
                b'-' => {
                    if self.peek() == b'=' {
                        self.mark();
                        self.forward();
                        // tokens.push(TiToken::new(OEq, self.range()));
                    } else if self.peek() == b'>' {
                        self.mark();
                        self.forward();
                        tokens.push(TiToken::new(ThinArrow, self.range()));
                    } else {
                        tokens.push(TiToken::new(OSub, self.here()));
                    }
                }
                b'*' => {
                    if self.peek() == b'=' {
                        self.mark();
                        self.forward();
                        // tokens.push(TiToken::new(OEq, self.range()));
                    } else if self.peek() == b'*' {
                        self.mark();
                        self.forward();
                        tokens.push(TiToken::new(OPow, self.range()));
                    } else {
                        tokens.push(TiToken::new(OMul, self.here()));
                    }
                }
                b'/' => {
                    if self.peek() == b'=' {
                        self.mark();
                        self.forward();
                        // tokens.push(TiToken::new(OEq, self.range()));
                    } else {
                        tokens.push(TiToken::new(ODiv, self.here()));
                    }
                }
                b'^' => {
                    if self.peek() == b'=' {
                        self.mark();
                        self.forward();
                        // tokens.push(TiToken::new(OEq, self.range()));
                    } else {
                        tokens.push(TiToken::new(OBitXor, self.here()));
                    }
                }
                b'&' => {
                    if self.peek() == b'&' {
                        self.mark();
                        self.forward();
                        tokens.push(TiToken::new(OAnd, self.range()));
                    } else {
                        tokens.push(TiToken::new(OBitAnd, self.here()));
                    }
                }
                b'|' => {
                    if self.peek() == b'|' {
                        self.mark();
                        self.forward();
                        tokens.push(TiToken::new(OOr, self.range()));
                    } else {
                        tokens.push(TiToken::new(OBitOr, self.here()));
                    }
                }
                b'!' => {
                    if self.peek() == b'=' {
                        self.mark();
                        self.forward();
                        tokens.push(TiToken::new(ONeq, self.range()));
                    } else {
                        tokens.push(TiToken::new(ONot, self.here()));
                    }
                }
                b'>' => {
                    if self.peek() == b'=' {
                        self.mark();
                        self.forward();
                        tokens.push(TiToken::new(OGeq, self.range()));
                    } else {
                        tokens.push(TiToken::new(OGrt, self.here()));
                    }
                }
                b'<' => {
                    if self.peek() == b'=' {
                        self.mark();
                        self.forward();
                        tokens.push(TiToken::new(OLeq, self.range()));
                    } else {
                        tokens.push(TiToken::new(OLes, self.here()));
                    }
                }
                b'=' => {
                    if self.peek() == b'=' {
                        self.mark();
                        self.forward();
                        tokens.push(TiToken::new(OEq, self.range()));
                    } else if self.peek() == b'>' {
                        self.mark();
                        self.forward();
                        tokens.push(TiToken::new(FatArrow, self.range()));
                    } else {
                        tokens.push(TiToken::new(OAssign, self.here()));
                    }
                }
                b'\r' => {
                    self.forward();
                    self.line += 1;
                }
                b'\n' => {
                    self.line += 1;
                }
                b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'$' => {
                    self.mark();
                    while !self.is_eof()
                        && match self.next() {
                            b'a'..=b'z' | b'A'..=b'Z' | b'_' | b'$' | b'0'..=b'9' | b'\'' => true,
                            _ => false,
                        }
                    {}
                    self.backward();
                    let t_str = &self.ti_source[self.mark..self.curr];
                    let token = match t_str {
                        "let" => KLet,
                        "return" => KReturn,
                        "for" => KFor,
                        "in" => KIn,
                        "if" => KIf,
                        "else" => KElse,
                        "while" => KWhile,
                        "continue" => KContinue,
                        "break" => KBreak,
                        "type" => KType,
                        "fn" => KFn,
                        "impl" => KImpl,
                        "nil" => LlNil,
                        _ => Ident(Rc::new(t_str.to_string())),
                    };
                    tokens.push(TiToken::new(token, self.range()));
                }
                b'0'..=b'9' => {
                    self.mark();
                    while !self.is_eof()
                        && match self.next() {
                            b'0'..=b'9' | b'.' => true,
                            _ => false,
                        }
                    {}
                    self.backward();
                    let t_str = &self.ti_source[self.mark..self.curr];
                    if let Ok(num) = t_str.parse() {
                        let token = LlNum(num);
                        tokens.push(TiToken::new(token, self.range()));
                    } else {
                        ti_reporter.report(TiError::new(
                            SyntaxError {
                                syntax_error: LexerError(format!(
                                    "invaild num literal `{}`",
                                    t_str
                                )),
                            },
                            self.range(),
                            self.ti_source,
                        ));
                    }
                }
                b'\'' | b'"' => {
                    self.mark();
                    let mut cast = false;
                    let mut raw_str = Vec::new();
                    while !self.is_eof() {
                        if cast {
                            cast = false;
                            match self.next() {
                                b'r' => raw_str.push(b'\r'),
                                b'n' => raw_str.push(b'\n'),
                                b't' => raw_str.push(b'\t'),
                                b'u' => todo!(),
                                _ => ti_reporter.report(TiError::new(
                                    SyntaxError {
                                        syntax_error: LexerError(format!(
                                            "invaild special char `\\{}` while parsing a string literal",
                                            self.last(),
                                        )),
                                    },
                                    self.here(),
                                    self.ti_source,
                                )),
                            }
                        } else {
                            match self.next() {
                                b'\'' | b'"' => break,
                                b'\\' => cast = true,
                                _ => raw_str.push(self.last()),
                            };
                        }
                    }
                    if let Ok(t_str) = String::from_utf8(raw_str) {
                        tokens.push(TiToken::new(LlStr(Rc::new(t_str)), self.range()));
                    } else {
                        ti_reporter.report(TiError::new(
                            SyntaxError {
                                syntax_error: LexerError(format!(
                                    "invaild string literal (decoding by utf-8)"
                                )),
                            },
                            self.here(),
                            self.ti_source,
                        ));
                    }
                }
                _ => ti_reporter.report(TiError::new(
                    SyntaxError {
                        syntax_error: LexerError(format!("unexpected symbol `{}`", curr as char)),
                    },
                    self.here(),
                    self.ti_source,
                )),
            }
        }
        TiTokenStream::new(tokens)
    }
}
