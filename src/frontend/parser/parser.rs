use std::{collections::HashMap, rc::Rc};

use crate::{
    error::{
        error::{TiError, TiErrorType, TiSyntaxError},
        reporter::TiReporter,
    },
    frontend::lexer::token::{TiToken, TiTokenStream, TiTokenType::*},
    types::types::TiType,
};

use super::ast::{TiExprType as E, *};

pub struct TiParser<'a> {
    pub types: HashMap<Rc<String>, Rc<Vec<(Rc<String>, Rc<TiType>)>>>,
    ti_tokens: TiTokenStream,
    ti_source: &'a str,
    pub ti_ast: TiProg,
}

impl<'a> TiParser<'a> {
    pub fn new(ti_tokens: TiTokenStream, ti_filename: &str, ti_source: &'a str) -> Self {
        Self {
            types: HashMap::new(),
            ti_tokens,
            ti_source,
            ti_ast: TiProg::new(ti_filename.to_string()),
        }
    }
}

const REPORT_BOUNDS_FLAG: bool = false;

macro_rules! report_bounds {
    ($self: expr, $ti_reporter: expr, $stmt: expr) => {
        if REPORT_BOUNDS_FLAG {
            $ti_reporter.report(TiError::new(
                TiErrorType::Note {
                    message: format!("bounds here"),
                },
                $stmt.ti_where,
                $self.ti_source,
            ));
        }
    };
}

impl<'a> TiParser<'a> {
    pub fn parse(&mut self, ti_reporter: &mut TiReporter<'a>) {
        while !self.ti_tokens.is_eof() {
            if let Some(stmt) = self.parse_stmt(ti_reporter) {
                report_bounds!(self, ti_reporter, stmt);
                self.ti_ast.add(stmt);
            }
        }
    }

    pub fn parse_block(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<Vec<TiStmt>> {
        if !self.ti_tokens.assert_next(LBra) {
            return None;
        }
        let mut block = Vec::new();
        while !self.ti_tokens.is_eof() {
            if self.ti_tokens.assert_next(RBra) {
                return Some(block);
            }
            if let Some(stmt) = self.parse_stmt(ti_reporter) {
                report_bounds!(self, ti_reporter, stmt);
                block.push(stmt);
            }
        }
        None
    }

    fn parse_stmt(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiStmt> {
        match self.ti_tokens.peek().ti_token_type {
            Semi => Some(TiStmt::new(
                self.ti_tokens.next().ti_where,
                TiStmtType::Empty,
            )),
            KLet => self.parse_stmt_let(ti_reporter),
            KBreak => Some(TiStmt::new(
                self.ti_tokens.next().ti_where,
                TiStmtType::Break,
            )),
            KContinue => Some(TiStmt::new(
                self.ti_tokens.next().ti_where,
                TiStmtType::Continue,
            )),
            KReturn => {
                let cw = self.ti_tokens.next().ti_where;
                let expr = if match self.ti_tokens.peek() {
                    TiToken {
                        ti_where: (l, _, _),
                        ..
                    } if l > &cw.0 => false,
                    TiToken {
                        ti_token_type: Semi,
                        ..
                    } => false,
                    _ => true,
                } {
                    self.parse_expr(ti_reporter)
                } else {
                    None
                };
                Some(TiStmt::new(cw, TiStmtType::Return(expr)))
            }
            KType => self.parse_stmt_type(ti_reporter),
            KImpl => self.parse_stmt_impl(ti_reporter),
            _ => {
                let expr = self.parse_expr(ti_reporter);
                expr.and_then(|expr| Some(TiStmt::new(expr.ti_where, TiStmtType::Expr(expr))))
            }
        }
    }

    fn parse_stmt_impl(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiStmt> {
        self.ti_tokens.mark();
        self.ti_tokens.forward();
        let impln = self.ti_tokens.assert_next_ident();
        let result = impln.and_then(|vimpln| {
            if self.ti_tokens.assert_next(KFor) {
                todo!()
            } else {
                if !self.ti_tokens.assert_next(LBra) {
                    ti_reporter.report(TiError::new(
                        TiErrorType::SyntaxError {
                            syntax_error: TiSyntaxError::ParserError(format!("")),
                        },
                        self.ti_tokens.here(),
                        self.ti_source,
                    ));
                    return None;
                }
                let mut impls = Vec::new();
                while !self.ti_tokens.is_eof() && !self.ti_tokens.assert_next(RBra) {
                    if self.ti_tokens.assert_next(KFn) {
                        if let Some(func) = self._parse_fn(ti_reporter) {
                            impls.push(func);
                        } else {
                            break;
                        }
                    }
                }
                Some(TiStmt::new(
                    self.ti_tokens.range(),
                    TiStmtType::Impl(vimpln, impls),
                ))
            }
        });
        self.ti_tokens.unmark();
        result
    }

    fn parse_stmt_type(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiStmt> {
        self.ti_tokens.mark();
        self.ti_tokens.forward();
        let tn = self.ti_tokens.assert_next_ident();
        let result = tn.and_then(|vtn| {
            if self.ti_tokens.assert_next(LBrace) {
                let mut fields = Vec::new();
                if !self.ti_tokens.assert_next(RBrace) {
                    loop {
                        if self.ti_tokens.is_eof() {
                            ti_reporter.report(TiError::new(
                                TiErrorType::SyntaxError {
                                    syntax_error: TiSyntaxError::ParserError(format!(
                                        "mismatched closing tag, expected RBrace, found flag eof, while parsing a function",
                                    )),
                                },
                                self.ti_tokens.last(),
                                self.ti_source,
                            ));
                            return None;
                        }
                        if let Some(aname) = self.ti_tokens.assert_next_ident() {
                            let ad = if self.ti_tokens.assert_next(Colon) {
                                if let Some(t) = self.parse_type(ti_reporter) {
                                    t
                                } else {
                                    return None
                                }
                            } else {
                                TiType::tvar()
                            };
                            fields.push((aname, Rc::new(ad)));
                        } else {
                            ti_reporter.report(TiError::new(
                                TiErrorType::SyntaxError {
                                    syntax_error: TiSyntaxError::ParserError(format!(
                                        "expected Ident, found {:?}, while parsing a function",
                                        self.ti_tokens.peek().ti_token_type,
                                    )),
                                },
                                self.ti_tokens.here(),
                                self.ti_source,
                            ));
                            return None
                        }
                        if self.ti_tokens.assert_next(RBrace) {
                            break
                        }
                        if !self.ti_tokens.assert_next(Comma) {
                                ti_reporter.report(TiError::new(
                                TiErrorType::SyntaxError {
                                    syntax_error: TiSyntaxError::ParserError(format!(
                                        "expected Comma, found {:?}, while parsing a function",
                                        self.ti_tokens.peek().ti_token_type,
                                    )),
                                },
                                self.ti_tokens.here(),
                                self.ti_source,
                            ));
                        }
                    }
                }
                self.types.insert(vtn.clone(), Rc::new(fields.clone()));
                Some(TiStmt::new(self.ti_tokens.range(), TiStmtType::TypeDecl(vtn, fields)))
            } else {
                ti_reporter.report(TiError::new(
                    TiErrorType::SyntaxError {
                        syntax_error: TiSyntaxError::ParserError(format!("expected LBrace, found {:?}, while parsing a mixed type", self.ti_tokens.peek().ti_token_type)),
                    },
                    self.ti_tokens.here(),
                    self.ti_source,
                ));
                None
            }

        });
        self.ti_tokens.unmark();
        result
    }

    fn parse_stmt_let(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiStmt> {
        self.ti_tokens.mark();
        self.ti_tokens.forward();
        let vn = self.ti_tokens.assert_next_ident();
        let result = vn.and_then(|vvn| {
            if self.ti_tokens.assert_next(OAssign) {
                let vv = self.parse_expr(ti_reporter);
                vv.and_then(|vvv| {
                    Some(TiStmt::new(
                        self.ti_tokens.range(),
                        TiStmtType::Let(vvn, Some(vvv)),
                    ))
                })
            } else {
                Some(TiStmt::new(
                    self.ti_tokens.range(),
                    TiStmtType::Let(vvn, None),
                ))
            }
        });
        self.ti_tokens.unmark();
        result
    }

    fn parse_expr(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiExpr> {
        self.parse_expr_assignment(ti_reporter)
    }

    fn parse_expr_assignment(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiExpr> {
        self.ti_tokens.mark();
        let expr = self.parse_expr_or(ti_reporter);
        let result = expr.and_then(|vexpr| {
            if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(OAssign) {
                let rexpr = self.parse_expr_assignment(ti_reporter);
                rexpr.and_then(|vrexpr| {
                    Some(TiExpr::new(
                        self.ti_tokens.range(),
                        E::Assign(Box::new(vexpr), Box::new(vrexpr)),
                    ))
                })
            } else {
                Some(vexpr)
            }
        });
        self.ti_tokens.unmark();
        result
    }

    fn parse_expr_or(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiExpr> {
        self.ti_tokens.mark();
        let mut result = self.parse_expr_and(ti_reporter);
        loop {
            if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(OOr) {
                let rexpr = self.parse_expr_and(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::Or(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
            } else {
                break;
            }
        }
        self.ti_tokens.unmark();
        result
    }

    fn parse_expr_and(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiExpr> {
        self.ti_tokens.mark();
        let mut result = self.parse_expr_equal(ti_reporter);
        loop {
            if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(OAnd) {
                let rexpr = self.parse_expr_equal(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::And(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
            } else {
                break;
            }
        }
        self.ti_tokens.unmark();
        result
    }

    fn parse_expr_equal(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiExpr> {
        self.ti_tokens.mark();
        let mut result = self.parse_expr_compare(ti_reporter);
        loop {
            if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(ONeq) {
                let rexpr = self.parse_expr_compare(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::Neq(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
            } else if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(OEq) {
                let rexpr = self.parse_expr_compare(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::Eq(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
            } else {
                break;
            }
        }
        self.ti_tokens.unmark();
        result
    }

    fn parse_expr_compare(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiExpr> {
        self.ti_tokens.mark();
        let mut result = self.parse_expr_add(ti_reporter);
        loop {
            if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(OGeq) {
                let rexpr = self.parse_expr_add(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::Geq(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
            } else if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(OGrt) {
                let rexpr = self.parse_expr_add(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::Grt(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
            } else if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(OLeq) {
                let rexpr = self.parse_expr_add(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::Leq(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
            } else if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(OLes) {
                let rexpr = self.parse_expr_add(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::Les(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
            } else {
                break;
            }
        }
        self.ti_tokens.unmark();
        result
    }

    fn parse_expr_add(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiExpr> {
        self.ti_tokens.mark();
        let mut result = self.parse_expr_mul(ti_reporter);
        loop {
            if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(OSub) {
                let rexpr = self.parse_expr_mul(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::Sub(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
            } else if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(OAdd) {
                let rexpr = self.parse_expr_mul(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::Add(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
            } else {
                break;
            }
        }
        self.ti_tokens.unmark();
        result
    }

    fn parse_expr_mul(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiExpr> {
        self.ti_tokens.mark();
        let mut result = self.parse_expr_unary(ti_reporter);
        loop {
            if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(ODiv) {
                let rexpr = self.parse_expr_unary(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::Div(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
            } else if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(OMul) {
                let rexpr = self.parse_expr_unary(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::Mul(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
            } else {
                break;
            }
        }
        self.ti_tokens.unmark();
        result
    }

    fn parse_expr_unary(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiExpr> {
        self.ti_tokens.mark();
        let mut result = self.parse_expr_call(ti_reporter);
        loop {
            if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(OMember) {
                let rexpr = self.parse_expr_call(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::Member(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
            } else if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(OField) {
                let rexpr = self.parse_expr_call(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::Field(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
            } else if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(LBracket) {
                let rexpr = self.parse_expr(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::Lookup(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
                if self.ti_tokens.is_eof() {
                    ti_reporter.report(TiError::new(
                        TiErrorType::SyntaxError { 
                            syntax_error: TiSyntaxError::ParserError(
                                format!("mismatched closing tag, expected RBracket, found flag eof, while parsing an expression")
                            )
                        },
                        self.ti_tokens.here(),
                        self.ti_source,
                    ));
                }
                if !self.ti_tokens.assert_next(RBracket) {
                    ti_reporter.report(TiError::new(
                        TiErrorType::SyntaxError { 
                            syntax_error: TiSyntaxError::ParserError(
                                format!("mismatched close tag, expected token RBracket, found {:?}, while parsing an expression",
                                    self.ti_tokens.peek().ti_token_type
                                )
                            )
                        },
                        self.ti_tokens.here(),
                        self.ti_source,
                    ));
                    return None;
                }
            } else {
                break;
            }
        }
        self.ti_tokens.unmark();
        result
    }

    fn parse_expr_member(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiExpr> {
        self.ti_tokens.mark();
        let mut result = self.parse_expr_binary(ti_reporter);
        loop {
            if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(OMember) {
                let rexpr = self.parse_expr_binary(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::Member(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
            } else if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(OField) {
                let rexpr = self.parse_expr_binary(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::Field(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
            } else if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(LBracket) {
                let rexpr = self.parse_expr(ti_reporter);
                result = rexpr.and_then(|vrexpr| {
                    result.and_then(|vexpr| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::Lookup(Box::new(vexpr), Box::new(vrexpr)),
                        ))
                    })
                });
                if self.ti_tokens.is_eof() {
                    ti_reporter.report(TiError::new(
                        TiErrorType::SyntaxError { 
                            syntax_error: TiSyntaxError::ParserError(
                                format!("mismatched closing tag, expected RBracket, found flag eof, while parsing an expression")
                            )
                        },
                        self.ti_tokens.here(),
                        self.ti_source,
                    ));
                }
                if !self.ti_tokens.assert_next(RBracket) {
                    ti_reporter.report(TiError::new(
                        TiErrorType::SyntaxError { 
                            syntax_error: TiSyntaxError::ParserError(
                                format!("mismatched close tag, expected token RBracket, found {:?}, while parsing an expression",
                                    self.ti_tokens.peek().ti_token_type
                                )
                            )
                        },
                        self.ti_tokens.here(),
                        self.ti_source,
                    ));
                    return None;
                }
            } else {
                break;
            }
        }
        self.ti_tokens.unmark();
        result
    }

    fn parse_expr_call(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiExpr> {
        self.ti_tokens.mark();
        let mut result = self.parse_expr_member(ti_reporter);
        loop {
            if !self.ti_tokens.is_eof() && self.ti_tokens.assert_next(LBrace) {
                let mut fargs = Vec::new();
                if !self.ti_tokens.assert_next(RBrace) {
                    loop {
                        let arg = self.parse_expr(ti_reporter);
                        if let Some(varg) = arg {
                            fargs.push(varg);
                        }
                        if self.ti_tokens.assert_next(RBrace) {
                            break;
                        }
                        if !self.ti_tokens.assert_next(Comma) {
                            ti_reporter.report(TiError::new(
                                TiErrorType::SyntaxError {
                                    syntax_error: TiSyntaxError::ParserError(format!(
                                        "expected Comma, found {:?}, while parsing a function call",
                                        self.ti_tokens.peek().ti_token_type,
                                    )),
                                },
                                self.ti_tokens.here(),
                                self.ti_source,
                            ));
                            return None;
                        }
                    }
                }
                result = result.and_then(|vexpr| {
                    Some(TiExpr::new(
                        self.ti_tokens.range(),
                        E::Call(Box::new(vexpr), fargs),
                    ))
                });
            } else {
                break;
            }
        }
        self.ti_tokens.unmark();
        result
    }

    fn parse_expr_binary(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiExpr> {
        let curr = &self.ti_tokens.next().ti_token_type;
        match curr {
            Ident(ident) => {
                let ident = ident.clone();
                Some(TiExpr::new(self.ti_tokens.last(), E::Var(ident)))
            }
            LlNum(num) => {
                let num = *num;
                Some(TiExpr::new(self.ti_tokens.last(), E::LlNum(num)))
            }
            LlStr(t_str) => {
                let t_str = t_str.clone();
                Some(TiExpr::new(self.ti_tokens.last(), E::LlStr(t_str)))
            }
            LlNil => Some(TiExpr::new(self.ti_tokens.last(), E::LlNil)),
            LlBool(t_bool) => {
                let t_bool = t_bool.clone();
                Some(TiExpr::new(self.ti_tokens.last(), E::LlBool(t_bool)))
            }
            LBrace => {
                let expr = self.parse_expr(ti_reporter);
                if expr.is_none() {
                    return None;
                }
                if self.ti_tokens.assert_next(RBrace) {
                    expr
                } else {
                    ti_reporter.report(TiError::new(
                        TiErrorType::SyntaxError {
                            syntax_error: TiSyntaxError::ParserError(format!(
                                "mismatched closing tag, expected RBrace, found {:?}",
                                self.ti_tokens.peek().ti_token_type
                            )),
                        },
                        self.ti_tokens.peek().ti_where,
                        self.ti_source,
                    ));
                    None
                }
            }
            LBra => {
                self.ti_tokens.backward();
                self.ti_tokens.mark();
                self.parse_block(ti_reporter).and_then(|block| {
                    let ti_where = self.ti_tokens.range();
                    self.ti_tokens.unmark();
                    Some(TiExpr::new(ti_where, E::Block(block)))
                })
            }
            KIf => {
                self.ti_tokens.mark_last();
                let cond = self.parse_expr(ti_reporter);
                let result = cond.and_then(|vcond| {
                    let tcase = self.parse_block(ti_reporter);
                    tcase.and_then(|vtcase| {
                        if self.ti_tokens.assert_next(KElse) {
                            let fcase = self.parse_block(ti_reporter);
                            fcase.and_then(|vfcase| {
                                Some(TiExpr::new(
                                    self.ti_tokens.range(),
                                    E::IfElse(Box::new(vcond), vtcase, Some(vfcase)),
                                ))
                            })
                        } else {
                            Some(TiExpr::new(
                                self.ti_tokens.range(),
                                E::IfElse(Box::new(vcond), vtcase, None),
                            ))
                        }
                    })
                });
                self.ti_tokens.unmark();
                result
            }
            KWhile => {
                self.ti_tokens.mark_last();
                let cond = self.parse_expr(ti_reporter);
                let result = cond.and_then(|vcond| {
                    let body = self.parse_block(ti_reporter);
                    body.and_then(|vbody| {
                        Some(TiExpr::new(
                            self.ti_tokens.range(),
                            E::While(Box::new(vcond), vbody),
                        ))
                    })
                });
                self.ti_tokens.unmark();
                result
            }
            KFn => {
                let func = self._parse_fn(ti_reporter);
                func.and_then(|vf| {
                    Some(TiExpr::new(
                        self.ti_tokens.range(),
                        E::Fn(vf.0, vf.1, vf.2, vf.3),
                    ))
                })
            }
            _ => {
                ti_reporter.report(TiError::new(
                    TiErrorType::SyntaxError {
                        syntax_error: TiSyntaxError::ParserError(format!(
                            "unexpected token {:?} while parsing an expression",
                            curr
                        )),
                    },
                    self.ti_tokens.last(),
                    self.ti_source,
                ));
                None
            }
        }
    }

    fn _parse_fn(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<ImplFnType> {
        self.ti_tokens.mark_last();
        let result = self.ti_tokens.assert_next_ident().and_then(|fname| {
                    if !self.ti_tokens.assert_next(LBrace) {
                        ti_reporter.report(TiError::new(
                            TiErrorType::SyntaxError {
                                syntax_error: TiSyntaxError::ParserError(format!(
                                    "expected token LBrace, found {:?}, while parsing a function",
                                    self.ti_tokens.peek().ti_token_type
                                )),
                            },
                            self.ti_tokens.peek().ti_where,
                            self.ti_source,
                        ));
                        return None;
                    }
                    let mut fargs = Vec::new();
                    if !self.ti_tokens.assert_next(RBrace) {loop {
                        if self.ti_tokens.is_eof() {
                            ti_reporter.report(TiError::new(
                                TiErrorType::SyntaxError {
                                    syntax_error: TiSyntaxError::ParserError(format!(
                                        "mismatched closing tag, expected RBrace, found flag eof, while parsing a function",
                                    )),
                                },
                                self.ti_tokens.last(),
                                self.ti_source,
                            ));
                            return None;
                        }
                        if let Some(aname) = self.ti_tokens.assert_next_ident() {
                            let ad = if self.ti_tokens.assert_next(Colon) {
                                if let Some(t) = self.parse_type(ti_reporter) {
                                    t
                                } else {
                                    return None
                                }
                            } else {
                                TiType::tvar()
                            };
                            fargs.push((aname, Rc::new(ad)));
                        } else {
                            ti_reporter.report(TiError::new(
                                TiErrorType::SyntaxError {
                                    syntax_error: TiSyntaxError::ParserError(format!(
                                        "expected Ident, found {:?}, while parsing a function",
                                        self.ti_tokens.peek().ti_token_type,
                                    )),
                                },
                                self.ti_tokens.here(),
                                self.ti_source,
                            ));
                            return None
                        }
                        if self.ti_tokens.assert_next(RBrace) {
                            break
                        }
                        if !self.ti_tokens.assert_next(Comma) {
                                ti_reporter.report(TiError::new(
                                TiErrorType::SyntaxError {
                                    syntax_error: TiSyntaxError::ParserError(format!(
                                        "expected Comma, found {:?}, while parsing a function",
                                        self.ti_tokens.peek().ti_token_type,
                                    )),
                                },
                                self.ti_tokens.here(),
                                self.ti_source,
                            ));
                            }
                    }}
                    let fretd = Rc::new(if self.ti_tokens.assert_next(ThinArrow) {
                        if let Some(t) = self.parse_type(ti_reporter) {
                                    t
                                } else {
                                    return None
                                }
                    } else {
                        TiType::tvar()
                    });
                    let fbody = if self.ti_tokens.assert_next(FatArrow) {
                        if let Some(vexpr) = self.parse_expr(ti_reporter) {
                            vec![TiStmt::new(vexpr.ti_where, TiStmtType::Expr(vexpr))]
                        } else {
                            return None;
                        }
                    } else {
                        if let Some(vbody) = self.parse_block(ti_reporter) {
                            vbody
                        } else {
                            return None;
                        }
                    };
                    Some((fname, fargs, fretd, fbody))
                });
        self.ti_tokens.unmark();
        result
    }

    fn parse_type(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiType> {
        self.parse_type_primary(ti_reporter)
    }

    fn parse_type_primary(&mut self, ti_reporter: &mut TiReporter<'a>) -> Option<TiType> {
        let curr = self.ti_tokens.next();
        match &curr.ti_token_type {
            TNum => Some(TiType::Num),
            TStr => Some(TiType::Str),
            TBool => Some(TiType::Bool),
            Ident(tname) => {
                if !self.types.contains_key(tname) {
                    ti_reporter.report(TiError::new(
                        TiErrorType::SyntaxError {
                            syntax_error: TiSyntaxError::ParserError(format!(
                                "name `{:?}` is not a type or a trait",
                                tname
                            )),
                        },
                        curr.ti_where,
                        self.ti_source,
                    ));
                    return None;
                }
                Some(TiType::Mixed(self.types.get(tname).unwrap().clone()))
            }
            LBrace => None,
            LBracket => {
                let t = self.parse_type(ti_reporter);
                t.and_then(|vt| {
                    if self.ti_tokens.assert_next(RBracket) {
                        Some(TiType::List(Box::new(vt)))
                    } else {
                        None
                    }
                })
            }
            LBra => {
                let kt = self.parse_type(ti_reporter);
                kt.and_then(|vkt| {
                    if self.ti_tokens.assert_next(Semi) {
                        let vt = self.parse_type(ti_reporter);
                        vt.and_then(|vvt| {
                            if self.ti_tokens.assert_next(RBra) {
                                Some(TiType::Map(Box::new(vkt), Box::new(vvt)))
                            } else {
                                None
                            }
                        })
                    } else {
                        None
                    }
                })
            }
            _ => {
                ti_reporter.report(TiError::new(
                    TiErrorType::SyntaxError {
                        syntax_error: TiSyntaxError::ParserError(format!(
                            "unexpected token {:?} while parsing a type",
                            curr.ti_token_type
                        )),
                    },
                    curr.ti_where,
                    self.ti_source,
                ));
                None
            }
        }
    }
}
