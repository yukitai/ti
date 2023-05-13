use std::rc::Rc;

use crate::{
    frontend::parser::ast::{ImplFnType, TiExpr, TiExprType as E, TiProg, TiStmt, TiStmtType},
    types::types::TiType,
};

use super::{emitter::TiEmit, stream::Stream};

macro_rules! expr_proc {
    ($self: expr, $a: expr, $b: expr, $c: expr, $d: ident $e: literal $f: ident) => {{
        if $a > $b {
            $c.write("(");
        }
        $self.emit_expr_proc($d, $c, $b);
        $c.write(" ");
        $c.write($e);
        $c.write(" ");
        $self.emit_expr_proc($f, $c, $b);
        if $a > $b {
            $c.write(")");
        }
    }};
}

pub struct JSEmitter {
    ident: usize,
    ident_str: String,
}

impl JSEmitter {
    pub fn new(ident_str: &str) -> Self {
        Self {
            ident: 0,
            ident_str: ident_str.to_string(),
        }
    }

    fn begin(&mut self) {
        self.ident += 1;
    }

    fn close(&mut self) {
        self.ident = self.ident.checked_sub(1).unwrap();
    }

    fn emit_ident(&self, stream: &mut Stream) {
        for _ in 0..self.ident {
            stream.write(&self.ident_str);
        }
    }

    fn emit_expr_proc(&mut self, expr: &TiExpr, stream: &mut Stream, proc: u8) {
        match &expr.ti_expr {
            E::Assign(lhs, rhs) => expr_proc!(self, proc, 0x10, stream, lhs "=" rhs),

            E::Or(lhs, rhs) => expr_proc!(self, proc, 0x11, stream, lhs "||" rhs),

            E::And(lhs, rhs) => expr_proc!(self, proc, 0x12, stream, lhs "&&" rhs),

            E::Neq(lhs, rhs) => expr_proc!(self, proc, 0x13, stream, lhs "!==" rhs),
            E::Eq(lhs, rhs) => expr_proc!(self, proc, 0x13, stream, lhs "===" rhs),

            E::Grt(lhs, rhs) => expr_proc!(self, proc, 0x14, stream, lhs ">" rhs),
            E::Geq(lhs, rhs) => expr_proc!(self, proc, 0x14, stream, lhs ">=" rhs),
            E::Les(lhs, rhs) => expr_proc!(self, proc, 0x14, stream, lhs "<" rhs),
            E::Leq(lhs, rhs) => expr_proc!(self, proc, 0x14, stream, lhs "<=" rhs),

            E::Add(lhs, rhs) => expr_proc!(self, proc, 0x15, stream, lhs "+" rhs),
            E::Sub(lhs, rhs) => expr_proc!(self, proc, 0x15, stream, lhs "-" rhs),

            E::Mul(lhs, rhs) => expr_proc!(self, proc, 0x16, stream, lhs "*" rhs),
            E::Div(lhs, rhs) => expr_proc!(self, proc, 0x16, stream, lhs "/" rhs),

            E::Var(n) => {
                stream.write("ti_");
                stream.write(n);
            }
            E::LlNum(x) => {
                stream.write(&x.to_string());
            }
            E::LlStr(s) => {
                stream.write(&format!("{:?}", s));
            }
            E::LlBool(b) => {
                if *b {
                    stream.write("true");
                } else {
                    stream.write("false");
                }
            }
            E::LlNil => stream.write("null"),

            E::Block(block) => {
                self.emit_block(block, stream);
            }

            E::IfElse(cond, tcase, fcase) => match fcase {
                Some(vfcase) => {
                    stream.write("(");
                    self.emit_expr(cond, stream);
                    stream.write(" ? ");
                    self.emit_block(tcase, stream);
                    stream.write(" : ");
                    self.emit_block(vfcase, stream);
                    stream.write(")");
                }
                None => {
                    stream.write("if (");
                    self.emit_expr(cond, stream);
                    stream.write(") {\r\n");
                    self.begin();
                    self.emit_block_internal(tcase, stream, false);
                    self.close();
                    self.emit_ident(stream);
                    stream.write("}");
                }
            },

            E::Fn(fname, fargs, fret_d, fbody) => {
                stream.write("function ti_");
                stream.write(fname);
                stream.write("(");
                if fargs.len() > 0 {
                    stream.write("ti_");
                    stream.write(&fargs[0].0);
                    stream.write(" /* : ");
                    self.emit_type(&fargs[0].1, stream);
                    stream.write("*/");
                    for (an, at) in &fargs[1..] {
                        stream.write(", ti_");
                        stream.write(an);
                        stream.write(" /* : ");
                        self.emit_type(at, stream);
                        stream.write(" */");
                    }
                }
                stream.write(") /* : ");
                self.emit_type(fret_d, stream);
                stream.write(" */ {\r\n");
                self.emit_block_internal(fbody, stream, true);
                self.emit_ident(stream);
                stream.write("}");
            }

            E::Member(obj, field) => {
                self.emit_expr(obj, stream);
                stream.write(".");
                self.emit_expr(field, stream);
            }
            E::Lookup(obj, field) => {
                self.emit_expr(obj, stream);
                stream.write("[");
                self.emit_expr(field, stream);
                stream.write("]");
            }
            E::Field(cls, field) => {
                stream.write("__");
                self.emit_expr(cls, stream);
                stream.write(".prototype.");
                self.emit_expr(field, stream);
            }

            E::Call(callable, fargs) => {
                self.emit_expr(callable, stream);
                stream.write("(");
                match &callable.ti_expr {
                    E::Member(lhs, _) => {
                        self.emit_expr(lhs, stream);
                        for arg in fargs {
                            stream.write(", ");
                            self.emit_expr(arg, stream);
                        }
                    }
                    _ => {
                        if fargs.len() > 0 {
                            self.emit_expr(&fargs[0], stream);
                            for arg in &fargs[1..] {
                                stream.write(", ");
                                self.emit_expr(arg, stream);
                            }
                        }
                    }
                }

                stream.write(")");
            }

            _ => {}
        }
    }

    fn emit_fn(&mut self, func: &ImplFnType, stream: &mut Stream) {
        let (fname, fargs, fret_d, fbody) = func;
        stream.write("function ti_");
        stream.write(fname);
        stream.write("(");
        if fargs.len() > 0 {
            stream.write("ti_");
            stream.write(&fargs[0].0);
            stream.write(" /* : ");
            self.emit_type(&fargs[0].1, stream);
            stream.write("*/");
            for (an, at) in &fargs[1..] {
                stream.write(", ti_");
                stream.write(an);
                stream.write(" /* : ");
                self.emit_type(at, stream);
                stream.write(" */");
            }
        }
        stream.write(") /* : ");
        self.emit_type(fret_d, stream);
        stream.write(" */ {\r\n");
        self.emit_block_internal(fbody, stream, true);
        self.emit_ident(stream);
        stream.write("}");
    }

    fn emit_expr(&mut self, expr: &TiExpr, stream: &mut Stream) {
        self.emit_expr_proc(expr, stream, 0);
    }

    fn emit_stmt(&mut self, stmt: &TiStmt, stream: &mut Stream, ret: bool) {
        self.emit_ident(stream);
        match &stmt.ti_stmt {
            TiStmtType::Empty => {}
            TiStmtType::Let(pattern, value) => {
                stream.write("let ti_");
                stream.write(pattern);
                if let Some(vvalue) = value {
                    stream.write(" = ");
                    self.emit_expr(vvalue, stream);
                }
                stream.write(";\r\n")
            }
            TiStmtType::Expr(ti_expr) => {
                if ret {
                    stream.write("return ");
                }
                self.emit_expr(ti_expr, stream);
                stream.write(";\r\n")
            }
            TiStmtType::Break => stream.write("break;\r\n"),
            TiStmtType::Continue => stream.write("continue;\r\n"),
            TiStmtType::Return(value) => {
                stream.write("return");
                if let Some(vv) = value {
                    stream.write(" ");
                    self.emit_expr(vv, stream);
                }
                stream.write(";\r\n");
            }
            TiStmtType::TypeDecl(name, fields) => {
                stream.write("function __ti_");
                stream.write(name);
                stream.write("(");
                if fields.len() > 0 {
                    stream.write("ti_");
                    stream.write(&fields[0].0);
                    for (field, _) in fields[1..].iter() {
                        stream.write(", ti_");
                        stream.write(field);
                    }
                }
                stream.write(") {\r\n");
                self.begin();
                for (field, _) in fields {
                    self.emit_ident(stream);
                    stream.write("this.ti_");
                    stream.write(field);
                    stream.write(" = ti_");
                    stream.write(field);
                    stream.write(";\r\n");
                }
                self.close();
                self.emit_ident(stream);
                stream.write("}\r\n");
                self.emit_ident(stream);
                stream.write("function ti_");
                stream.write(name);
                stream.write("(");
                if fields.len() > 0 {
                    stream.write("ti_");
                    stream.write(&fields[0].0);
                    stream.write(" /* : ");
                    self.emit_type(&fields[0].1, stream);
                    stream.write("*/");
                    for (field, at) in fields[1..].iter() {
                        stream.write(", ti_");
                        stream.write(field);
                        stream.write(" /* : ");
                        self.emit_type(at, stream);
                        stream.write(" */");
                    }
                }
                stream.write(") {\r\n");
                self.begin();
                self.emit_ident(stream);
                stream.write("return new __ti_");
                stream.write(name);
                stream.write("(");
                if fields.len() > 0 {
                    stream.write("ti_");
                    stream.write(&fields[0].0);
                    for (field, _) in fields[1..].iter() {
                        stream.write(", ti_");
                        stream.write(field);
                    }
                }
                stream.write(");\r\n");
                self.close();
                self.emit_ident(stream);
                stream.write("}\r\n");
            }
            TiStmtType::Impl(name, funcs) => {
                for func in funcs {
                    self.emit_single_impl(name, func, stream);
                }
            }
        }
    }

    fn emit_single_impl(&mut self, name: &Rc<String>, func: &ImplFnType, stream: &mut Stream) {
        stream.write("__ti_");
        stream.write(name);
        stream.write(".prototype.ti_");
        stream.write(&func.0);
        stream.write(" = ");
        self.emit_fn(func, stream);
        stream.write(";\r\n");
    }

    fn emit_block_internal(&mut self, block: &Vec<TiStmt>, stream: &mut Stream, ret: bool) {
        let last = block.last();
        if let Some(vlast) = last {
            self.begin();
            for stmt in block[0..(block.len() - 1)].iter() {
                self.emit_stmt(stmt, stream, false);
            }
            self.emit_stmt(vlast, stream, ret);
            self.close();
        }
    }

    fn emit_block(&mut self, block: &Vec<TiStmt>, stream: &mut Stream) {
        stream.write("(() => {\r\n");
        self.emit_block_internal(block, stream, true);
        self.emit_ident(stream);
        stream.write("})()");
    }

    fn emit_type(&mut self, ti_type: &TiType, stream: &mut Stream) {
        match ti_type {
            TiType::Num => stream.write("number"),
            TiType::Str => stream.write("string"),
            TiType::Bool => stream.write("boolean"),
            TiType::Map(k, v) => {
                stream.write("{ [");
                self.emit_type(k, stream);
                stream.write("]: ");
                self.emit_type(v, stream);
                stream.write(" }");
            }
            TiType::List(t) => {
                self.emit_type(t, stream);
                stream.write("[]");
            }
            TiType::Mixed(tl) => {
                stream.write("{\r\n");
                self.begin();
                for (tn, tt) in tl.iter() {
                    self.emit_ident(stream);
                    stream.write(tn);
                    stream.write(": ");
                    self.emit_type(tt, stream);
                    stream.write(",\r\n");
                }
                self.close();
                self.emit_ident(stream);
                stream.write("}");
            }
            TiType::TVar(_sym, _) => {
                stream.write("unknown");
            }
        }
    }
}

impl TiEmit for JSEmitter {
    fn emit(&mut self, ast: &TiProg, stream: &mut Stream) {
        self.emit_block(&ast.ti_children, stream);
    }
}
