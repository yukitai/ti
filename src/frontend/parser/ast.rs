use crate::error::error::TiWhere;

use std::rc::Rc;

#[derive(Debug)]
pub struct TiProg {
    pub ti_children: Vec<TiStmt>,
    pub ti_filename: String,
}

impl TiProg {
    pub fn new(ti_filename: String) -> Self {
        Self {
            ti_children: Vec::new(),
            ti_filename,
        }
    }

    pub fn add(&mut self, child: TiStmt) {
        self.ti_children.push(child);
    }
}

#[derive(Debug)]
pub struct TiStmt {
    pub ti_stmt: TiStmtType,
    pub ti_where: TiWhere,
}

impl TiStmt {
    pub fn new(ti_where: TiWhere, ti_stmt: TiStmtType) -> Self {
        Self { ti_stmt, ti_where }
    }
}

#[derive(Debug)]
pub enum TiStmtType {
    Let(Rc<String>, Option<TiExpr>),
    Break,
    Continue,
    Return(Option<TiExpr>),
    Expr(TiExpr),
}

#[derive(Debug)]
pub struct TiExpr {
    pub ti_expr: TiExprType,
    pub ti_where: TiWhere,
}

impl TiExpr {
    pub fn new(ti_where: TiWhere, ti_expr: TiExprType) -> Self {
        Self { ti_expr, ti_where }
    }
}

type TiDescri = Rc<String>;

#[derive(Debug)]
pub enum TiExprType {
    Var(Rc<String>),
    LlNum(f64),
    LlStr(Rc<String>),
    LlList(Vec<TiExpr>),
    LlMap(Vec<(TiExpr, TiExpr)>),

    Member(Box<TiExpr>, Box<TiExpr>),
    Neg(Box<TiExpr>),
    Not(Box<TiExpr>),

    Add(Box<TiExpr>, Box<TiExpr>),
    Sub(Box<TiExpr>, Box<TiExpr>),
    Mul(Box<TiExpr>, Box<TiExpr>),
    Div(Box<TiExpr>, Box<TiExpr>),
    Mod(Box<TiExpr>, Box<TiExpr>),
    BitXor(Box<TiExpr>, Box<TiExpr>),
    BitOr(Box<TiExpr>, Box<TiExpr>),
    BitAnd(Box<TiExpr>, Box<TiExpr>),
    And(Box<TiExpr>, Box<TiExpr>),
    Or(Box<TiExpr>, Box<TiExpr>),
    Xor(Box<TiExpr>, Box<TiExpr>),
    Eq(Box<TiExpr>, Box<TiExpr>),
    Neq(Box<TiExpr>, Box<TiExpr>),
    Grt(Box<TiExpr>, Box<TiExpr>),
    Les(Box<TiExpr>, Box<TiExpr>),
    Geq(Box<TiExpr>, Box<TiExpr>),
    Leq(Box<TiExpr>, Box<TiExpr>),

    Call(Box<TiExpr>, Vec<TiExpr>),

    Assign(Box<TiExpr>, Box<TiExpr>),

    IfElse(Box<TiExpr>, Vec<TiStmt>, Option<Vec<TiStmt>>),

    While(Box<TiExpr>, Vec<TiStmt>),

    Block(Vec<TiStmt>),

    Fn(
        Rc<String>,
        Vec<(Rc<String>, TiDescri)>,
        TiDescri,
        Vec<TiStmt>,
    ),
}
