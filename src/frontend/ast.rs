// Helper structs for parsing tree

use crate::types::Type;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, PartialEq)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, PartialEq)]
pub enum Item {
    Function(FunctionDef),
    VarDecl(VarDecl),
}

#[derive(Debug, PartialEq)]
pub struct VarDecl {
    pub name: String,
    pub _type: Rc<RefCell<Type>>,
    pub value: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Param {
    pub name: String,
    pub _type: Rc<RefCell<Type>>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionDef {
    pub name: String,
    pub params: Vec<Param>,
    pub body: Vec<Stmt>,
    pub return_type: Rc<RefCell<Type>>,
}

impl FunctionDef {
    pub fn get_type(&self) -> Rc<RefCell<Type>> {
        let args_ty = self
            .params
            .iter()
            .map(|param| param._type.clone())
            .collect();
        Rc::new(RefCell::new(Type::Function(
            args_ty,
            self.return_type.clone(),
        )))
    }
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    VarDecl(VarDecl),
    Expr(Box<Expr>),
    Return(Box<Expr>),
    IfElse(Box<Expr>, Vec<Stmt>, Vec<Stmt>),
    While(Box<Expr>, Vec<Stmt>),
}

#[derive(Debug, PartialEq)]
pub enum OpCode {
    Mul,
    Div,
    Add,
    Sub,
    Equal,
    NotEqual,
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Call(String, Vec<Box<Expr>>),
    Binary(Box<Expr>, OpCode, Box<Expr>),
    Atom(Box<Atom>),
    Var(String),
}

#[derive(Debug, PartialEq)]
pub enum Atom {
    I32(i32),
    Bool(bool),
}
