use lazy_static::lazy_static;
use std::cell::RefCell;
use std::collections::HashSet;
use std::rc::Rc;
use std::sync::Mutex;

pub type TVarID = i32;

pub type VarSet = HashSet<TVarID>;

#[derive(Debug, PartialEq)]
pub enum TVar {
    Resolved(Rc<RefCell<Type>>),
    Unresolved(TVarID),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    ForAll(VarSet, Rc<RefCell<Type>>),
    Var(Rc<RefCell<TVar>>),
    Con(String),
    Function(Vec<Rc<RefCell<Type>>>, Rc<RefCell<Type>>),
}

lazy_static! {
    static ref COUNTER: Mutex<i32> = Mutex::new(0);
}

pub fn unwrap_resolved(ty: Rc<RefCell<Type>>) -> Rc<RefCell<Type>> {
    let mut result = ty;

    while let Type::Var(tvar) = &*result.clone().borrow() {
        match &*tvar.borrow() {
            TVar::Resolved(inner) => result = Rc::clone(&inner),
            _ => break,
        }
    }

    result
}

impl Type {
    pub fn unknown() -> Self {
        let mut counter = COUNTER.lock().unwrap();
        *counter += 1;
        Type::Var(Rc::new(RefCell::new(TVar::Unresolved(*counter))))
    }
}
