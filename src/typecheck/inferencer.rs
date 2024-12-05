// REF:
// 1. https://github.com/glyh/wyah/blob/master/typecheck/type_inference.ml
//

use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;

use crate::frontend::ast;
use crate::types::{unwrap_resolved, TVar, Type, VarSet};

type TyEnv = HashMap<String, Rc<RefCell<Type>>>;

fn free_variables(ty: Rc<RefCell<Type>>) -> VarSet {
    use TVar::*;
    use Type::*;
    match &*ty.borrow() {
        ForAll(ref vars, body) => free_variables(body.clone())
            .difference(vars)
            .copied()
            .collect(),
        Var(tvar) => match &*tvar.borrow() {
            Resolved(ty) => free_variables(ty.clone()),
            Unresolved(id) => VarSet::from([id.to_owned()]),
        },
        Function(args, ret) => {
            let mut result = free_variables(ret.clone());
            for arg in args.iter() {
                result = &free_variables(arg.clone()) | &result;
            }
            result
        }
        Con(_) => VarSet::new(),
    }
}

type Constraint = (Rc<RefCell<Type>>, Rc<RefCell<Type>>);

fn maybe_link(var: Rc<RefCell<TVar>>, target: Rc<RefCell<Type>>) {
    use TVar::*;
    if let Resolved(solved) = &*var.borrow() {
        let solved_ty = &*solved.borrow();
        let referred_ty = &*target.borrow();
        if solved_ty == referred_ty {
            return;
        } else {
            panic!("type mismatch: {:?} and {:?}", solved_ty, referred_ty);
        }
    }
    *var.borrow_mut() = Resolved(target.clone());
}

fn unify_one(c: Constraint, out: &mut VecDeque<Constraint>) {
    let ty1 = unwrap_resolved(c.0);
    let ty2 = unwrap_resolved(c.1);

    if ty1 == ty2 {
        return;
    }

    use Type::*;

    let ty1_borrowed = (*ty1.borrow()).clone();
    let ty2_borrowed = (*ty2.borrow()).clone();

    match (ty1_borrowed, ty2_borrowed) {
        (Function(args1, ret1), Function(args2, ret2)) => {
            if args1.len() == args2.len() {
                out.push_back((ret1.clone(), ret2.clone()));
                for (arg1, arg2) in args1.iter().zip(args2.iter()) {
                    out.push_back((arg1.clone(), arg2.clone()));
                }
            } else {
                panic!(
                    "type mismatch: {:?} and {:?}",
                    Function(args1, ret1),
                    Function(args2, ret2)
                );
            }
        }
        (Var(tvar1), _) => maybe_link(tvar1.clone(), ty2.clone()),
        (_, Var(tvar2)) => maybe_link(tvar2.clone(), ty1.clone()),
        (lhs, rhs) => panic!("type mismatch: {:?} and {:?}", lhs, rhs),
    }
}

fn infer_atom(atom: &ast::Atom) -> Rc<RefCell<Type>> {
    use ast::Atom::*;

    match atom {
        I32(_) => Type::con("I32".into()),
        Bool(_) => Type::con("Bool".into()),
    }
}

fn infer_expr(
    env: &mut TyEnv,
    expr: &ast::Expr,
    constraints: &mut VecDeque<Constraint>,
) -> Rc<RefCell<Type>> {
    use ast::Expr::*;
    use ast::OpCode::*;

    match expr {
        Binary(left, op, right) => match op {
            Add | Sub | Mul | Div => {
                let i32_type = Type::con("I32".into());
                let left_type = infer_expr(env, left, constraints);
                let right_type = infer_expr(env, right, constraints);
                constraints.push_back((left_type.clone(), right_type));
                constraints.push_back((left_type, i32_type.clone()));
                i32_type
            }
            Equal | NotEqual => {
                let bool_type = Type::con("Bool".into());
                let left_type = infer_expr(env, left, constraints);
                let right_type = infer_expr(env, right, constraints);
                constraints.push_back((left_type.clone(), right_type));
                bool_type
            }
        },
        Atom(atom) => infer_atom(atom),
        Call(fn_name, args) => {
            let returned = Rc::new(RefCell::new(Type::unknown()));
            let mut arg_types = vec![];
            for arg in args.iter() {
                arg_types.push(infer_expr(env, arg, constraints));
            }
            let fn_type = env.get(fn_name).unwrap().clone();

            constraints.push_back((
                fn_type,
                Rc::new(RefCell::new(Type::Function(arg_types, returned.clone()))),
            ));

            returned
        }

        ast::Expr::Var(name) => env.get(name).unwrap().clone(),
    }
}

fn infer_var_decl(env: &mut TyEnv, v_decl: &ast::VarDecl, constraints: &mut VecDeque<Constraint>) {
    let inferred_type = infer_expr(env, &v_decl.value, constraints);
    let annotated_type = v_decl._type.clone();
    env.insert(v_decl.name.clone(), annotated_type.clone());
    constraints.push_back((inferred_type, annotated_type));
}

fn infer_block(
    env: &mut TyEnv,
    stmts: &Vec<ast::Stmt>,
    constraints: &mut VecDeque<Constraint>,
    return_type: Rc<RefCell<Type>>,
) {
    for stmt in stmts.iter() {
        infer_stmt(env, stmt, constraints, return_type.clone());
    }
}

fn infer_stmt(
    env: &mut TyEnv,
    stmt: &ast::Stmt,
    constraints: &mut VecDeque<Constraint>,
    returned_type: Rc<RefCell<Type>>,
) {
    use ast::Stmt::*;
    match stmt {
        VarDecl(v_decl) => infer_var_decl(env, v_decl, constraints),
        Expr(expr) => {
            infer_expr(env, &expr, constraints);
        }
        Return(returned) => {
            let return_inferred = infer_expr(env, &returned, constraints);
            constraints.push_back((return_inferred, returned_type))
        }
        IfElse(cond, then_branch, else_branch) => {
            let cond_type = infer_expr(env, &cond, constraints);
            infer_block(env, then_branch, constraints, returned_type.clone());
            infer_block(env, else_branch, constraints, returned_type);
            let bool_type = Type::con("Bool".into());
            constraints.push_back((cond_type, bool_type));
        }
        While(cond, body) => {
            let cond_type = infer_expr(env, &cond, constraints);
            infer_block(env, body, constraints, returned_type);
            let bool_type = Type::con("Bool".into());
            constraints.push_back((cond_type, bool_type));
        }
    }
}

fn infer_function_top(
    env: &mut TyEnv,
    func_def: &ast::FunctionDef,
    constraints: &mut VecDeque<Constraint>,
) {
    let mut body_env = env.clone();
    body_env.insert(func_def.name.clone(), func_def.get_type());
    for param in func_def.params.iter() {
        body_env.insert(param.name.clone(), param._type.clone());
    }

    infer_block(
        &mut body_env,
        &func_def.body,
        constraints,
        func_def.return_type.clone(),
    );
}

pub fn infer_type_func(func_def: &ast::FunctionDef) {
    let mut constraints: VecDeque<Constraint> = VecDeque::new();

    let mut env = TyEnv::new();

    infer_function_top(&mut env, func_def, &mut constraints);

    while let Some(c) = constraints.pop_front() {
        unify_one(c, &mut constraints);
    }
}

pub fn infer_type(prog: &ast::Program) {
    let mut constraints: VecDeque<Constraint> = VecDeque::new();
    use ast::*;

    let mut env = TyEnv::new();

    for item in prog.items.iter() {
        match item {
            Item::VarDecl(v_decl) => infer_var_decl(&mut env, v_decl, &mut constraints),
            Item::Function(func_def) => infer_function_top(&mut env, func_def, &mut constraints),
        }
    }

    while let Some(c) = constraints.pop_front() {
        unify_one(c, &mut constraints);
    }
}
