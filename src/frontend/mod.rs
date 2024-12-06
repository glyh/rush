use lalrpop_util::lalrpop_mod;

pub mod ast;
pub mod lexer;
pub mod tokens;

lalrpop_mod!(
    pub rush, "/frontend/rush.rs"
);

#[test]
fn rush_parse() {
    use std::cell::RefCell;
    use std::rc::Rc;

    use crate::frontend::ast::*;
    use crate::frontend::rush;
    use crate::types::*;

    let source = "fn main() -> I32 { return 42; }";

    let lexer = lexer::Lexer::new(source);

    let prog_parser = rush::ProgramParser::new();
    let ast = prog_parser.parse(lexer).unwrap();
    assert_eq!(
        ast,
        Program {
            items: vec![Item::Function(FunctionDef {
                name: "main".to_string(),
                params: vec![],
                body: vec![Stmt::Return(Box::new(Expr::Atom(Box::new(Atom::I32(42)))),),],
                return_type: Rc::new(RefCell::new(Type::Con("I32".into()))),
            }),],
        }
    );
}
