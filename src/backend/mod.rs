mod jit;

use crate::frontend::ast;
use core::mem;

unsafe fn run_code<I, O>(
    jit: &mut jit::JIT,
    fun_def: &ast::FunctionDef,
    input: I,
) -> Result<O, String> {
    // Pass the string to the JIT, and it returns a raw pointer to machine code.
    let code_ptr = jit.compile_function(fun_def)?;
    // Cast the raw pointer to a typed function pointer. This is unsafe, because
    // this is the critical point where you have to trust that the generated code
    // is safe to be called.
    let code_fn = mem::transmute::<_, fn(I) -> O>(code_ptr);
    // And now we can call it!
    Ok(code_fn(input))
}

#[test]
fn test_fib() {
    let source = r#"
fn fib(n) {
    if (n == 0) {
        return 0;
    } else {
        if (n == 1) {
            return 1;
        } else {
            return fib(n - 1) + fib(n - 2);
        }
    }
}"#;

    let lexer = crate::frontend::lexer::Lexer::new(source);

    let prog_parser = crate::frontend::rush::FunctionDefParser::new();

    let func_ast = prog_parser.parse(lexer).unwrap();
    crate::typecheck::inferencer::infer_type_func(&func_ast);

    let mut jit = jit::JIT::default();

    assert_eq!(unsafe { run_code(&mut jit, &func_ast, 7i32) }, Ok(13i32));
}
