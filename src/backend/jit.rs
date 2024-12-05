use std::fmt::write;
use std::rc::Rc;
use std::{cell::RefCell, collections::HashMap};

use cranelift::frontend::FunctionBuilderContext;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataDescription, Linkage, Module};

use crate::frontend::ast::*;

pub struct JIT {
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    data_description: DataDescription,
    module: JITModule,
    func_sig_env: HashMap<String, Signature>,
}

fn declare_variable(
    builder: &mut FunctionBuilder,
    ty: Type,
    variables: &mut HashMap<String, Variable>,
    index: &mut usize,
    name: &str,
) -> Variable {
    let var = Variable::new(*index);
    if !variables.contains_key(name) {
        variables.insert(name.into(), var);
        builder.declare_var(var, ty);
        *index += 1;
    }
    var
}

impl Default for JIT {
    fn default() -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();

        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        let module = JITModule::new(builder);

        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description: DataDescription::new(),
            module,
            func_sig_env: HashMap::new(),
        }
    }
}

fn translate_type(module: &JITModule, ty: Rc<RefCell<crate::types::Type>>) -> Type {
    let type_resolved = crate::types::unwrap_resolved(ty);

    use crate::types::Type::*;

    let borrow = type_resolved.borrow();
    match &*borrow {
        Con(name) => match name.as_str() {
            "I32" => types::I32,
            "Bool" => types::I8,
            name => panic!("no such type: {}", name),
        },
        ForAll(..) | Var(_) => panic!("unknown type!"),
        Function(..) => module.target_config().pointer_type(),
    }
}

fn collect_variable_declarations(
    module: &JITModule,
    builder: &mut FunctionBuilder,
    params: &Vec<Param>,
    stmts: &Vec<Stmt>,
    entry_block: Block,
) -> HashMap<String, Variable> {
    let mut variables = HashMap::new();
    let mut index = 0;

    for (i, param) in params.iter().enumerate() {
        let val = builder.block_params(entry_block)[i];
        let param_type = translate_type(module, param._type.clone());
        let name = &param.name;
        let var = declare_variable(
            builder,
            param_type,
            &mut variables,
            &mut index,
            name.as_str(),
        );
        builder.def_var(var, val);
    }
    for stmt in stmts {
        declare_variables_in_stmt(module, builder, &mut variables, &mut index, stmt);
    }
    variables
}

// BUG: we need to fix the naming conflicts here
fn declare_variables_in_stmt(
    module: &JITModule,
    builder: &mut FunctionBuilder,
    variables: &mut HashMap<String, Variable>,
    index: &mut usize,
    stmt: &Stmt,
) {
    match stmt {
        Stmt::VarDecl(VarDecl { name, _type, .. }) => {
            declare_variable(
                builder,
                translate_type(module, _type.clone()),
                variables,
                index,
                name,
            );
        }
        Stmt::Expr(_) | Stmt::Return(_) => (),
        Stmt::IfElse(_, then_clause, else_clause) => {
            for stmt in then_clause {
                declare_variables_in_stmt(module, builder, variables, index, stmt);
            }
            for stmt in else_clause {
                declare_variables_in_stmt(module, builder, variables, index, stmt);
            }
        }
        Stmt::While(_, body) => {
            for stmt in body {
                declare_variables_in_stmt(module, builder, variables, index, stmt);
            }
        }
    }
}

impl JIT {
    pub fn compile_global(&mut self, decl: &VarDecl) {
        unimplemented!("global")
    }
    pub fn compile_function(&mut self, fun: &FunctionDef) -> Result<*const u8, String> {
        self.translate_function(&fun)?;

        let id = self
            .module
            .declare_function(&fun.name, Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| e.to_string())?;
        self.module.clear_context(&mut self.ctx);
        self.module.finalize_definitions().unwrap();

        let code = self.module.get_finalized_function(id);
        Ok(code)
    }

    fn translate_function(&mut self, fun: &FunctionDef) -> Result<(), String> {
        for param in &fun.params {
            let arg_ty = AbiParam::new(translate_type(&self.module, param._type.clone()));
            self.ctx.func.signature.params.push(arg_ty);
        }
        let ret_ty = AbiParam::new(translate_type(&self.module, fun.return_type.clone()));
        self.ctx.func.signature.returns.push(ret_ty);

        self.func_sig_env
            .insert(fun.name.clone(), self.ctx.func.signature.clone());

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        let entry_block = builder.create_block();
        builder.append_block_params_for_function_params(entry_block);
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let variables = collect_variable_declarations(
            &self.module,
            &mut builder,
            &fun.params,
            &fun.body,
            entry_block,
        );

        let mut trans = FunctionTranslator {
            builder,
            variables,
            module: &mut self.module,
            func_sig_env: &self.func_sig_env,
        };

        trans.translate_stmts(&fun.body);
        trans.builder.finalize();

        Ok(())
    }
}

struct FunctionTranslator<'a> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Variable>,
    module: &'a mut JITModule,
    func_sig_env: &'a HashMap<String, Signature>,
}

struct StmtState {
    reach_branching: bool,
}

impl<'a> FunctionTranslator<'a> {
    fn translate_atom(&mut self, atom: &Atom) -> Value {
        use Atom::*;
        match atom {
            I32(i) => {
                let i32_type = translate_type(self.module, crate::types::Type::con("I32".into()));

                self.builder.ins().iconst(i32_type, i64::from(*i))
            }
            Bool(b) => {
                let bool_type = translate_type(self.module, crate::types::Type::con("Bool".into()));
                self.builder.ins().iconst(bool_type, if *b { 1 } else { 0 })
            }
        }
    }
    fn translate_expr(&mut self, expr: &Expr) -> Value {
        use Expr::*;
        use OpCode::*;
        match expr {
            Atom(a) => self.translate_atom(&a),
            Binary(lhs, Add, rhs) => {
                let lhs = self.translate_expr(lhs);
                let rhs = self.translate_expr(rhs);
                self.builder.ins().iadd(lhs, rhs)
            }
            Binary(lhs, Sub, rhs) => {
                let lhs = self.translate_expr(lhs);
                let rhs = self.translate_expr(rhs);
                self.builder.ins().isub(lhs, rhs)
            }
            Binary(lhs, Mul, rhs) => {
                let lhs = self.translate_expr(lhs);
                let rhs = self.translate_expr(rhs);
                self.builder.ins().imul(lhs, rhs)
            }
            Binary(lhs, Div, rhs) => {
                let lhs = self.translate_expr(lhs);
                let rhs = self.translate_expr(rhs);
                self.builder.ins().udiv(lhs, rhs)
            }
            Binary(lhs, Equal, rhs) => {
                let lhs = self.translate_expr(lhs);
                let rhs = self.translate_expr(rhs);
                self.builder.ins().icmp(IntCC::Equal, lhs, rhs)
            }
            Binary(lhs, NotEqual, rhs) => {
                let lhs = self.translate_expr(lhs);
                let rhs = self.translate_expr(rhs);
                self.builder.ins().icmp(IntCC::NotEqual, lhs, rhs)
            }
            Var(name) => {
                let variable = self.variables.get(name).expect("variable not defined");
                self.builder.use_var(*variable)
            }
            Call(name, args) => self.translate_call(name, args),
        }
    }

    fn translate_call(&mut self, name: &String, args: &Vec<Box<Expr>>) -> Value {
        let sig = self.func_sig_env.get(name).unwrap();

        // TODO: Streamline the API here?
        let callee = self
            .module
            .declare_function(&name, Linkage::Import, &sig)
            .expect("problem declaring function");
        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

        let mut arg_values = Vec::new();
        for arg in args {
            arg_values.push(self.translate_expr(arg))
        }
        let call = self.builder.ins().call(local_callee, &arg_values);
        self.builder.inst_results(call)[0]
    }

    fn translate_stmts(&mut self, stmts: &Vec<Stmt>) -> StmtState {
        let mut stmt_state = StmtState {
            reach_branching: false,
        };
        for stmt in stmts {
            stmt_state = self.translate_stmt(stmt);
            if stmt_state.reach_branching {
                break;
            }
        }
        stmt_state
    }

    fn translate_stmt(&mut self, stmt: &Stmt) -> StmtState {
        match stmt {
            Stmt::VarDecl(VarDecl { name, _type, value }) => {
                let val = self.translate_expr(value);
                let var = self.variables.get(name).unwrap();
                self.builder.def_var(*var, val);
                StmtState {
                    reach_branching: false,
                }
            }
            Stmt::Expr(expr) => {
                self.translate_expr(expr);
                StmtState {
                    reach_branching: true,
                }
            }
            Stmt::Return(expr) => {
                let to_return = self.translate_expr(expr);
                self.builder.ins().return_(&[to_return]);
                StmtState {
                    reach_branching: true,
                }
            }
            Stmt::IfElse(cond, then_branch, else_branch) => {
                self.translate_if(cond, then_branch, else_branch)
            }

            Stmt::While(cond, body) => self.translate_while(cond, body),
        }
    }

    fn translate_if(
        &mut self,
        cond: &Expr,
        then_branch: &Vec<Stmt>,
        else_branch: &Vec<Stmt>,
    ) -> StmtState {
        let condition_value = self.translate_expr(cond);
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();

        self.builder
            .ins()
            .brif(condition_value, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        self.builder.seal_block(then_block);
        let stmt_state = self.translate_stmts(then_branch);
        if !stmt_state.reach_branching {
            self.builder.ins().jump(merge_block, &[]);
        }

        self.builder.switch_to_block(else_block);
        self.builder.seal_block(else_block);

        let stmt_state = self.translate_stmts(else_branch);
        if !stmt_state.reach_branching {
            self.builder.ins().jump(merge_block, &[]);
        }

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        StmtState {
            reach_branching: false,
        }
    }

    fn translate_while(&mut self, cond: &Expr, body: &Vec<Stmt>) -> StmtState {
        let header_block = self.builder.create_block();
        let body_block = self.builder.create_block();
        let exit_block = self.builder.create_block();

        self.builder.ins().jump(header_block, &[]);

        self.builder.switch_to_block(header_block);
        let condition = self.translate_expr(cond);
        self.builder
            .ins()
            .brif(condition, body_block, &[], exit_block, &[]);

        self.builder.switch_to_block(body_block);
        self.builder.seal_block(body_block);
        let stmt_state = self.translate_stmts(body);
        if !stmt_state.reach_branching {
            self.builder.ins().jump(header_block, &[]);
        }

        self.builder.switch_to_block(exit_block);

        self.builder.seal_block(header_block);
        self.builder.seal_block(exit_block);
        StmtState {
            reach_branching: false,
        }
    }
}
