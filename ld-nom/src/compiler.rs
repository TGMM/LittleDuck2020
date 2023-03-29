use crate::{
    ast::{
        Condition, Exp, ExpBOp, ExpOp, Expr, ExprRhs, ExpressionOp, Factor, PrintType, Program,
        Statement, Term, TermBOp, TermOp, Token, VarType,
    },
    lexer::token_parser,
    parser::program_parser,
    token::Tokens,
};
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    passes::PassManager,
    targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetTriple},
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, GlobalValue,
        PointerValue,
    },
    AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel,
};
use std::{error::Error, path::Path};

static MAIN_FN_NAME: &str = "main";
pub struct Compiler<'input, 'ctx> {
    pub context: &'ctx Context,
    pub builder: &'input Builder<'ctx>,
    pub fpm: &'input PassManager<FunctionValue<'ctx>>,
    pub module: &'input Module<'ctx>,
}

impl<'input, 'ctx> Compiler<'input, 'ctx> {
    fn build_factor(&self, factor: Factor) -> BasicValueEnum {
        if let Factor::ConstantVal(constant) = factor {
            let float_type = self.context.f32_type();
            let int_type = self.context.i32_type();

            return match constant {
                crate::ast::VarValue::Float(f) => {
                    float_type.const_float(f.into()).as_basic_value_enum()
                }
                crate::ast::VarValue::Int(i) => {
                    let i: i64 = i.into();
                    let i: u64 = unsafe { std::mem::transmute(i) };

                    return int_type.const_int(i, false).as_basic_value_enum();
                }
                crate::ast::VarValue::Var(id) => {
                    let id = id.0.as_str();
                    let var = self
                        .module
                        .get_global(id)
                        .expect(format!("Invalid reference to non-existant global: {id}").as_str());

                    let ptr_var = var.as_pointer_value();
                    self.builder.build_load(ptr_var, "globalderef")
                }
            };
        }

        if let Factor::ParenExpr(pexpr) = factor {
            return self.build_expr(*pexpr);
        }

        unreachable!()
    }

    fn build_term(&self, term: Term) -> BasicValueEnum {
        if let Term::Factor(factor) = term {
            return self.build_factor(factor);
        }

        if let Term::BOp(bop) = term {
            let result = self.build_term_bop(*bop);
            return result;
        }

        unreachable!()
    }

    fn build_term_bop(&self, exp_bop: TermBOp) -> BasicValueEnum {
        let TermBOp { lhs, op, rhs } = exp_bop;
        let lhs = self.build_term(lhs);
        let rhs = self.build_term(rhs);

        let bop_type = lhs.get_type();
        assert_eq!(lhs.get_type(), rhs.get_type());

        if let BasicTypeEnum::FloatType(_) = bop_type {
            let lhs = lhs.into_float_value();
            let rhs = rhs.into_float_value();

            match op {
                TermOp::Mul => {
                    return self
                        .builder
                        .build_float_mul(lhs, rhs, "fmul")
                        .as_basic_value_enum();
                }
                TermOp::Div => {
                    return self
                        .builder
                        .build_float_div(lhs, rhs, "fdiv")
                        .as_basic_value_enum();
                }
            }
        }

        if let BasicTypeEnum::IntType(_) = bop_type {
            let lhs = lhs.into_int_value();
            let rhs = rhs.into_int_value();

            match op {
                TermOp::Mul => {
                    return self
                        .builder
                        .build_int_mul(lhs, rhs, "imul")
                        .as_basic_value_enum();
                }
                TermOp::Div => {
                    return self
                        .builder
                        .build_int_signed_div(lhs, rhs, "idiv")
                        .as_basic_value_enum();
                }
            }
        }

        unreachable!()
    }

    fn build_exp(&self, exp: Exp) -> BasicValueEnum {
        if let Exp::Term(term) = exp {
            return self.build_term(term);
        }

        if let Exp::BOp(bop) = exp {
            let result = self.build_exp_bop(*bop);
            return result;
        }

        unreachable!()
    }

    fn build_exp_bop(&self, exp_bop: ExpBOp) -> BasicValueEnum {
        let ExpBOp { lhs, op, rhs } = exp_bop;
        let lhs = self.build_exp(lhs);
        let rhs = self.build_exp(rhs);

        let bop_type = lhs.get_type();
        assert_eq!(lhs.get_type(), rhs.get_type());

        if let BasicTypeEnum::FloatType(_) = bop_type {
            let lhs = lhs.into_float_value();
            let rhs = rhs.into_float_value();

            match op {
                ExpOp::Add => {
                    return self
                        .builder
                        .build_float_add(lhs, rhs, "fadd")
                        .as_basic_value_enum();
                }
                ExpOp::Sub => {
                    return self
                        .builder
                        .build_float_sub(lhs, rhs, "fsub")
                        .as_basic_value_enum();
                }
            }
        }

        if let BasicTypeEnum::IntType(_) = bop_type {
            let lhs = lhs.into_int_value();
            let rhs = rhs.into_int_value();

            match op {
                ExpOp::Add => {
                    return self
                        .builder
                        .build_int_add(lhs, rhs, "iadd")
                        .as_basic_value_enum();
                }
                ExpOp::Sub => {
                    return self
                        .builder
                        .build_int_sub(lhs, rhs, "isub")
                        .as_basic_value_enum();
                }
            }
        }

        unreachable!()
    }

    fn build_expr(&self, expr: Expr) -> BasicValueEnum {
        if expr.rhs.is_none() {
            return match expr.lhs {
                Exp::Term(val) => self.build_term(val),
                Exp::BOp(bop) => self.build_exp_bop(*bop),
            };
        }

        let lhs = self.build_exp(expr.lhs);
        let ExprRhs { op, rhs } = expr.rhs.unwrap();
        let rhs = self.build_exp(rhs);

        let bop_type = lhs.get_type();
        assert_eq!(lhs.get_type(), rhs.get_type());

        if let BasicTypeEnum::FloatType(_) = bop_type {
            let lhs = lhs.into_float_value();
            let rhs = rhs.into_float_value();

            return match op {
                ExpressionOp::Gt => self
                    .builder
                    .build_float_compare(FloatPredicate::OGT, lhs, rhs, "fgtcmp")
                    .as_basic_value_enum(),
                ExpressionOp::Lt => self
                    .builder
                    .build_float_compare(FloatPredicate::OLT, lhs, rhs, "fltcmp")
                    .as_basic_value_enum(),
                ExpressionOp::LtGt => self
                    .builder
                    .build_float_compare(FloatPredicate::ONE, lhs, rhs, "fnecmp")
                    .as_basic_value_enum(),
            };
        }

        if let BasicTypeEnum::IntType(_) = bop_type {
            let lhs = lhs.into_int_value();
            let rhs = rhs.into_int_value();

            return match op {
                ExpressionOp::Gt => self
                    .builder
                    .build_int_compare(IntPredicate::SGT, lhs, rhs, "igtcmp")
                    .as_basic_value_enum(),
                ExpressionOp::Lt => self
                    .builder
                    .build_int_compare(IntPredicate::SLT, lhs, rhs, "iltcmp")
                    .as_basic_value_enum(),
                ExpressionOp::LtGt => self
                    .builder
                    .build_int_compare(IntPredicate::NE, lhs, rhs, "inecmp")
                    .as_basic_value_enum(),
            };
        }

        unreachable!()
    }

    fn build_cond(&self, cond: Condition) {
        let cond_expr = self.build_expr(cond.expression).into_int_value();
        let fun = self.module.get_function(MAIN_FN_NAME).unwrap();

        // Branch
        let then_bb = self.context.append_basic_block(fun, "then");
        let else_bb = self.context.append_basic_block(fun, "else");
        let cont_bb = self.context.append_basic_block(fun, "ifcont");

        self.builder
            .build_conditional_branch(cond_expr, then_bb, else_bb);

        // Then
        self.builder.position_at_end(then_bb);
        for stmt in cond.then_block.statements {
            self.build_stmt(stmt);
        }
        self.builder.build_unconditional_branch(cont_bb);

        // Else
        if let Some(else_block) = cond.else_block {
            self.builder.position_at_end(else_bb);
            for stmt in else_block.statements {
                self.build_stmt(stmt);
            }
        }
        self.builder.build_unconditional_branch(cont_bb);

        // Merge
        self.builder.position_at_end(cont_bb);
    }

    fn build_stmt(&self, stmt: Statement) {
        match stmt {
            Statement::Assignment(assmt) => {
                let var_id = assmt.id.0.as_str();
                let var = self.module.get_global(var_id).expect(
                    format!("Unexpected assignment to undeclared variable {var_id}").as_str(),
                );
                let var_ptr = var.as_pointer_value();

                let new_val = self.build_expr(assmt.value);
                self.builder.build_store(var_ptr, new_val);
            }
            Statement::Condition(cond) => self.build_cond(cond),
            Statement::Print(print) => {
                let printf = self
                    .module
                    .get_function("printf")
                    .expect("Could not find printf. Make sure you're linking to libc.");

                let mut print_str = String::new();
                let mut print_args = Vec::new();
                for output in print.output {
                    match output {
                        PrintType::Expression(expr) => {
                            let mut value = self.build_expr(expr);
                            if let BasicValueEnum::PointerValue(ptr) = value {
                                value = self.builder.build_load(ptr, "ptrtoval");
                            }

                            let mut add_specifier = |v| match v {
                                BasicValueEnum::IntValue(_) => {
                                    print_str += "%d";
                                }
                                BasicValueEnum::FloatValue(_) => {
                                    print_str += "%f";
                                }
                                _ => unreachable!(),
                            };

                            add_specifier(value);
                            print_args.push(value);
                        }
                        PrintType::Str(string) => print_str += string.as_str(),
                    }
                }
                let message = self.create_global_str(print_str.as_str());
                let message_ptr = self.create_ptr_from_global_str(message);

                let print_args_iter = print_args.into_iter().map(|f| match f {
                    BasicValueEnum::IntValue(i) => BasicMetadataValueEnum::IntValue(i),
                    BasicValueEnum::FloatValue(f) => BasicMetadataValueEnum::FloatValue(f),
                    _ => unreachable!(),
                });
                let args: Vec<BasicMetadataValueEnum> =
                    [BasicMetadataValueEnum::PointerValue(message_ptr)]
                        .into_iter()
                        .chain(print_args_iter)
                        .collect();
                self.builder.build_call(printf, &args, "print");
            }
        }
    }

    fn create_global_str(&self, new_str: &str) -> GlobalValue<'ctx> {
        let new_str = unsafe { self.builder.build_global_string(new_str, "globstr") };
        new_str
    }

    fn create_ptr_from_global_str(&self, str_val: GlobalValue<'ctx>) -> PointerValue<'ctx> {
        let str_ptr = str_val.as_pointer_value();

        let i8_type = self.context.i8_type();
        let zero = i8_type.const_zero();
        let gep_ptr = unsafe { self.builder.build_gep(str_ptr, &[zero, zero], "globstrptr") };

        gep_ptr
    }

    fn codegen(&self, program: Program) {
        let fn_type = self.context.i32_type().fn_type(&[], false);
        let fun = self
            .module
            .add_function(MAIN_FN_NAME, fn_type, Some(Linkage::External));
        let entry_basic_block = self.context.append_basic_block(fun, "entry");
        self.builder.position_at_end(entry_basic_block);

        let int_type = self.context.i32_type();
        let float_type = self.context.f32_type();

        let default_int = int_type.const_zero();
        let default_float = float_type.const_zero();

        // Declare global variables
        for var in program.vars {
            let (var_type, default_val): (BasicTypeEnum, BasicValueEnum) = match var.vtype {
                VarType::Float => (
                    float_type.as_basic_type_enum(),
                    default_float.as_basic_value_enum(),
                ),
                VarType::Int => (
                    int_type.as_basic_type_enum(),
                    default_int.as_basic_value_enum(),
                ),
            };
            let new_global = self.module.add_global(var_type, None, var.id.0.as_str());
            new_global.set_initializer(&default_val);
        }

        // Main block
        for stmt in program.block.statements {
            self.build_stmt(stmt);
        }

        let int_zero = self.context.i32_type().const_zero();
        self.builder.build_return(Some(&int_zero));
    }

    fn compile_to_x86(compiler: &Compiler<'input, 'ctx>, path: &str, file_name: &str) {
        Target::initialize_x86(&InitializationConfig::default());
        let triple = TargetTriple::create("x86_64-pc-windows-msvc");
        let target = Target::from_triple(&triple).unwrap();
        let cpu = "generic";
        let features = "";
        let target_machine = target
            .create_target_machine(
                &triple,
                cpu,
                features,
                OptimizationLevel::None,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();

        compiler.module.set_triple(&triple);
        compiler
            .module
            .set_data_layout(&target_machine.get_target_data().get_data_layout());

        compiler
            .module
            .print_to_file(&format!("{path}/{file_name}.ll"))
            .unwrap();
        target_machine
            .write_to_file(
                compiler.module,
                FileType::Object,
                Path::new(&format!("{path}/{file_name}.o")),
            )
            .unwrap();
        target_machine
            .write_to_file(
                compiler.module,
                FileType::Assembly,
                Path::new(&format!("{path}/{file_name}.asm")),
            )
            .unwrap();
    }
}

pub fn compile_ld(input: &str, output_dir: &str, output_name: &str) -> Result<(), Box<dyn Error>> {
    let (_, token_span_vec) = token_parser(input).unwrap();
    let token_vec: Vec<Token> = token_span_vec.into_iter().map(|ts| ts.into()).collect();
    let tokens = Tokens::new(&token_vec);
    let (_, program) = program_parser(tokens).unwrap();
    let id = program.id.0.as_str();

    let context = Context::create();
    let module = context.create_module(id);
    let builder = context.create_builder();

    let fpm = PassManager::create(&module);
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.add_gvn_pass();
    fpm.add_cfg_simplification_pass();
    fpm.add_basic_alias_analysis_pass();
    fpm.add_promote_memory_to_register_pass();
    fpm.add_instruction_combining_pass();
    fpm.add_reassociate_pass();
    fpm.initialize();

    let compiler = Compiler {
        builder: &builder,
        module: &module,
        context: &context,
        fpm: &fpm,
    };

    // Link to printf
    let const_char_ptr_type = compiler.context.i8_type().ptr_type(AddressSpace::default());
    let const_char_ptr_enum = BasicMetadataTypeEnum::PointerType(const_char_ptr_type);
    let printf_signature = compiler
        .context
        .i32_type()
        .fn_type(&[const_char_ptr_enum], true);
    compiler
        .module
        .add_function("printf", printf_signature, Some(Linkage::External));

    compiler.codegen(program);
    Compiler::compile_to_x86(&compiler, output_dir, output_name);

    Ok(())
}
