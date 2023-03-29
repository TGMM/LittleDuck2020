use crate::ast::{
    Assignment, Block, Condition, Exp, ExpBOp, ExpOp, Expr, ExprRhs, ExpressionOp, Factor, Id,
    Print, PrintType, Program, Statement, Term, TermBOp, TermOp, Token, Var, VarType, VarValue,
};
use crate::token::Tokens;
use nom::branch::alt;
use nom::bytes::complete::take;
use nom::combinator::{map, opt, verify};
use nom::error::{Error, ErrorKind};
use nom::multi::many0;
use nom::sequence::{delimited, pair, preceded};
use nom::Err;
use nom::IResult;

macro_rules! tag_token (
    ($func_name:ident, $tag: expr) => (
        fn $func_name(tokens: Tokens) -> IResult<Tokens, Tokens> {
            verify(take(1usize), |t: &Tokens| t.tok_span[0].token == $tag)(tokens)
        }
    )
  );

tag_token!(program_tag, Token::Program);
tag_token!(stmt_end_tag, Token::StmtEnd);
tag_token!(var_tag, Token::Var);
tag_token!(lbracket_tag, Token::LBracket);
tag_token!(rbracket_tag, Token::RBracket);
tag_token!(lparen_tag, Token::LParen);
tag_token!(rparen_tag, Token::RParen);
tag_token!(comma_tag, Token::Comma);
tag_token!(type_sep_tag, Token::TypeSep);
tag_token!(int_tag, Token::Int);
tag_token!(float_tag, Token::Float);
tag_token!(eq_tag, Token::Eq);
tag_token!(if_tag, Token::If);
tag_token!(else_tag, Token::Else);
tag_token!(print_tag, Token::Print);
tag_token!(gt_tag, Token::Gt);
tag_token!(lt_tag, Token::Lt);
tag_token!(lt_gt_tag, Token::LtGt);
tag_token!(add_tag, Token::Add);
tag_token!(sub_tag, Token::Sub);
tag_token!(mul_tag, Token::Mul);
tag_token!(div_tag, Token::Div);

fn id_parser(input: Tokens) -> IResult<Tokens, Id> {
    let (remaining_tokens, sym_token) = take(1usize)(input)?;
    if sym_token.tok_span.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match sym_token.tok_span[0].token.clone() {
            Token::Id(id) => Ok((remaining_tokens, id)),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

fn string_parser(input: Tokens) -> IResult<Tokens, String> {
    let (remaining_tokens, sym_token) = take(1usize)(input)?;
    if sym_token.tok_span.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match sym_token.tok_span[0].token.clone() {
            Token::Str(str_val) => Ok((remaining_tokens, str_val)),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

fn num_literal_parser(input: Tokens) -> IResult<Tokens, VarValue> {
    let (remaining_tokens, sym_token) = take(1usize)(input)?;
    if sym_token.tok_span.is_empty() {
        Err(Err::Error(Error::new(input, ErrorKind::Tag)))
    } else {
        match sym_token.tok_span[0].token.clone() {
            Token::Num(value) => Ok((remaining_tokens, value)),
            _ => Err(Err::Error(Error::new(input, ErrorKind::Tag))),
        }
    }
}

fn var_type_parser(input: Tokens) -> IResult<Tokens, VarType> {
    map(alt((int_tag, float_tag)), |tokens| {
        let tok_span = &tokens.tok_span[0];
        let var_type: VarType = match tok_span.token {
            Token::Float => VarType::Float,
            Token::Int => VarType::Int,
            _ => unreachable!(),
        };
        var_type
    })(input)
}

fn vars_parser(input: Tokens) -> IResult<Tokens, Vec<Var>> {
    let (input, _) = var_tag(input)?;
    let (input, first_var) = id_parser(input)?;

    // Var Idents
    let (input, var_idents) = many0(preceded(comma_tag, id_parser))(input)?;

    let (input, _) = type_sep_tag(input)?;
    // Tipo: Int o Float
    let (input, var_type) = var_type_parser(input)?;
    let (input, _) = stmt_end_tag(input)?;

    let vars = [first_var]
        .into_iter()
        .chain(var_idents)
        .into_iter()
        .map(|id| Var {
            id,
            vtype: var_type,
        })
        .collect();
    Ok((input, vars))
}

fn block_parser(input: Tokens) -> IResult<Tokens, Block> {
    let (input, statements) = delimited(lbracket_tag, many0(stmt_parser), rbracket_tag)(input)?;

    let block = Block { statements };
    Ok((input, block))
}

fn stmt_parser(input: Tokens) -> IResult<Tokens, Statement> {
    let assignment = map(assignment_parser, |a| Statement::Assignment(a));
    let condition = map(condition_parser, |c| Statement::Condition(c));
    let print = map(print_parser, |p| Statement::Print(p));

    alt((assignment, condition, print))(input)
}

fn assignment_parser(input: Tokens) -> IResult<Tokens, Assignment> {
    let (input, id) = id_parser(input)?;
    let (input, _) = eq_tag(input)?;
    let (input, expr) = expression_parser(input)?;
    let (input, _) = stmt_end_tag(input)?;

    let assignment = Assignment { id, value: expr };
    Ok((input, assignment))
}

fn condition_parser(input: Tokens) -> IResult<Tokens, Condition> {
    let (input, _) = if_tag(input)?;
    let (input, expr) = delimited(lparen_tag, expression_parser, rparen_tag)(input)?;
    let (input, then_block) = block_parser(input)?;

    let (input, else_block) = opt(preceded(else_tag, block_parser))(input)?;

    let (input, _) = stmt_end_tag(input)?;

    let condition = Condition {
        expression: expr,
        then_block,
        else_block,
    };
    Ok((input, condition))
}

fn print_parser(input: Tokens) -> IResult<Tokens, Print> {
    let (input, _) = print_tag(input)?;
    let (input, _) = lparen_tag(input)?;

    let printable_expr = map(expression_parser, |expr| PrintType::Expression(expr));
    let printable_str = map(string_parser, |string| PrintType::Str(string));
    let mut printable = alt((printable_expr, printable_str));

    let (input, first_output) = printable(input)?;
    let (input, mut rest_output) = many0(preceded(comma_tag, printable))(input)?;
    let (input, _) = rparen_tag(input)?;
    let (input, _) = stmt_end_tag(input)?;

    // Insert first item at the beggining of vec (which might be empty)
    rest_output.insert(0, first_output);
    let output = rest_output;
    let print = Print { output };
    Ok((input, print))
}

fn expression_parser(input: Tokens) -> IResult<Tokens, Expr> {
    let (input, lhs) = exp_parser(input)?;
    let comparator = map(alt((lt_gt_tag, gt_tag, lt_tag)), |op| {
        match op.tok_span[0].token {
            Token::Gt => ExpressionOp::Gt,
            Token::Lt => ExpressionOp::Lt,
            Token::LtGt => ExpressionOp::LtGt,
            _ => unreachable!(),
        }
    });
    let (input, expr_rhs) = opt(pair(comparator, exp_parser))(input)?;

    let rhs = expr_rhs.map(|erhs| ExprRhs {
        op: erhs.0,
        rhs: erhs.1,
    });
    let expr = Expr { lhs, rhs };
    Ok((input, expr))
}

fn exp_parser(input: Tokens) -> IResult<Tokens, Exp> {
    let (input, lhs) = term_parser(input)?;
    let add = map(add_tag, |_| ExpOp::Add);
    let sub = map(sub_tag, |_| ExpOp::Sub);
    let add_or_sub = alt((add, sub));
    let (input, exp_rhs) = many0(pair(add_or_sub, term_parser))(input)?;

    if exp_rhs.is_empty() {
        return Ok((input, Exp::Term(lhs)));
    }

    if exp_rhs.len() == 1 {
        let (op, rhs) = exp_rhs.into_iter().next().unwrap();
        return Ok((
            input,
            Exp::BOp(Box::new(ExpBOp {
                lhs: Exp::Term(lhs),
                op,
                rhs: Exp::Term(rhs),
            })),
        ));
    }

    let mut lhs = Exp::Term(lhs);
    for (op, rhs) in exp_rhs {
        lhs = Exp::BOp(Box::new(ExpBOp {
            lhs,
            op,
            rhs: Exp::Term(rhs),
        }));
    }

    Ok((input, lhs))
}

fn term_parser(input: Tokens) -> IResult<Tokens, Term> {
    let (input, lhs) = factor_parser(input)?;
    let mul = map(mul_tag, |_| TermOp::Mul);
    let div = map(div_tag, |_| TermOp::Div);
    let mul_or_div = alt((mul, div));
    let (input, term_rhs) = many0(pair(mul_or_div, factor_parser))(input)?;

    if term_rhs.is_empty() {
        return Ok((input, Term::Factor(lhs)));
    }

    if term_rhs.len() == 1 {
        let (op, rhs) = term_rhs.into_iter().next().unwrap();
        return Ok((
            input,
            Term::BOp(Box::new(TermBOp {
                lhs: Term::Factor(lhs),
                op,
                rhs: Term::Factor(rhs),
            })),
        ));
    }

    let mut lhs = Term::Factor(lhs);
    for (op, rhs) in term_rhs {
        lhs = Term::BOp(Box::new(TermBOp {
            lhs,
            op,
            rhs: Term::Factor(rhs),
        }));
    }

    Ok((input, lhs))
}

fn factor_parser(input: Tokens) -> IResult<Tokens, Factor> {
    let constant = map(constant_value_parser, |val| Factor::ConstantVal(val));
    let parenthesized_expr = map(delimited(lparen_tag, expression_parser, rparen_tag), |pe| {
        Factor::ParenExpr(Box::new(pe))
    });

    alt((constant, parenthesized_expr))(input)
}

fn constant_value_parser(input: Tokens) -> IResult<Tokens, VarValue> {
    let var = map(id_parser, |id| VarValue::Var(id));

    alt((num_literal_parser, var))(input)
}

pub fn program_parser(input: Tokens) -> IResult<Tokens, Program> {
    let (input, _) = program_tag(input)?;
    let (input, program_id) = id_parser(input)?;
    let (input, _) = stmt_end_tag(input)?;

    let (input, vars) = map(many0(vars_parser), |v| v.into_iter().flatten().collect())(input)?;
    let (input, block) = block_parser(input)?;

    let program = Program {
        id: program_id,
        vars,
        block,
    };
    Ok((input, program))
}

#[cfg(test)]
mod test {
    use crate::{
        ast::{Token, VarValue},
        parser::{exp_parser, program_parser, term_parser},
        token::Tokens,
    };

    #[test]
    fn term_parser_test() {
        let token_span_vec = &[
            Token::Num(VarValue::Int(1)),
            Token::Mul,
            Token::Num(VarValue::Int(2)),
            Token::Div,
            Token::Num(VarValue::Int(3)),
            Token::Mul,
            Token::Num(VarValue::Int(4)),
        ]
        .map(|t| t.into());
        let tokens = Tokens::new(token_span_vec);
        let res = term_parser(tokens);
        assert!(res.is_ok());
    }

    #[test]
    fn exp_parser_test() {
        let token_span_vec = &[
            Token::Num(VarValue::Int(1)),
            Token::Add,
            Token::Num(VarValue::Int(2)),
            Token::Div,
            Token::Num(VarValue::Int(3)),
            Token::Sub,
            Token::Num(VarValue::Int(4)),
        ]
        .map(|t| t.into());
        let tokens = Tokens::new(token_span_vec);
        let res = exp_parser(tokens);
        assert!(res.is_ok());

        let (remaining, _term) = res.unwrap();
        assert!(remaining.tok_span.is_empty());
    }

    #[test]
    fn full_parser_test() {
        // program my_program;
        // var my_var: int;
        // var my_other_var, my_other_other_var: float;
        // {
        //     my_var = 10.0;
        //     my_var = 10 > 10;
        //     my_var = 10 < 10;
        //     my_var = 10 <> 10;
        //     my_var = 10 + 5 * 30;
        //     my_var = (10 + 5) * 30;
        //     my_var = 10 + (5 * 30);
        //
        //     print("test");
        //     print("test", my_var, 10);
        // }
        let token_arr = [
            // program my_program;
            Token::Program,
            Token::Id("my_program".into()),
            Token::StmtEnd,
            // var my_var: int;
            Token::Var,
            Token::Id("my_var".into()),
            Token::TypeSep,
            Token::Int,
            Token::StmtEnd,
            // var my_other_var, my_other_other_var: float;
            Token::Var,
            Token::Id("my_other_var".into()),
            Token::Comma,
            Token::Id("my_other_other_var".into()),
            Token::TypeSep,
            Token::Float,
            Token::StmtEnd,
            // {
            Token::LBracket,
            // my_var = 10.0;
            Token::Id("my_var".into()),
            Token::Eq,
            Token::Num(VarValue::Float(10.0)),
            Token::StmtEnd,
            // my_var = 10 > 10;
            Token::Id("my_var".into()),
            Token::Eq,
            Token::Num(VarValue::Int(10)),
            Token::Gt,
            Token::Num(VarValue::Int(10)),
            Token::StmtEnd,
            // my_var = 10 < 10;
            Token::Id("my_var".into()),
            Token::Eq,
            Token::Num(VarValue::Int(10)),
            Token::Lt,
            Token::Num(VarValue::Int(10)),
            Token::StmtEnd,
            // my_var = 10 <> 10;
            Token::Id("my_var".into()),
            Token::Eq,
            Token::Num(VarValue::Int(10)),
            Token::LtGt,
            Token::Num(VarValue::Int(10)),
            Token::StmtEnd,
            // my_var = 10 + 5 * 30;
            Token::Id("my_var".into()),
            Token::Eq,
            Token::Num(VarValue::Int(10)),
            Token::Add,
            Token::Num(VarValue::Int(5)),
            Token::Mul,
            Token::Num(VarValue::Int(30)),
            Token::StmtEnd,
            // my_var = (10 + 5) * 30;
            Token::Id("my_var".into()),
            Token::Eq,
            Token::LParen,
            Token::Num(VarValue::Int(10)),
            Token::Add,
            Token::Num(VarValue::Int(5)),
            Token::RParen,
            Token::Mul,
            Token::Num(VarValue::Int(30)),
            Token::StmtEnd,
            // my_var = 10 + (5 * 30);
            Token::Id("my_var".into()),
            Token::Eq,
            Token::Num(VarValue::Int(10)),
            Token::Add,
            Token::LParen,
            Token::Num(VarValue::Int(5)),
            Token::RParen,
            Token::Mul,
            Token::Num(VarValue::Int(30)),
            Token::StmtEnd,
            // print("test");
            Token::Print,
            Token::LParen,
            Token::Str("test".to_string()),
            Token::RParen,
            Token::StmtEnd,
            // print("test", my_var, 10);
            Token::Print,
            Token::LParen,
            Token::Str("test".to_string()),
            Token::Comma,
            Token::Id("my_var".into()),
            Token::Comma,
            Token::Num(VarValue::Int(10)),
            Token::RParen,
            Token::StmtEnd,
            // }
            Token::RBracket,
        ];
        let token_span_vec = &token_arr.map(|t| t.into());
        let tokens = Tokens::new(token_span_vec);

        let res = program_parser(tokens);
        assert!(res.is_ok());

        let (remaining, _program) = res.unwrap();
        assert!(remaining.tok_span.is_empty());
    }
}
