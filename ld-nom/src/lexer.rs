use crate::ast::{Token, VarValue, Id};
use nom::{
    branch::alt,
    bytes::complete::{escaped, tag},
    character::complete::{alpha1, alphanumeric0, alphanumeric1, digit1, multispace0, one_of},
    combinator::{map, opt, recognize},
    error::{Error, ParseError},
    multi::many0,
    number::complete::{float, recognize_float},
    sequence::{delimited, pair},
    Err, IResult,
};

fn ws<'a, F, O, E: ParseError<&'a str>>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

#[inline]
fn keyword_parser<'s>(
    keyword: &'s str,
    tok: Token,
) -> impl FnMut(&'s str) -> Result<(&'s str, Token), Err<Error<&'s str>>> {
    map(tag(keyword), move |_: &str| tok.clone())
}

fn keywords_parser(input: &str) -> IResult<&str, Token> {
    // Keywords
    let program = keyword_parser("program", Token::Program);
    let var = keyword_parser("var", Token::Var);
    let int = keyword_parser("int", Token::Int);
    let float = keyword_parser("float", Token::Float);
    let print = keyword_parser("print", Token::Print);
    let kw_if = keyword_parser("if", Token::If);
    let kw_else = keyword_parser("else", Token::Else);

    let kw = alt((program, var, int, float, print, kw_if, kw_else));

    // Operators
    let lparen = keyword_parser("(", Token::LParen);
    let rparen = keyword_parser(")", Token::RParen);
    let lbracket = keyword_parser("{", Token::LBracket);
    let rbracket = keyword_parser("}", Token::RBracket);
    let equal = keyword_parser("=", Token::Eq);
    let stmt_end = keyword_parser(";", Token::StmtEnd);
    let type_sep = keyword_parser(":", Token::TypeSep);
    let gt = keyword_parser(">", Token::Gt);
    let lt = keyword_parser("<", Token::Lt);
    let lt_gt = keyword_parser("<>", Token::LtGt);
    let comma = keyword_parser(",", Token::Comma);
    let add = keyword_parser("+", Token::Add);
    let sub = keyword_parser("-", Token::Sub);
    let div = keyword_parser("/", Token::Div);
    let mul = keyword_parser("*", Token::Mul);

    let op = alt((
        lparen, rparen, lbracket, rbracket, equal, stmt_end, type_sep, gt, lt, lt_gt, comma, add,
        sub, div, mul,
    ));

    alt((kw, op))(input)
}

fn id(input: &str) -> IResult<&str, Token> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |s: &str| Token::Id(Id(s.to_owned())),
    )(input)
}

/// TODO: This should parse both ints and floats as opposed
/// to only floats
fn number(input: &str) -> IResult<&str, Token> {
    let (remaining, num) = recognize_float(input)?;
    let num_res: Result<_, Err<nom::error::Error<&str>>> =
        pair(opt(alt((tag("+"), tag("-")))), digit1)(num);
    dbg!(&num_res);
    if let Ok((int_remaining, (sign, num_str))) = num_res 
    && int_remaining.is_empty()
    && let Ok(mut num) = num_str.parse::<i32>() {
        if sign == Some("-") {
            num = 0 - num;
        }

        return Ok((remaining, Token::Num(VarValue::Int(num))));
    }

    map(float, |f| Token::Num(VarValue::Float(f)))(input)
}

fn string(input: &str) -> IResult<&str, Token> {
    let parse_escaped_str = delimited(
        tag("\""),
        escaped(alphanumeric0, '\\', one_of("\"\\")),
        tag("\""),
    );
    let mut str_to_tok = map(parse_escaped_str, |f: &str| Token::Str(f.to_owned()));
    let (remaining, tok) = str_to_tok(input)?;

    Ok((remaining, tok))
}

pub fn token_parser(input: &str) -> IResult<&str, Vec<Token>> {
    many0(ws(alt((keywords_parser, number, string, id))))(input)
}

#[cfg(test)]
mod test {
    use super::token_parser;

    #[test]
    fn lexer_works_correctly() {
        let input = r#"
        program my_program;
        var my_var: int;
        var my_other_var, my_other_other_var: float;
        {
            my_var = 10.0;
            my_var = 10 > 10;
            my_var = 10 < 10;
            my_var = 10 <> 10;
            my_var = 10 + 5 * 30;
            my_var = (10 + 5) * 30;
            my_var = 10 + (5 * 30);
            
            print("test");
            print("test", my_var, 10);
        }
        "#;

        let res = token_parser(input);
        dbg!(&res);
        assert!(res.is_ok());
    }
}
