use crate::{ast::{Token, VarValue, Id}, parse_string::parse_string};
use nom::{
    branch::alt,
    bytes::complete::{tag},
    character::complete::{alpha1, alphanumeric1, digit1, multispace0},
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
        lparen, rparen, lbracket, rbracket, equal, stmt_end, type_sep, lt_gt, gt, lt, comma, add,
        sub, div, mul,
    ));

    alt((kw, op))(input)
}

fn id_parser(input: &str) -> IResult<&str, Token> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |s: &str| Token::Id(Id(s.to_owned())),
    )(input)
}

fn number_parser(input: &str) -> IResult<&str, Token> {
    let (remaining, num) = recognize_float(input)?;
    let num_res: Result<_, Err<nom::error::Error<&str>>> =
        pair(opt(alt((tag("+"), tag("-")))), digit1)(num);
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

fn string_parser(input: &str) -> IResult<&str, Token> {
    map(parse_string, |f: String| Token::Str(f.to_owned()))(input)
}

pub fn token_parser(input: &str) -> IResult<&str, Vec<Token>> {
    many0(ws(alt((keywords_parser, number_parser, string_parser, id_parser))))(input)
}

#[cfg(test)]
mod test {
    use nom::multi::{many1, many0};

    use crate::{ast::{Token, VarValue}, lexer::{ws, number_parser, id_parser}};
    use super::{token_parser, string_parser, keywords_parser};

    #[test]
    fn keywords_parser_test() {
        let input = "program var int float print if else ( ) { } = ; : > < <> , + - / *";
        
        let res = many1(ws(keywords_parser))(input);
        assert!(res.is_ok());

        let (remaining, tokens) = res.unwrap();
        dbg!(remaining, &tokens);
        assert!(remaining.is_empty());
        
        assert_eq!(tokens, vec![
            Token::Program,
            Token::Var,
            Token::Int,
            Token::Float,
            Token::Print,
            Token::If,
            Token::Else,
            Token::LParen,
            Token::RParen,
            Token::LBracket,
            Token::RBracket,
            Token::Eq,
            Token::StmtEnd,
            Token::TypeSep,
            Token::Gt,
            Token::Lt,
            Token::LtGt,
            Token::Comma,
            Token::Add,
            Token::Sub,
            Token::Div,
            Token::Mul
        ]);
    }

    #[test]
    fn number_parser_test() {
        let input = "1 2.0 3.1 4.234 5 123456789";
        let res = many1(ws(number_parser))(input);
        assert!(res.is_ok());

        let (remaining, tokens) = res.unwrap();
        dbg!(remaining, &tokens);
        assert!(remaining.is_empty());

        assert_eq!(tokens, vec![
            Token::Num(VarValue::Int(1)),
            Token::Num(VarValue::Float(2.0)),
            Token::Num(VarValue::Float(3.1)),
            Token::Num(VarValue::Float(4.234)),
            Token::Num(VarValue::Int(5)),
            Token::Num(VarValue::Int(123456789)),
        ]);
    }

    #[test]
    fn string_parser_test() {
        let input = r#""This is a test string\n" "This is another test string\t""#;
        let res = many0(ws(string_parser))(input);
        dbg!(&res);
        assert!(res.is_ok());

        let (remaining, str_tokens) = res.unwrap();
        dbg!(remaining, &str_tokens);
        assert!(remaining.is_empty());

        assert_eq!(str_tokens, vec![
            Token::Str("This is a test string\n".to_string()),
            Token::Str("This is another test string\t".to_string())
        ]);
    }

    #[test]
    fn id_parser_test() {
        let input = "x y z my_var my_super_long_var_name";
        let res = many1(ws(id_parser))(input);
        assert!(res.is_ok());

        let (remaining, tokens) = res.unwrap();
        dbg!(remaining, &tokens);
        assert!(remaining.is_empty());

        assert_eq!(tokens, vec![
            Token::Id("x".into()),
            Token::Id("y".into()),
            Token::Id("z".into()),
            Token::Id("my_var".into()),
            Token::Id("my_super_long_var_name".into()),
        ]);
    }

    #[test]
    fn can_parse_strings() {
        let input = r#"
        program my_program;
        {   
            print("\ntest");
        }
        "#;

        let res = token_parser(input);
        dbg!(&res);
        assert!(res.is_ok());

        let (remaining, token_vec) = res.unwrap();
        dbg!(remaining);
        assert!(remaining.is_empty());

        assert_eq!(token_vec, vec![
            Token::Program,
            Token::Id("my_program".into()),
            Token::StmtEnd,
            Token::LBracket,
            Token::Print,
            Token::LParen,
            Token::Str("\ntest".to_string()),
            Token::RParen,
            Token::StmtEnd,
            Token::RBracket,
        ]);
    }

    #[test]
    fn full_lexer_test() {
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
