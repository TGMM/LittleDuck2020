use std::{
    collections::{HashMap, HashSet},
    sync::LazyLock,
};

use crate::{
    ast::{Token, VarValue},
    parse_string::parse_string,
    token_span::{StrSpan, TokenSpan},
};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, digit1, multispace0, one_of},
    combinator::{map, opt, recognize},
    error::{Error, ErrorKind, ParseError},
    multi::many0,
    number::complete::{float, recognize_float},
    sequence::{delimited, pair},
    Err, IResult,
};

fn ws<'a, F, O, E: ParseError<StrSpan<'a>>>(
    inner: F,
) -> impl FnMut(StrSpan<'a>) -> IResult<StrSpan<'a>, O, E>
where
    F: FnMut(StrSpan<'a>) -> IResult<StrSpan<'a>, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

static KEYWORD_DICT: LazyLock<HashMap<&'static str, Token>> = LazyLock::new(|| {
    let mut hm = HashMap::new();
    // Keywords
    hm.insert("program", Token::Program);
    hm.insert("var", Token::Var);
    hm.insert("int", Token::Int);
    hm.insert("float", Token::Float);
    hm.insert("print", Token::Print);
    hm.insert("if", Token::If);
    hm.insert("else", Token::Else);
    hm
});

static OPERATOR_DICT: LazyLock<HashMap<&'static str, Token>> = LazyLock::new(|| {
    let mut hm = HashMap::new();
    // Operators
    hm.insert("(", Token::LParen);
    hm.insert(")", Token::RParen);
    hm.insert("{", Token::LBracket);
    hm.insert("}", Token::RBracket);
    hm.insert("=", Token::Eq);
    hm.insert(";", Token::StmtEnd);
    hm.insert(":", Token::TypeSep);
    hm.insert(">", Token::Gt);
    hm.insert("<", Token::Lt);
    hm.insert("<>", Token::LtGt);
    hm.insert(",", Token::Comma);
    hm.insert("+", Token::Add);
    hm.insert("-", Token::Sub);
    hm.insert("/", Token::Div);
    hm.insert("*", Token::Mul);
    hm
});

static OPERATOR_CHARS: LazyLock<String> = LazyLock::new(|| {
    let mut hs: HashSet<char> = HashSet::new();
    for op in OPERATOR_DICT.keys() {
        hs.extend(op.chars());
    }

    hs.into_iter().collect()
});

/// Parser identifiers and keywords
fn id_kw_parser(input: StrSpan) -> IResult<StrSpan, TokenSpan> {
    let (remaining, id) = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)?;

    let id_str: &str = &id;
    let id_tok = KEYWORD_DICT
        .get(id_str)
        .map(|token| token.clone())
        .unwrap_or(Token::Id(id_str.into()));

    Ok((
        remaining,
        TokenSpan {
            position: id,
            token: id_tok,
        },
    ))
}

fn operator_parser(input: StrSpan) -> IResult<StrSpan, TokenSpan> {
    // This will break in case another double (or more) digit operator is added
    let (remaining, op_span) =
        recognize(pair(one_of(OPERATOR_CHARS.as_str()), opt(tag(">"))))(input)?;
    let op_str: &str = &op_span;
    let op = OPERATOR_DICT
        .get(op_str)
        .ok_or_else(|| Err::Error(Error::from_error_kind(remaining, ErrorKind::OneOf)))?;

    Ok((
        remaining,
        TokenSpan {
            position: op_span,
            token: op.clone(),
        },
    ))
}

fn number_parser(input: StrSpan) -> IResult<StrSpan, TokenSpan> {
    let (remaining, num_span) = recognize_float(input)?;

    let num_res: Result<_, Err<nom::error::Error<StrSpan>>> =
        pair(opt(alt((tag("+"), tag("-")))), digit1)(num_span);
    if let Ok((int_remaining, (sign, num_str))) = num_res
    && int_remaining.is_empty()
    && let Ok(mut num) = num_str.parse::<i32>() {
        if sign == Some("-".into()) {
            num = 0 - num;
        }

        return Ok((remaining, TokenSpan {
            position: num_span,
            token: Token::Num(VarValue::Int(num))
        }));
    }

    map(float, move |f| TokenSpan {
        position: num_span,
        token: Token::Num(VarValue::Float(f)),
    })(input)
}

fn string_parser(input: StrSpan) -> IResult<StrSpan, TokenSpan> {
    let mut string_parser = map(parse_string, |s| Token::Str(s));
    let (remaining, string_tok) = string_parser(input)?;
    let (_, str_span) = recognize(string_parser)(input)?;

    Ok((
        remaining,
        TokenSpan {
            position: str_span,
            token: string_tok,
        },
    ))
}

pub fn token_parser(input: &str) -> IResult<StrSpan, Vec<TokenSpan>> {
    let input = StrSpan::new(input);

    many0(ws(alt((
        number_parser,
        string_parser,
        id_kw_parser,
        operator_parser,
    ))))(input)
}

#[cfg(test)]
mod test {
    use nom::multi::{many0, many1};

    use super::{string_parser, token_parser};
    use crate::{
        ast::{Token, VarValue},
        lexer::{id_kw_parser, number_parser, operator_parser, ws, StrSpan},
    };

    #[test]
    fn keywords_parser_test() {
        let input_str = "program var int float print if else";
        let input = StrSpan::new(input_str);

        let res = many1(ws(id_kw_parser))(input);
        assert!(res.is_ok());

        let (remaining, token_spans) = res.unwrap();
        dbg!(remaining, &token_spans);
        assert!(remaining.is_empty());

        let tokens: Vec<Token> = token_spans.into_iter().map(|ts| ts.into()).collect();
        assert_eq!(
            tokens,
            vec![
                Token::Program,
                Token::Var,
                Token::Int,
                Token::Float,
                Token::Print,
                Token::If,
                Token::Else,
            ]
        );
    }

    #[test]
    fn operator_parser_test() {
        let input_str = "( ) { } = ; : > < <> , + - / *";
        let input = StrSpan::new(input_str);

        let res = many1(ws(operator_parser))(input);
        assert!(res.is_ok());

        let (remaining, token_spans) = res.unwrap();
        dbg!(remaining, &token_spans);
        assert!(remaining.is_empty());

        let tokens: Vec<Token> = token_spans.into_iter().map(|ts| ts.into()).collect();
        assert_eq!(
            tokens,
            vec![
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
            ]
        );
    }

    #[test]
    fn number_parser_test() {
        let input_str = "1 2.0 3.1 4.234 5 123456789";
        let input = StrSpan::new(input_str);
        let res = many1(ws(number_parser))(input);
        assert!(res.is_ok());

        let (remaining, token_spans) = res.unwrap();
        dbg!(remaining, &token_spans);
        assert!(remaining.is_empty());

        let tokens: Vec<Token> = token_spans.into_iter().map(|ts| ts.into()).collect();
        assert_eq!(
            tokens,
            vec![
                Token::Num(VarValue::Int(1)),
                Token::Num(VarValue::Float(2.0)),
                Token::Num(VarValue::Float(3.1)),
                Token::Num(VarValue::Float(4.234)),
                Token::Num(VarValue::Int(5)),
                Token::Num(VarValue::Int(123456789)),
            ]
        );
    }

    #[test]
    fn string_parser_test() {
        let input_str = r#""This is a test string\n" "This is another test string\t""#;
        let input = StrSpan::new(input_str);
        let res = many0(ws(string_parser))(input);
        dbg!(&res);
        assert!(res.is_ok());

        let (remaining, str_token_spans) = res.unwrap();
        dbg!(remaining, &str_token_spans);
        assert!(remaining.is_empty());

        let str_tokens: Vec<Token> = str_token_spans.into_iter().map(|ts| ts.into()).collect();
        assert_eq!(
            str_tokens,
            vec![
                Token::Str("This is a test string\n".to_string()),
                Token::Str("This is another test string\t".to_string())
            ]
        );
    }

    #[test]
    fn id_parser_test() {
        let input_str = "x y z my_var my_super_long_var_name";
        let input = StrSpan::new(input_str);
        let res = many1(ws(id_kw_parser))(input);
        assert!(res.is_ok());

        let (remaining, token_spans) = res.unwrap();
        dbg!(remaining, &token_spans);
        assert!(remaining.is_empty());

        let tokens: Vec<Token> = token_spans.into_iter().map(|ts| ts.into()).collect();
        assert_eq!(
            tokens,
            vec![
                Token::Id("x".into()),
                Token::Id("y".into()),
                Token::Id("z".into()),
                Token::Id("my_var".into()),
                Token::Id("my_super_long_var_name".into()),
            ]
        );
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

        let (remaining, token_spans) = res.unwrap();
        dbg!(remaining);
        assert!(remaining.is_empty());

        let tokens: Vec<Token> = token_spans.into_iter().map(|ts| ts.into()).collect();
        assert_eq!(
            tokens,
            vec![
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
            ]
        );
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

        let (remaining, token_spans) = res.unwrap();
        dbg!(remaining);
        assert!(remaining.is_empty());

        let tokens: Vec<Token> = token_spans.into_iter().map(|ts| ts.into()).collect();
        assert_eq!(tokens, {
            use Token::*;
            vec![
                // program my_program;
                Program,
                Id("my_program".into()),
                StmtEnd,
                // var my_var: int;
                Var,
                Id("my_var".into()),
                TypeSep,
                Int,
                StmtEnd,
                // var my_other_var, my_other_other_var: float;
                Var,
                Id("my_other_var".into()),
                Comma,
                Id("my_other_other_var".into()),
                TypeSep,
                Float,
                StmtEnd,
                // {
                LBracket,
                // my_var = 10.0;
                Id("my_var".into()),
                Eq,
                Num(VarValue::Float(10.0)),
                StmtEnd,
                // my_var = 10 > 10;
                Id("my_var".into()),
                Eq,
                Num(VarValue::Int(10)),
                Gt,
                Num(VarValue::Int(10)),
                StmtEnd,
                // my_var = 10 < 10;
                Id("my_var".into()),
                Eq,
                Num(VarValue::Int(10)),
                Lt,
                Num(VarValue::Int(10)),
                StmtEnd,
                // my_var = 10 <> 10;
                Id("my_var".into()),
                Eq,
                Num(VarValue::Int(10)),
                LtGt,
                Num(VarValue::Int(10)),
                StmtEnd,
                // my_var = 10 + 5 * 30;
                Id("my_var".into()),
                Eq,
                Num(VarValue::Int(10)),
                Add,
                Num(VarValue::Int(5)),
                Mul,
                Num(VarValue::Int(30)),
                StmtEnd,
                // my_var = (10 + 5) * 30;
                Id("my_var".into()),
                Eq,
                LParen,
                Num(VarValue::Int(10)),
                Add,
                Num(VarValue::Int(5)),
                RParen,
                Mul,
                Num(VarValue::Int(30)),
                StmtEnd,
                // my_var = 10 + (5 * 30);
                Id("my_var".into()),
                Eq,
                Num(VarValue::Int(10)),
                Add,
                LParen,
                Num(VarValue::Int(5)),
                Mul,
                Num(VarValue::Int(30)),
                RParen,
                StmtEnd,
                // print("test");
                Print,
                LParen,
                Str("test".to_string()),
                RParen,
                StmtEnd,
                // print("test", my_var, 10);
                Print,
                LParen,
                Str("test".to_string()),
                Comma,
                Id("my_var".into()),
                Comma,
                Num(VarValue::Int(10)),
                RParen,
                StmtEnd,
                RBracket,
            ]
        });
    }
}
