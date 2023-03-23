use crate::ast::Token;
use crate::token::Tokens;
use nom::bytes::complete::take;
use nom::combinator::verify;
use nom::IResult;

macro_rules! tag_token (
    ($func_name:ident, $tag: expr) => (
        fn $func_name(tokens: Tokens) -> IResult<Tokens, Tokens> {
            verify(take(1usize), |t: &Tokens| t.tok[0] == $tag)(tokens)
        }
    )
  );

tag_token!(var_tag, Token::Var);
