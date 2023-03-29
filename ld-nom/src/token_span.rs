use nom_locate::LocatedSpan;

use crate::ast::Token;

pub type StrSpan<'a> = LocatedSpan<&'a str>;
#[derive(Debug, PartialEq)]
pub struct TokenSpan<'a> {
    pub position: StrSpan<'a>,
    pub token: Token,
}

impl From<TokenSpan<'_>> for Token {
    fn from(value: TokenSpan) -> Self {
        value.token
    }
}

#[cfg(test)]
impl From<Token> for TokenSpan<'_> {
    fn from(value: Token) -> Self {
        TokenSpan {
            position: StrSpan::new(""),
            token: value,
        }
    }
}
