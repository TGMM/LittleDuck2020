use nom::{
    branch::alt,
    bytes::complete::{escaped, tag},
    character::complete::{alpha1, alphanumeric0, alphanumeric1, digit1, multispace0, one_of},
    combinator::{map, opt, recognize},
    error::ParseError,
    multi::many0,
    number::complete::float,
    sequence::{delimited, pair, tuple},
    IResult,
};

pub fn programa_parser(input: &str) -> IResult<&str, ()> {
    let (input, _) = tag("program")(input)?;
    let (input, _) = id(input)?;
    let (input, _) = tag(";")(input)?;

    let (input, _) = many0(vars)(input)?;

    let (input, _) = bloque(input)?;

    Ok((input, ()))
}

fn vars(input: &str) -> IResult<&str, ()> {
    let (input, _) = tag("var")(input)?;
    let (input, _) = id(input)?;

    // Var Idents
    let (input, _) = many0(pair(tag(","), id))(input)?;

    let (input, _) = tag(":")(input)?;
    // Tipo: Int o Float
    let (input, _) = alt((tag("int"), tag("float")))(input)?;
    let (input, _) = tag(";")(input)?;

    Ok((input, ()))
}

fn bloque(input: &str) -> IResult<&str, ()> {
    let (input, _) = tag("{")(input)?;
    let (input, _) = many0(estatuto)(input)?;
    let (input, _) = tag("}")(input)?;

    Ok((input, ()))
}

fn estatuto(input: &str) -> IResult<&str, ()> {
    map(alt((asignacion, condicion, escritura)), |_| ())(input)
}

fn asignacion(input: &str) -> IResult<&str, ()> {
    let (input, _) = id(input)?;
    let (input, _) = tag("=")(input)?;
    let (input, _) = expresion(input)?;
    let (input, _) = tag(";")(input)?;

    Ok((input, ()))
}
fn condicion(input: &str) -> IResult<&str, ()> {
    let (input, _) = tag("if")(input)?;
    let (input, _) = tag("(")(input)?;
    let (input, _) = expresion(input)?;
    let (input, _) = tag(")")(input)?;
    let (input, _) = bloque(input)?;

    let (input, _) = opt(pair(tag("else"), bloque))(input)?;

    let (input, _) = tag(";")(input)?;

    Ok((input, ()))
}
fn escritura(input: &str) -> IResult<&str, ()> {
    let (input, _) = tag("print")(input)?;
    let (input, _) = tag("(")(input)?;
    let mut printable = alt((expresion, string));
    let (input, _) = printable(input)?;
    let (input, _) = many0(pair(tag(","), printable))(input)?;
    let (input, _) = tag(")")(input)?;
    let (input, _) = tag(";")(input)?;

    Ok((input, ()))
}

fn exp(input: &str) -> IResult<&str, ()> {
    let (input, _) = termino(input)?;
    let add_or_sub = alt((tag("+"), tag("-")));
    let (input, _) = opt(pair(add_or_sub, termino))(input)?;

    Ok((input, ()))
}

fn termino(input: &str) -> IResult<&str, ()> {
    let (input, _) = factor(input)?;
    let mul_or_div = alt((tag("*"), tag("/")));
    let (input, _) = many0(pair(mul_or_div, factor))(input)?;

    Ok((input, ()))
}

fn factor(input: &str) -> IResult<&str, ()> {
    let paren = map(tuple((tag("("), expresion, tag(")"))), |_| ());

    let add_or_sub = alt((tag("+"), tag("-")));
    let signed_const = map(pair(opt(add_or_sub), const_val), |_| ());

    let (input, _) = alt((paren, signed_const))(input)?;
    Ok((input, ()))
}

fn expresion(input: &str) -> IResult<&str, ()> {
    let (input, _) = exp(input)?;
    let comparators = alt((tag("<>"), tag(">"), tag("<")));
    let (input, _) = opt(pair(comparators, exp))(input)?;

    Ok((input, ()))
}

fn const_val(input: &str) -> IResult<&str, ()> {
    let digit1_empty = map(digit1, |_| ());
    let float_empty = map(float, |_| ());
    let (input, _) = alt((id, digit1_empty, float_empty))(input)?;

    Ok((input, ()))
}

fn id(input: &str) -> IResult<&str, ()> {
    map(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |_| (),
    )(input)
}

fn string(input: &str) -> IResult<&str, ()> {
    map(
        delimited(
            tag("\""),
            escaped(alphanumeric0, '\\', one_of("\"\\")),
            tag("\""),
        ),
        |_| (),
    )(input)
}

/// Removes whitespace from parser
pub fn ws<'a, F, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: FnMut(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}
