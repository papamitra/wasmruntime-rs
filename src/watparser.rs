
use nom::{
    IResult,
    bytes::complete::{tag, is_not},
    character::complete::{multispace0, multispace1, alphanumeric1},
    sequence::{delimited, pair, tuple},
    multi::{many0, many1, fold_many1},
    branch::alt,
    combinator::{recognize, opt, map, value},
    error::ParseError,
};

struct Module;

#[derive(Debug, PartialEq, Eq)]
struct Param {
    valtype: ValType,
    id: Option<String>,
}

struct Type {
    id: Option<String>,
    functype: FuncType,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum ValType {
    I32,
    I64,
    F32,
    F64
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct FuncType {
    param_types: Vec<ValType>,
    result_types: Vec<ValType>,
}


fn line_comment(input: &str)  -> IResult<&str, &str> {
    delimited(tag(";;"), is_not("\n"), tag("\n"))(input)
}

fn block_comment(input: &str) -> IResult<&str, &str> {
    delimited(tag("(;"), is_not(";)"), tag(";)"))(input)
}

fn ws(input: &str) -> IResult<&str, ()> {
    value((),
          many0(alt((multispace1, block_comment, line_comment))))(input)
}

fn functype(input: &str) -> IResult<&str, FuncType> {
    let (input, _) = tuple((ws, tag("("), ws, tag("func")))(input)?;

    let (input, opt_param) = opt(param)(input)?;
    let param_types = opt_param.unwrap_or(Vec::new());

    let (input, opt_result) = opt(result)(input)?;
    let result_types = opt_result.unwrap_or(Vec::new());

    let (input, _) = tuple((ws, tag(")")))(input)?;

    Ok((input, FuncType{param_types, result_types}))
}

// TODO: parse identified params
fn param(input: &str) -> IResult<&str, Vec<ValType>> {
    let (input, _) = tuple((ws, tag("("), ws, tag("param"), ws))(input)?;

    let (input, valtypes) = fold_many1(tuple((ws, valtype)),
               Vec::new(),
               |mut acc: Vec<_>, (_, valt)| {
                   acc.push(valt);
                   acc
               })(input)?;

    let (input, _) = tuple((ws, tag(")")))(input)?;

    Ok((input, valtypes))
}

// TODO: parse identified result
fn result(input: &str) -> IResult<&str, Vec<ValType>> {
    let (input, _) = tuple((ws, tag("("), ws, tag("result"), ws))(input)?;

    let (input, valtypes) = fold_many1(tuple((ws, valtype)),
               Vec::new(),
               |mut acc: Vec<_>, (_, valt)| {
                   acc.push(valt);
                   acc
               })(input)?;

    let (input, _) = tuple((ws, tag(")")))(input)?;

    Ok((input, valtypes))
}

fn valtype(input: &str) -> IResult<&str, ValType> {
    let (input, _) = ws(input)?;
    map(alt((tag("i32"), tag("i64"), tag("f32"), tag("f64"))), |s: &str| {
        match s {
            "i32" => ValType::I32,
            "i64" => ValType::I64,
            "f32" => ValType::F32,
            "f64" => ValType::F64,
            _ => panic!("valtype"),
        }
    })(input)
}

fn id(input: &str) -> IResult<&str, &str> {
    recognize(pair(tag("$"), many1(alt((alphanumeric1, 
                                       tag("!"), tag("#"),tag("$"),
                                       tag("%"), tag("&"), tag("'"),
                                       tag("*"), tag("+"), tag("-"),
                                       tag("."), tag("/"), tag(":"),
                                       tag("<"), tag("="), tag(">"),
                                       alt((tag("?"), tag("@"), tag("\\"),
                                       tag("^"), tag("_"), tag("`"),
                                       tag("|"), tag("~"))))))))(input)
}

fn module(input: &str) -> IResult<&str, Module> {
    let (input, _) = tuple((ws, tag("("), ws, tag("module"), ws))(input)?;


    Ok((input, Module))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_consume_block_comment() {
        assert_eq!(
            block_comment("(;comment;)"),
            Ok(("", "comment"))
        );
    }

    #[test]
    fn should_consume_line_comment() {
        assert_eq!(
            line_comment(";;comment\n"),
            Ok(("", "comment"))
        );
    }

    #[test]
    fn shold_consume_id() {
        assert_eq!(
            id("$!#$%&'*+-./:<=>?@\\^_`|~"),
            Ok(("", "$!#$%&'*+-./:<=>?@\\^_`|~"))
        );
    }

    #[test]
    fn should_consume_param() {
        assert_eq!(
            param(" (param i32)"),
                  Ok(("", vec![ValType::I32]))
        );
    }

    #[test]
    fn should_consume_functype() {
        assert_eq!(
            functype("(func (param i32 i64) (result f32 f64))"),
            Ok(("", FuncType{param_types: vec![ValType::I32, ValType::I64],
                             result_types: vec![ValType::F32, ValType::F64]}))
        );
    }
}
