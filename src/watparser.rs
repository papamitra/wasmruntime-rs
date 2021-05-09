
use nom::{
    IResult,
    bytes::complete::{tag, is_not},
    character::complete::{multispace0, multispace1, alphanumeric1, digit1, char, one_of, anychar, none_of},
    sequence::{delimited, pair, tuple, preceded, terminated},
    multi::{many0, many1, fold_many1},
    branch::alt,
    combinator::{recognize, opt, map, value},
    error::ParseError,
};

use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, PartialEq, Eq, Default)]
pub struct Module {
    ty: Vec<FuncType>,
    im: Vec<Import>,
    pub func: Vec<Func>,
    pub ta: Vec<Table>,
    me: Vec<Mem>,
    pub gl: Vec<Global>,
    ex: Vec<Export>,
    // st: Start,
    el: Vec<Elem>,
    da: Vec<Data>,
}

impl Module {
    pub fn new() -> Self {
        Module::default()
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Param {
    valtype: ValType,
    id: Option<String>,
}

#[derive(Debug, PartialEq, Eq)]
struct Type {
    id: Option<String>,
    functype: FuncType,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ValType {
    I32,
    I64,
    F32,
    F64
}

impl ValType {
    fn size(&self) -> u32 {
        match self {
            ValType::I32 => 4,
            ValType::I64 => 8,
            ValType::F32 => 4,
            ValType::F64 => 8,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct FuncType {
    index: Option<Index>,
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

fn ws_<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
  where F: Fn(&'a str) -> IResult<&'a str, O>, {
    delimited(
        many0(alt((multispace1, block_comment, line_comment))),
        inner,
        many0(alt((multispace1, block_comment, line_comment))),
    )
}

fn functype(input: &str) -> IResult<&str, FuncType> {
    let (input, _) = tuple((ws_(tag("(")), ws_(tag("type"))))(input)?;
    let (input, index) = opt(ws_(index))(input)?;

    let (input, _) = tuple((ws_(tag("(")), ws_(tag("func"))))(input)?;

    let (input, opt_param) = opt(param)(input)?;
    let param_types = opt_param.unwrap_or(Vec::new());

    let (input, opt_result) = opt(result)(input)?;
    let result_types = opt_result.unwrap_or(Vec::new());

    let (input, _) = ws_(tag(")"))(input)?;
    let (input, _) = ws_(tag(")"))(input)?;

    Ok((input, FuncType{index, param_types, result_types}))
}

// TODO: parse identified params
fn param(input: &str) -> IResult<&str, Vec<ValType>> {
    let (input, _) = pair(ws_(tag("(")), ws_(tag("param")))(input)?;

    let (input, valtypes) = fold_many1(tuple((ws, valtype)),
               Vec::new(),
               |mut acc: Vec<_>, (_, valt)| {
                   acc.push(valt);
                   acc
               })(input)?;

    let (input, _) = ws_(tag(")"))(input)?;

    Ok((input, valtypes))
}

// TODO: parse identified result
fn result(input: &str) -> IResult<&str, Vec<ValType>> {
    let (input, _) = pair(ws_(tag("(")), ws_(tag("result")))(input)?;

    let (input, valtypes) = fold_many1(tuple((ws, valtype)),
               Vec::new(),
               |mut acc: Vec<_>, (_, valt)| {
                   acc.push(valt);
                   acc
               })(input)?;

    let (input, _) = ws_(tag(")"))(input)?;

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

pub fn module(input: &str) -> IResult<&str, Module> {
    let (input, _) = tuple((ws_(tag("(")), ws_(tag("module")), ws))(input)?;

    let module = Rc::new(RefCell::new(Module::new()));

    let (input, _) = many0(alt((map(functype, |ft| { module.borrow_mut().ty.push(ft); ()}),
                                map(import, |im| { module.borrow_mut().im.push(im); ()}),
                                map(func, |func| { module.borrow_mut().func.push(func); ()}),
                                map(table, |ta| { module.borrow_mut().ta.push(ta); ()}),
                                map(mem, |me| { module.borrow_mut().me.push(me); ()}),
                                map(global, |gl| { module.borrow_mut().gl.push(gl); ()}),
                                map(export, |ex| { module.borrow_mut().ex.push(ex); ()}),
                                map(elem, |el| { module.borrow_mut().el.push(el); ()}),
                                map(data, |da| { module.borrow_mut().da.push(da); ()}),
    )))(input)?;

    let (input, _) = ws_(tag(")"))(input)?;

    Ok((input, Rc::try_unwrap(module).ok().unwrap().into_inner()))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Func {
    pub id: Option<String>,
    pub typeuse: TypeUse,
    pub locals: Vec<ValType>,
    pub body: Vec<Instr>,
}

pub(crate) fn func(input: &str) -> IResult<&str, Func> {
    let (input, _) = ws(input)?;

    let (input, (_, id, typeuse, locals, body, _)) =  tuple((
        pair(tag("("), ws_(tag("func"))),
        opt(ws_(id)),
        ws_(typeuse),
        opt(ws_(local)),
        instrs,
        ws_(tag(")")),
    ))(input)?;

    let id = id.map(|s| s.to_string());

    Ok((input, Func{id, typeuse,
                    locals: locals.unwrap_or(Vec::new()),
                    body}))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeUse {
    index: Option<Index>,
    pub params: Vec<ValType>,
    pub results: Vec<ValType>
}

fn typeuse(input: &str) -> IResult<&str, TypeUse> {
    let (input, _) = ws(input)?;

    let (input, index) = opt(delimited(pair(tag("("), ws_(tag("type"))),
                                     index,
                                     ws_(tag(")"))))(input)?;

    // TODO: verify param, result
    let (input, params) = opt(param)(input)?;
    let params = params.unwrap_or(Vec::new());
    let (input, results) = opt(result)(input)?;
    let results = results.unwrap_or(Vec::new());

    Ok((input, TypeUse{index, params, results}))
}

fn local(input: &str) -> IResult<&str, Vec<ValType>> {
    let (input, _) = ws(input)?;

    let (input, (_, locals, _)) = tuple((tuple((tag("("), ws, tag("local"))),
                                 many0(preceded(ws, valtype)),
                                 pair(ws, tag(")"))))(input)?;

    Ok((input, locals))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Index {
    Val(u32),
    Id(String),
}

fn index(input: &str) -> IResult<&str, Index> {
    let (input, _) = ws(input)?;

    alt((map(digit1, |s: &str| Index::Val(s.parse::<u32>().unwrap())),
         map(id, |s| Index::Id(s.to_string()))))(input)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Instr {
    Unreachable,
    Nop,
    Br(u32),
    BrIf(u32),
    BrTable(Vec<u32>),
    Return,
    Call(Index),
    CallIndirect(u32),
    Drop,
    Select,
    VarInstr(VarInstr),
    TableInstr(TableInstr),
    MemOpInstr(MemOpInstr),
    NumInstr(NumInstr),
    MemorySize,
    MemoryGrow,
    ConstInstr(ConstInstr),
    Block(Block),
    Loop(Block),
    // TODO: If instruction
}


fn plaininstr(input: &str) -> IResult<&str, Instr> {
    let (input, _) = ws(input)?;

    alt((value(Instr::Unreachable, tag("unreachable")),
         value(Instr::Nop, tag("nop")),
         map(preceded(pair(tag("br"), ws), digit1),
             |s| Instr::Br(s.parse::<u32>().unwrap())),
         map(preceded(tag("br_table"), many1(preceded(ws, digit1))),
             |s| Instr::BrTable(s.into_iter().map(|s| s.parse::<u32>().unwrap()).collect::<Vec<_>>())),
         value(Instr::Return, tag("return")),
         map(preceded(pair(tag("call"), ws), index), Instr::Call),
         map(preceded(pair(tag("call_indirect"), ws), digit1),
             |s| Instr::CallIndirect(s.parse::<u32>().unwrap())),
         value(Instr::Drop, tag("drop")),
         value(Instr::Select, tag("Select")),
         map(varinstr, Instr::VarInstr),
         map(tableinstr, Instr::TableInstr),
         map(memopinstr, Instr::MemOpInstr),
         map(constinstr, Instr::ConstInstr), // must be before numinstr
         map(numinstr, Instr::NumInstr),
         value(Instr::MemorySize, tag("memory.size")),
         value(Instr::MemoryGrow, tag("memory.grow")),
         ))(input)
}

fn folded_instr(input: &str) -> IResult<&str, Vec<Instr>> {
    let (input, _) = ws(input)?;

    // TODO: support folded block,loop,if instr
    map(tuple((tag("("), plaininstr, many0(folded_instr), pair(ws, tag(")")))),
        |(_, pinstr, finstr, _)| {
            let mut finstr = finstr.concat();
            finstr.push(pinstr);
            finstr
        })(input)
}


#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarInstr {
    LocalGet(Index),
    LocalSet(Index),
    LocalTee(Index),
    GlobalGet(Index),
    GlobalSet(Index),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TableInstr {
    TableGet(Index),
    TableSet(Index),
    TableSize(Index),
    TableGrow(Index),
    TableFill(Index),
    TableCopy(Index, Index),
    TableInit(Index, Index),
    ElemDrop(Index),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemOpInstr {
    ty: ValType,
    op: String,
    len: Option<u32>,
    signed: Option<bool>,
    arg: MemOpArg,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MemOpArg {
    offset: u32,
    align: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block {
    pub body: Vec<Instr>
}

fn instr(input: &str) -> IResult<&str, Instr> {
    let (input, _) = ws(input)?;

    alt((plaininstr,
         map(blockinstr, Instr::Block),
         map(loopinstr, Instr::Loop)))(input)
}

fn instrs(input: &str) -> IResult<&str, Vec<Instr>> {
    let (input, _) = ws(input)?;

    map(many0(alt((many1(instr), folded_instr))), |x| x.concat())(input)
}

fn blockinstr(input: &str) -> IResult<&str, Block> {
    let (input, _) = ws(input)?;

    let (input, body) = preceded(tag("block"), instrs)(input)?;

    let (input, _) = pair(ws, tag("end"))(input)?;

    Ok((input, Block{ body }))
}

fn loopinstr(input: &str) -> IResult<&str, Block> {
    let (input, _) = ws(input)?;

    let (input, body) = preceded(tag("loop"), instrs)(input)?;

    let (input, _) = pair(ws, tag("end"))(input)?;

    Ok((input, Block{ body }))
}

fn memopinstr(input: &str) -> IResult<&str, MemOpInstr> {
    let (input, _) = ws(input)?;

    let (input, (ty, _, op, lenstr, signedstr)) = tuple((valtype, tag("."), alt((tag("store"), tag("load"))),
                             opt(alt((tag("8"), tag("16"), tag("32")))),
                             opt(alt((tag("_u"), tag("_s"))))))(input)?;

    let op = op.to_string();

    let len = lenstr.map(|l| l.parse::<u32>().unwrap());
    let signed = signedstr.map(|s| if s == "_s" { true } else { false });

    let (input, _) = ws(input)?;
    let (input, offset) = opt(preceded(tag("offset="), digit1))(input)?;
    let offset = offset.map(|x| x.parse::<u32>().unwrap()).unwrap_or(0);

    let (input, _) = ws(input)?;
    let (input, align) = opt(preceded(tag("align="), digit1))(input)?;
    let align = align.map(|x| x.parse::<u32>().unwrap()).unwrap_or(len.unwrap_or(ty.size()) / 4);

    Ok((input, MemOpInstr{ty, op, len, signed, arg: MemOpArg{offset, align}}))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NumInstr {
    pub ty: ValType,
    pub op: String,
}

fn numinstr(input: &str) -> IResult<&str, NumInstr> {
    let (input, _) = ws(input)?;

    let (input, (ty, _, op)) = tuple((valtype, tag("."), recognize(many1(alt((alphanumeric1, tag("_")))))))(input)?;
    let op = op.to_string();

    Ok((input, NumInstr{ty, op}))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstInstr {
    I32(String),
    I64(String),
    F32(String),
    F64(String)
}

fn constinstr(input: &str) -> IResult<&str, ConstInstr> {
    let (input, _) = ws(input)?;

    alt((map(preceded(pair(tag("i32.const"), ws), decimal), |s| ConstInstr::I32(s.to_string())),
         map(preceded(pair(tag("i64.const"), ws), decimal), |s| ConstInstr::I64(s.to_string())),
         map(preceded(pair(tag("f32.const"), ws), float), |s| ConstInstr::F32(s.to_string())),
         map(preceded(pair(tag("f64.const"), ws), float), |s| ConstInstr::F64(s.to_string())),
    ))(input)
}

fn decimal(input: &str) -> IResult<&str, &str> {
  recognize(
    many1(
      terminated(one_of("0123456789"), many0(char('_')))
    )
  )(input)
}

fn float(input: &str) -> IResult<&str, &str> {
  alt((
    // Case one: .42
    recognize(
      tuple((
        char('.'),
        decimal,
        opt(tuple((
          one_of("eE"),
          opt(one_of("+-")),
          decimal
        )))
      ))
    )
    , // Case two: 42e42 and 42.42e42
    recognize(
      tuple((
        decimal,
        opt(preceded(
          char('.'),
          decimal,
        )),
        one_of("eE"),
        opt(one_of("+-")),
        decimal
      ))
    )
    , // Case three: 42. and 42.42
    recognize(
      tuple((
        decimal,
        char('.'),
        opt(decimal)
      ))
    )
  ))(input)
}

fn varinstr(input: &str) -> IResult<&str, VarInstr> {
    let (input, _) = ws(input)?;

    alt((map(preceded(tag("local.get"), preceded(ws, index)),
             |i| VarInstr::LocalGet(i)),
         map(preceded(tag("local.set"), preceded(ws, index)),
             |i| VarInstr::LocalSet(i)),
         map(preceded(tag("local.tee"), preceded(ws, index)),
             |i| VarInstr::LocalTee(i)),
         map(preceded(tag("global.get"), preceded(ws, index)),
             |i| VarInstr::GlobalGet(i)),
         map(preceded(tag("Global.set"), preceded(ws, index)),
             |i| VarInstr::GlobalSet(i)),))(input)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Table {
    pub limits: Limits
}

fn table(input: &str) -> IResult<&str, Table> {
    let (input, _) = ws(input)?;

    let (input, (_, limits, _)) = tuple((tuple((tag("("), ws, tag("table"), ws)),
                                         limits,
                                         tuple((ws, tag("funcref"), ws, tag(")")))))(input)?;

    Ok((input, Table{limits}))
}

fn tableinstr(input: &str) -> IResult<&str, TableInstr> {
    let (input, _) = ws(input)?;

    alt((map(preceded(tag("table.get"), ws_(index)),
             |i| TableInstr::TableGet(i)),
         map(preceded(tag("table.set"), ws_(index)),
             |i| TableInstr::TableSet(i)),
         map(preceded(tag("table.size"), ws_(index)),
             |i| TableInstr::TableSize(i)),
         map(preceded(tag("table.grow"), ws_(index)),
             |i| TableInstr::TableGrow(i)),
         map(preceded(tag("table.fill"), ws_(index)),
             |i| TableInstr::TableFill(i)),
         map(preceded(tag("table.copy"), tuple((ws_(index), ws_(index)))),
             |(i, j)| TableInstr::TableCopy(i, j)),
         map(preceded(tag("table.init"), tuple((ws_(index), ws_(index)))),
             |(i, j)| TableInstr::TableInit(i, j)),
         map(preceded(tag("elem.drop"), ws_(index)),
             |i| TableInstr::ElemDrop(i))))(input)
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Mem {
    limits: Limits
}

fn mem(input: &str) -> IResult<&str, Mem> {
    let (input, _) = ws(input)?;

    let (input, (_, limits, _)) = tuple((tuple((tag("("), ws, tag("memory"), ws)),
                                         limits,
                                         tuple((ws, tag(")")))))(input)?;

    Ok((input, Mem{limits}))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Limits {
    pub min: u32,
    pub max: Option<u32>
}

fn limits(input: &str) -> IResult<&str, Limits> {
    let (input, _) = ws(input)?;

    let (input, (min, _, max)) = tuple((map(digit1, |s: &str| s.parse::<u32>().unwrap()),
                                        ws,
                                        opt(map(digit1, |s: &str| s.parse::<u32>().unwrap()))))(input)?;

    Ok((input, Limits{min, max}))
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GlobalType {
    Const(ValType),
    Var(ValType)
}

fn globaltype(input: &str) -> IResult<&str, GlobalType> {
    let (input, _) = ws(input)?;

    alt((map(valtype, GlobalType::Const),
         map(preceded(tuple((tag("("), ws, tag("mut"), ws)),
                      terminated(valtype, pair(ws, tag(")")))), GlobalType::Var)))(input)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Global {
    pub gt: GlobalType,
    pub init: Vec<Instr>
}

pub fn global(input: &str) -> IResult<&str, Global> {

    // TODO: support Abbreviations
    let (input, (_, _, gt, init, _)) =
        tuple((ws_(tag("(")), ws_(tag("global")), globaltype, expr, ws_(tag(")"))))(input)?;

    Ok((input, Global{gt, init}))
}

fn expr(input: &str) -> IResult<&str, Vec<Instr>> {
    instrs(input)
}

#[derive(Debug, PartialEq, Eq)]
struct Export {
    name: String,
    desc: ExportDesc
}

#[derive(Debug, PartialEq, Eq)]
enum ExportDesc {
    Func(Index),
    Table(Index),
    Mem(Index),
    Global(Index),
}

fn export(input: &str) -> IResult<&str, Export> {
    let (input, (_, _, name, desc, _)) =
        tuple((ws_(tag("(")), ws_(tag("export")), string, exportdesc, ws_(tag(")"))))(input)?;

    Ok((input, Export{name, desc}))
}

fn string(input: &str) -> IResult<&str, String> {

    let (input, _) = ws(input)?;

    map(delimited(tag("\""),
                  many0(alt((escapeseq, none_of("\"\\")))),
                  tag("\"")), |s| s.into_iter().collect::<String>())(input)
}

fn escapeseq(input: &str) -> IResult<&str, char> {
    let hexdigit = |s| one_of("0123456789abcdefABCDEF")(s);

    alt((value('\t', tag("\\t")),
         value('\n', tag("\\n")),
         value('\r', tag("\\r")),
         value('"', tag("\\\"")),
         value('\'', tag("\\'")),
         value('\\', tag("\\\\")),
         map(delimited(tag("\\u{"),
                       recognize(
                           many1(
                               terminated(hexdigit.clone(), many0(char('_')))
                           )),
                       tag("}")),
             |out: &str| std::char::from_u32(u32::from_str_radix(&str::replace(&out, "_", ""), 16).unwrap()).unwrap(),
         ),
         map(preceded(tag("\\"), recognize(pair(hexdigit.clone(), hexdigit.clone()))),
             |out: &str| unsafe {std::char::from_u32_unchecked(u32::from_str_radix(&out, 16).unwrap())} )
    ))(input)
}

fn exportdesc(input: &str) -> IResult<&str, ExportDesc> {
    let (input, _) = ws_(tag("("))(input)?;

    let (input, desc) = alt((map(preceded(ws_(tag("func")), index), ExportDesc::Func),
                             map(preceded(ws_(tag("table")), index), ExportDesc::Table),
                             map(preceded(ws_(tag("memory")), index), ExportDesc::Mem),
                             map(preceded(ws_(tag("global")), index), ExportDesc::Global)
                             ))(input)?;

    let (input, _) = ws_(tag(")"))(input)?;

    Ok((input, desc))
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Elem {
    table: Index,
    offset: Vec<Instr>,
    init: Vec<Index>,
}

fn elem(input: &str) -> IResult<&str, Elem> {
    let (input, (_,_, offset, _, init, _)) =
        tuple((ws_(tag("(")), ws_(tag("elem")),
               offset_expr, opt(ws_(tag("func"))), many0(index),
               ws_(tag(")"))))(input)?;

    Ok(("", Elem{table: Index::Val(0), offset, init}))
}

fn offset_expr(input: &str) -> IResult<&str, Vec<Instr>> {
    alt((delimited(pair(ws_(tag("(")), ws_(tag("offset"))),
                   expr, ws_(tag(")"))),
         folded_instr,
         map(instr, |i| vec![i])))(input)
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Import {
    module: String,
    name: String,
    desc: ImportDesc,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ImportDesc {
    Func(Option<Index>, TypeUse),
    Table(Index),
    Mem(Index),
    Global(Index),
}

fn import(input: &str) -> IResult<&str, Import> {
    let (input, (_, _, module, name, desc, _)) =
        tuple((ws_(tag("(")), ws_(tag("import")), string, string, importdesc, ws_(tag(")"))))(input)?;

    Ok((input, Import{module, name, desc}))
}

fn importdesc(input: &str) -> IResult<&str, ImportDesc> {
    let (input, _) = ws_(tag("("))(input)?;

    // TODO: support table, memory, global IDs
    let (input, desc) = alt((map(preceded(ws_(tag("func")), pair(opt(index), typeuse)),
                                 |(id, typeuse)| ImportDesc::Func(id, typeuse)),
                             map(preceded(ws_(tag("table")), index), ImportDesc::Table),
                             map(preceded(ws_(tag("memory")), index), ImportDesc::Mem),
                             map(preceded(ws_(tag("global")), index), ImportDesc::Global)
                             ))(input)?;

    let (input, _) = ws_(tag(")"))(input)?;

    Ok((input, desc))
}

#[derive(Debug, PartialEq, Eq)]
struct Data {
    idx: Option<Index>,
    offset: Vec<Instr>,
    init: String,
}

fn data(input: &str) -> IResult<&str, Data> {
    let (input, (_, idx, offset, init, _)) = tuple((pair(ws_(tag("(")), ws_(tag("data"))),
                             opt(index),
                             offset_expr,
                             string,
                             ws_(tag(")"))))(input)?;

    let init = init.to_string();

    Ok((input, Data{idx, offset, init}))
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
            functype("(type (func (param i32 i64) (result f32 f64)))"),
            Ok(("", FuncType{index: None,
                             param_types: vec![ValType::I32, ValType::I64],
                             result_types: vec![ValType::F32, ValType::F64]}))
        );
    }

    #[test]
    fn should_consume_meminstr() {
        assert_eq!(
            memopinstr("i32.load8_u offset=5 align=16"),
            Ok(("", MemOpInstr{ty: ValType::I32,
                             op: "load".to_owned(),
                             len: Some(8),
                             signed: Some(false),
                             arg: MemOpArg{ offset: 5, align: 16}}))
        );
    }

    #[test]
    fn should_consume_plaininstr() {
        assert_eq!(
            plaininstr("unreachable"),
            Ok(("", Instr::Unreachable))
        );

        assert_eq!(
            plaininstr("nop"),
            Ok(("", Instr::Nop))
        );

        assert_eq!(
            plaininstr("br 3"),
            Ok(("", Instr::Br(3)))
        );

        assert_eq!(
            plaininstr("br_table 1 2 3"),
            Ok(("", Instr::BrTable(vec![1,2,3])))
        );

        assert_eq!(
            plaininstr("call $__wasm_call_ctors"),
            Ok(("", Instr::Call(Index::Id("$__wasm_call_ctors".to_owned()))))
        );
    }

    #[test]
    fn should_consume_typeuse() {
        assert_eq!(
            typeuse("(type 0)"),
            Ok(("", TypeUse{index: Some(Index::Val(0)), params: vec![], results: vec![]}))
        );
    }

    #[test]
    fn should_consume_block() {
        assert_eq!(
            blockinstr("block  ;; label = @1
      local.get 0
      unreachable
    end"),
            Ok(("", Block{body: vec![Instr::VarInstr(VarInstr::LocalGet(Index::Val(0))),
                                     Instr::Unreachable,]}))
        );
    }

    #[test]
    fn should_consume_func() {
        assert_eq!(
            func("(func $_start (type 0) (local i32))"),
            Ok(("", Func{id: Some("$_start".to_owned()),
                         typeuse: TypeUse{index: Some(Index::Val(0)), params: vec![], results: vec![]},
                         locals: vec![ValType::I32],
                         body: vec![]}))
        );

        assert_eq!(
            func("(func $_start (result i32) i32.const 42)"),
            Ok(("", Func{id: Some("$_start".to_owned()),
                         typeuse: TypeUse{index: None, params: vec![], results: vec![ValType::I32]},
                         locals: vec![],
                         body: vec![Instr::ConstInstr(ConstInstr::I32("42".to_string()))]})),
        );
    }

    #[test]
    fn should_consume_table() {
        assert_eq!(
            table("(table (;0;) 102 102 funcref)"),
            Ok(("", Table{limits: Limits{min: 102, max: Some(102)}}))
        );
    }

    #[test]
    fn should_consume_mem() {
        assert_eq!(
            mem("(memory (;0;) 102 102)"),
            Ok(("", Mem{limits: Limits{min: 102, max: Some(102)}}))
        );
    }

    #[test]
    fn should_consume_constinstr() {
        assert_eq!(
            constinstr("i32.const 0"),
            Ok(("", ConstInstr::I32("0".to_string())))
        );
    }

    #[test]
    fn should_consume_folded_instr() {
        assert_eq!(
            folded_instr("(i32.mul (i32.add (local.get $x) (i32.const 2)) (i32.const 3))"),
            many0(instr)("local.get $x i32.const 2 i32.add i32.const 3 i32.mul")
        );
    }

    #[test]
    fn should_consume_global() {
        assert_eq!(
            global("(global (;0;) (mut i32) (i32.const 1048576))"),
            Ok(("", Global{ gt: GlobalType::Var(ValType::I32),
                            init: vec![Instr::ConstInstr(ConstInstr::I32("1048576".to_string()))] })),
        );
    }

    #[test]
    fn should_consume_elem() {
        assert_eq!(
            elem("(elem (i32.const 123) func $x $y)"),
            Ok(("", Elem{table: Index::Val(0),
                         offset: vec![Instr::ConstInstr(ConstInstr::I32("123".to_string()))],
                         init: vec![Index::Id("$x".to_string()), Index::Id("$y".to_string())],})),
        );
    }

    #[test]
    fn should_consume_escapeseq() {
        assert_eq!(
            escapeseq(r"\n"),
            Ok(("", '\n'))
        );

        assert_eq!(
            escapeseq(r"\n"),
            Ok(("", '\n'))
        );
    }

    #[test]
    fn should_consume_string() {
        assert_eq!(
            string("\"test\""),
            Ok(("", "test".to_string()))
        );

        assert_eq!(
            string(r#""\n""#),
            Ok(("", "\n".to_string()))
        );

    }

    #[test]
    fn should_comsume_import() {
        assert_eq!(
            import("(import \"wasi_snapshot_preview1\" \"proc_exit\" (func $__wasi_proc_exit (type 1)))"),
            Ok(("", Import{module: "wasi_snapshot_preview1".to_string(),
                           name: "proc_exit".to_string(),
                           desc: ImportDesc::Func(Some(Index::Id("$__wasi_proc_exit".to_string())),
                                                  TypeUse{index: Some(Index::Val(1)), params: vec![], results: vec![]})
            }))
        );

    }

    #[test]
    fn should_consume_data() {
        assert_eq!(
            data(r#"(data (;1;) (i32.const 1058288) "\01\00\00\00\00\00\00\00\01\00\00\00\18\0f\10\00")"#),
            Ok(("", Data{idx: None, offset: vec![Instr::ConstInstr(ConstInstr::I32("1058288".to_string()))],
                         init: "\u{1}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{0}\u{1}\u{0}\u{0}\u{0}\u{18}\u{f}\u{10}\u{0}".to_string()}))
        );
    }

    #[test]
    fn should_consume_module() {
        assert_eq!(
            module("(module)"),
            Ok(("", Module::new())),
        );

        {
            module("(module (type (;0;) (func)))").unwrap();
            module("(module (type (;0;) (func)) (func $_start (type 0) (result i32) i32.const 42))").unwrap();
        }
    }
}
