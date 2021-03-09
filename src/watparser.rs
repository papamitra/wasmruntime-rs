
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
    let (input, _) = tuple((ws_(tag("(")), ws_(tag("func"))))(input)?;

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

#[derive(Debug, Clone, PartialEq, Eq)]
struct Func {
    id: Option<String>,
    typeidx: Index,
    locals: Vec<ValType>,
    body: Vec<Instr>,
}

fn func(input: &str) -> IResult<&str, Func> {
    let (input, _) = ws(input)?;

    let (input, (_, id, typeidx, locals, body, _)) =  tuple((
        tuple((tag("("), ws, tag("func"))),
        preceded(ws, opt(id)),
        preceded(ws, typeuse),
        preceded(ws, local),
        instrs,
        tuple((ws, tag(")")))))(input)?;

    let id = id.map(|s| s.to_string());

    Ok((input, Func{id, typeidx, locals, body}))
}

fn typeuse(input: &str) -> IResult<&str, Index> {
    let (input, _) = ws(input)?;

    let (input, (_, idx, _)) = tuple((tuple((tag("("), ws, tag("type"))),
                                      index,
                                      pair(ws, tag(")"))))(input)?;

    // TODO: verify param, result
    let (input, _) = opt(param)(input)?;
    let (input, _) = opt(result)(input)?;

    Ok((input, idx))
}

fn local(input: &str) -> IResult<&str, Vec<ValType>> {
    let (input, _) = ws(input)?;

    let (input, (_, locals, _)) = tuple((tuple((tag("("), ws, tag("local"))),
                                 many0(preceded(ws, valtype)),
                                 pair(ws, tag(")"))))(input)?;

    Ok((input, locals))
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Index {
    Val(u32),
    Id(String),
}

fn index(input: &str) -> IResult<&str, Index> {
    let (input, _) = ws(input)?;

    alt((map(digit1, |s: &str| Index::Val(s.parse::<u32>().unwrap())),
         map(id, |s| Index::Id(s.to_string()))))(input)
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Instr {
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
enum VarInstr {
    LocalGet(Index),
    LocalSet(Index),
    LocalTee(Index),
    GlobalGet(Index),
    GlobalSet(Index),
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct MemOpInstr {
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
struct Block {
    body: Vec<Instr>
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
struct NumInstr {
    ty: ValType,
    op: String,
}

fn numinstr(input: &str) -> IResult<&str, NumInstr> {
    let (input, _) = ws(input)?;

    let (input, (ty, _, op)) = tuple((valtype, tag("."), recognize(many1(alt((alphanumeric1, tag("_")))))))(input)?;
    let op = op.to_string();

    Ok((input, NumInstr{ty, op}))
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum ConstInstr {
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
struct Table {
    limits: Limits
}

fn table(input: &str) -> IResult<&str, Table> {
    let (input, _) = ws(input)?;

    let (input, (_, limits, _)) = tuple((tuple((tag("("), ws, tag("table"), ws)),
                                         limits,
                                         tuple((ws, tag("funcref"), ws, tag(")")))))(input)?;

    Ok((input, Table{limits}))
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
struct Limits {
    min: u32,
    max: Option<u32>
}

fn limits(input: &str) -> IResult<&str, Limits> {
    let (input, _) = ws(input)?;

    let (input, (min, _, max)) = tuple((map(digit1, |s: &str| s.parse::<u32>().unwrap()),
                                        ws,
                                        opt(map(digit1, |s: &str| s.parse::<u32>().unwrap()))))(input)?;

    Ok((input, Limits{min, max}))
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum GlobalType {
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
struct Global {
    gt: GlobalType,
    init: Vec<Instr>
}

fn global(input: &str) -> IResult<&str, Global> {

    // TODO: support Abbreviations
    let (input, (_, _, gt, init, _)) =
        tuple((ws_(tag("(")), ws_(tag("global")), globaltype, expr, ws_(tag(")"))))(input)?;

    Ok((input, Global{gt, init}))
}

fn expr(input: &str) -> IResult<&str, Vec<Instr>> {
    instrs(input)
}

struct Export {
    name: String,
    desc: ExportDesc
}

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

    // TODO: support escape sequence
    map(delimited(tag("\""), many0(none_of("\"")), tag("\"")), |s| s.iter().collect::<String>())(input)
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
               elem_offset, opt(ws_(tag("func"))), many0(index),
               ws_(tag(")"))))(input)?;

    Ok(("", Elem{table: Index::Val(0), offset, init}))
}

fn elem_offset(input: &str) -> IResult<&str, Vec<Instr>> {
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
    Func(Option<Index>, Index),
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
                                 |(id, typeidx)| ImportDesc::Func(id, typeidx)),
                             map(preceded(ws_(tag("table")), index), ImportDesc::Table),
                             map(preceded(ws_(tag("memory")), index), ImportDesc::Mem),
                             map(preceded(ws_(tag("global")), index), ImportDesc::Global)
                             ))(input)?;

    let (input, _) = ws_(tag(")"))(input)?;

    Ok((input, desc))
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
            Ok(("", Index::Val(0)))
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
                         typeidx: Index::Val(0),
                         locals: vec![ValType::I32],
                         body: vec![]}))
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
    fn should_consume_string() {
        assert_eq!(
            string("\"test\""),
            Ok(("", "test".to_string()))
        );
    }

    #[test]
    fn should_comsume_import() {
        assert_eq!(
            import("(import \"wasi_snapshot_preview1\" \"proc_exit\" (func $__wasi_proc_exit (type 1)))"),
            Ok(("", Import{module: "wasi_snapshot_preview1".to_string(),
                           name: "proc_exit".to_string(),
                           desc: ImportDesc::Func(Some(Index::Id("$__wasi_proc_exit".to_string())), Index::Val(1))
            }))
        );

    }
}
