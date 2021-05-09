use crate::structure::*;
use crate::watparser::*;

use thiserror::Error;

use std::collections::HashMap;
use std::rc::Rc;

type FuncAddr = usize;
type ExternAddr = usize;
type GlobalAddr = usize;
type TableAddr = usize;

#[derive(Error, Debug)]
pub enum ExecError {
    #[error("type mismatch (expected {expected:?}, found {found:?})")]
    TypeMismatch{ expected: String, found: String},

    #[error("stack empty")]
    StackEmpty,

    #[error("invalid stack value: {0}")]
    InvalidStack(String),

    #[error("function index not found: index = {index}")]
    FuncIdxNotFound{ index: usize },

    #[error("function not defined: name = {name}")]
    FuncUndefined{ name: String },

}

#[derive(Debug, Default)]
struct Store<'a> {
    funcs: Vec<FuncInstance<'a>>,
    globals: Vec<GlobalInstance>,
    tables: Vec<TableInstance>,
}

impl<'a> Store<'a> {
    fn new() -> Self {
        Store {
            funcs: Vec::new(),
            globals: Vec::new(),
            tables: Vec::new(),
        }
    }
}

#[derive(Debug)]
struct Frame {
    module: Rc<ModuleInstance>,
    locals: Vec<Value>,
    arity: usize,
}

#[derive(Debug)]
struct Label<'a> {
    instrs: &'a [Instr],
    arity: usize,
    pc: usize,
}

#[derive(Debug, Clone)]
struct ModuleInstance {
    funcaddrs: Vec<FuncAddr>,
    globaladdrs: Vec<GlobalAddr>,
    tableaddrs: Vec<TableAddr>,

    funcnamemap: HashMap<String, FuncAddr>
}

impl ModuleInstance {
    fn new() -> ModuleInstance {
        ModuleInstance{
            funcaddrs: Vec::new(),
            globaladdrs: Vec::new(),
            tableaddrs: Vec::new(),

            funcnamemap: HashMap::new()}
    }
}

#[derive(Debug, Clone)]
struct FuncInstance<'a> {
    module: Rc<ModuleInstance>,
    code: &'a Func,
}

#[derive(Debug, Clone)]
struct GlobalInstance {
    mutability: Mutability,
    value: Value,
}

#[derive(Debug, Clone)]
enum Mutability {
    Const,
    Var,
}

#[derive(Debug, Clone)]
struct TableInstance {
    tabletype: Limits,
    elem: Vec<Ref>,
}

#[derive(Debug, Default)]
struct IdContext {
    funcs: HashMap<String, usize>,
}

impl IdContext {
    fn new() -> IdContext {
        IdContext::default()
    }
}

#[derive(Debug)]
enum Entry<'a> {
    Value(Value),
    Label(Label<'a>),
    Activation(Frame)
}

impl<'a> Entry<'a> {
    fn typedescr(&self) -> String {
        match self {
            Entry::Value(v) => v.typedescr(),
            _ => unimplemented!()
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Ref(Ref),
}

impl Value {
    fn typedescr(&self) -> String {
        match self {
            Value::I32(_) => "i32".to_owned(),
            Value::I64(_) => "i64".to_owned(),
            Value::F32(_) => "f32".to_owned(),
            Value::F64(_) => "f64".to_owned(),
            Value::Ref(_) => "Ref".to_owned(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Ref {
    Null(RefType),
    FuncAddr(FuncAddr),
    ExternAddr(ExternAddr),
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum RefType {
    Function,
    Extern,
}

struct Stack<'a>(Vec<Entry<'a>>);

impl<'a> Stack<'a> {

    fn new() -> Self {
        Stack(Vec::new())
    }

    fn pop(&mut self) -> Option<Entry> {
        self.0.pop()
    }

    fn push(&mut self, entry: Entry<'a>) {
        self.0.push(entry)
    }

    fn push_i32(&mut self, v: i32) {
        self.0.push(Entry::Value(Value::I32(v)));
    }

    fn push_i64(&mut self, v: i64) {
        self.0.push(Entry::Value(Value::I64(v)));
    }

    fn pop_i32(&mut self) -> Result<i32, ExecError> {
        match self.pop() {
            Some(Entry::Value(Value::I32(v))) => Ok(v),
            Some(v) => Err(ExecError::TypeMismatch{ expected: "i32".to_owned(),
                                                    found: v.typedescr()}),
            None => Err(ExecError::StackEmpty)
        }
    }

    fn pop_i64(&mut self) -> Result<i64, ExecError> {
        match self.pop() {
            Some(Entry::Value(Value::I64(v))) => Ok(v),
            Some(v) => Err(ExecError::TypeMismatch{ expected: "i64".to_owned(),
                                                    found: v.typedescr()}),
            None => Err(ExecError::StackEmpty)
        }
    }

    fn current_frame(&mut self) -> Option<&mut Frame> {
        for entry in self.0.iter_mut().rev() {
            match entry {
                Entry::Activation(frame) => return Some(frame),
                _ => continue
            }
        }

        None
    }

    fn current_label(&mut self) -> Option<&mut Label<'a>> {
        for entry in self.0.iter_mut().rev() {
            match entry {
                Entry::Label(label) => return Some(label),
                Entry::Activation(frame) => return None,
                _ => continue
            }
        }

        None
    }

    fn pop_val(&mut self) -> Result<Value, ExecError> {
        match self.pop() {
            Some(Entry::Value(v)) => Ok(v),
            Some(v) => return Err(ExecError::TypeMismatch{ expected: "Value".to_owned(),
                                                           found: v.typedescr()}),
            _ => return Err(ExecError::StackEmpty)
        }
    }

    fn pop_n_vals(&mut self, n: usize) -> Result<Vec<Value>, ExecError> {
        let mut vals = Vec::new();

        for _ in 0..n {
            match self.pop() {
                Some(Entry::Value(v)) => vals.push(v),
                Some(v) => return Err(ExecError::TypeMismatch{ expected: "Value".to_owned(),
                                                               found: v.typedescr()}),
                _ => return Err(ExecError::StackEmpty)
            }
        }

        Ok(vals)
    }

    fn push_val(&mut self, val: Value) {
        self.0.push(Entry::Value(val));
    }

    fn push_vals(&mut self, vals: &[Value]) {
        vals.iter().for_each(|v| self.push_val(*v));
    }

    fn push_frame(&mut self, frame: Frame) {
        self.0.push(Entry::Activation(frame))
    }

    fn push_label(&mut self, label: Label<'a>) {
        self.0.push(Entry::Label(label))
    }
}

fn exec_constinstr(instr: &ConstInstr, stack: &mut Stack) {
    let value = match instr {
        ConstInstr::I32(s) => Value::I32(s.parse::<i32>().unwrap()),
        ConstInstr::I64(s) => Value::I64(s.parse::<i64>().unwrap()),
        ConstInstr::F32(s) => Value::F32(s.parse::<f32>().unwrap()),
        ConstInstr::F64(s) => Value::F64(s.parse::<f64>().unwrap()),
    };

    stack.push(Entry::Value(value));
}

fn exec_numinstr(instr: &NumInstr, stack: &mut Stack) -> Result<(), ExecError> {
    match instr {
        NumInstr{ty, ref op} if op == "add" => exec_add(&ty, stack)?,
        _ => unimplemented!()
    }

    Ok(())
}

fn binop_i32(stack: &mut Stack, f: fn(i64, i64) -> i64) -> Result<(), ExecError> {
    let lhs = stack.pop_i32()? as i64;
    let rhs = stack.pop_i32()? as i64;

    let res = (f(lhs, rhs) % 2_i64.pow(32)) as i32;

    stack.push_i32(res);

    Ok(())
}

fn binop_i64(stack: &mut Stack, f: fn(i128, i128) -> i128) -> Result<(), ExecError> {
    let lhs = stack.pop_i64()? as i128;
    let rhs = stack.pop_i64()? as i128;

    let res = (f(lhs, rhs) % 2_i128.pow(64)) as i64;

    stack.push_i64(res);

    Ok(())
}


fn exec_add(ty: &ValType, stack: &mut Stack) -> Result<(), ExecError> {
    match ty {
        ValType::I32 => {
            binop_i32(stack, |x, y| x+y)
        },
        _ => unimplemented!()
    }
}

fn exec_call<'a>(funcidx: &Index, stack: &mut Stack<'a>, store: &Store<'a>) -> Result<(), ExecError> {

    let frame = stack.current_frame().unwrap();
    let module = &frame.module;

    let idx = match funcidx {
        Index::Val(idx) => *idx as usize,
        Index::Id(s) => *module.funcnamemap.get(s)
            .ok_or_else(|| ExecError::FuncUndefined{ name: s.clone()})? as usize
    };

    invoke_function(store, idx, stack);

    Ok(())
}

fn invoke_function<'a>(store: &Store<'a>, funcaddr: FuncAddr, stack: &mut Stack<'a>) -> Result<(), ExecError> {
    let func = store.funcs.get(funcaddr)
        .ok_or_else(|| ExecError::FuncIdxNotFound{index: funcaddr})?;

    let typeuse = &func.code.typeuse;
    let arity = typeuse.results.len();
    let mut locals = stack.pop_n_vals(typeuse.params.len())?;
    for valtype in func.code.locals.iter() {
        locals.push(valtype_to_defaultval(valtype));
    }


    let next_frame = Frame{ module: Rc::clone(&func.module),
                            locals,
                            arity,
                            };

    stack.push_frame(next_frame);

    enter_instrs_with_label(stack, &func.code.body);

    Ok(())
}

fn valtype_to_defaultval(valtype: &ValType) -> Value {
    match valtype {
        ValType::I32 => Value::I32(0),
        ValType::I64 => Value::I64(0),
        ValType::F32 => Value::F32(0.0),
        ValType::F64 => Value::F64(0.0),
    }
}

fn enter_instrs_with_label<'a, 'b: 'a>(stack: &mut Stack<'a>, instrs: &'b [Instr]) {
    let label = Label { instrs,
                        arity: 0, // TODO
                        pc: 0};
    stack.push_label(label);
}


fn allocmodule<'a>(store: &mut Store<'a>, module: &'a Module) -> Rc<ModuleInstance> {
    log::debug!("alloc module");

    let mut mod_inst = Rc::new(ModuleInstance::new());

    let mod_ptr: *mut _ = Rc::get_mut(&mut mod_inst).unwrap();

    for func in module.func.iter() {
        let funcaddr = allocfunc(store, func, Rc::clone(&mod_inst));
        unsafe { (*mod_ptr).funcaddrs.push(funcaddr);}
        if let Some(name) = &func.id {
            unsafe { (*mod_ptr).funcnamemap.insert(name.clone(), funcaddr);}
        }
    }

    for gl in module.gl.iter() {
        let globaladdr = allocglobal(store, gl);
        unsafe { (*mod_ptr).globaladdrs.push(globaladdr); }

        // TODO: treat global id??
    }

    for ta in module.ta.iter() {
        let tableaddr = alloctable(store, &ta.limits, &Ref::Null(RefType::Function));
        unsafe { (*mod_ptr).tableaddrs.push(tableaddr); }
    }

    mod_inst
}

fn allocfunc<'a>(store: &mut Store<'a>, func: &'a Func, mod_inst: Rc<ModuleInstance>) -> FuncAddr {
    let funcaddr = store.funcs.len();

    let funcinst = FuncInstance{ module: mod_inst, code: func };
    store.funcs.push(funcinst);

    funcaddr
}

fn allocglobal<'a>(store: &mut Store<'a>, gl: &Global) -> GlobalAddr {
    let globaladdr = store.globals.len();

    let mut stack = Stack::new();
    let mut dummy_store = Store::new();

    let label = Label { instrs: &gl.init,
                        arity: 0,
                        pc: 0};
    stack.push_label(label);

    exec(&mut stack, &mut dummy_store);

    let init = stack.pop_val().unwrap(); // FIXME

    let mutability = match gl.gt {
        GlobalType::Const(_) => Mutability::Const,
        GlobalType::Var(_) => Mutability::Var,
    };

    let glinst = GlobalInstance{ mutability, value: init};
    store.globals.push(glinst);

    globaladdr
}

fn alloctable<'a>(store: &mut Store<'a>, tabletype: &Limits, refv: &Ref) -> TableAddr {
    let tableaddr = store.tables.len();

    let tableinstance = TableInstance{ tabletype: tabletype.clone(),
                                       elem: vec![refv.clone(); tabletype.min as usize]};

    store.tables.push(tableinstance);

    tableaddr
}

fn create_idcontext(module: &Module) -> IdContext {
    let mut context = IdContext::new();

    for (i, func) in module.func.iter().enumerate() {
        if let Some(name) = &func.id {
            context.funcs.insert(name.clone(),i);
        }
    }

    context
}

fn invoke<'a>(store: &mut Store<'a>, funcaddr: FuncAddr, val: Vec<Value>) -> Result<Vec<Value>, ExecError> {
    log::debug!("invoke funcaddr: {}", funcaddr);

    let func = &store.funcs[funcaddr];
    let typeuse = &func.code.typeuse;
    if typeuse.params.len() != val.len() {
        panic!("invode error: param length missmatch");
    }

    // TODO: verify params type

    let mod_inst = Rc::new(ModuleInstance::new());

    let frame = Frame{ module: mod_inst,
                       locals: Vec::new(),
                       arity: typeuse.results.len()};

    let mut stack = Stack::new();
    for v in val {
        stack.push(Entry::Value(v));
    }

    invoke_function(store, funcaddr, &mut stack)?;

    exec(&mut stack, store);

    stack.pop_n_vals(frame.arity)
}

// dummy code
fn exec<'a, 'b: 'a>(stack: &mut Stack<'a>, store: &mut Store<'b>) {
    loop {
        let label = stack.current_label();
        if label.is_none() {
            let frame = stack.current_frame();
            if frame.is_none() {
                log::debug!("reached end of execution");
                return;
            }

            log::debug!("reached end of function");
            let frame = frame.unwrap();
            let arity = frame.arity;

            let vals = stack.pop_n_vals(arity).unwrap();
            match stack.pop() {
                Some(Entry::Activation(frame)) => {
                    for val in vals.into_iter().rev() {
                        stack.push(Entry::Value(val));
                    }
                },
                _ => panic!("invalid stack")
            }

            continue;
        }

        let mut label = label.unwrap();

        let pc = label.pc;

        if label.instrs.len() <= pc {
            // reached end of label

            let mut vals = Vec::new();
            loop {
                match stack.pop() {
                    Some(Entry::Value(v)) => vals.push(Entry::Value(v)),
                    Some(Entry::Label(_)) => {
                        for v in vals.into_iter().rev() {
                            stack.push(v);
                        }
                        break
                    },
                    _ => panic!("Error: invalid stack")
                }
            }

            continue;
        }

        let instr = &label.instrs[pc];
        label.pc += 1;

        exec_instr(instr, stack, store);
    }
}

fn exec_instr<'a, 'b: 'a>(instr: &'a Instr, stack: &mut Stack<'a>, store: &mut Store<'b>) -> Result<(), ExecError> {
    log::debug!("exec_instr: {:?}", instr);

    match instr {
        Instr::Unreachable => panic!("Unreachable"),
        Instr::ConstInstr(instr) => exec_constinstr(instr, stack),
        Instr::NumInstr(instr) => exec_numinstr(instr, stack)?,

        Instr::Call(funcidx) => {
            exec_call(funcidx, stack, store)?;
            return Ok(());
        },

        Instr::Return => exec_return(stack)?,

        Instr::VarInstr(instr) => exec_varinstr(instr, stack, store)?,

        Instr::Block(block) => exec_block(block, stack)?,
        // Instr::Loop
        // Instr::If
        Instr::Br(l) => exec_br(*l, stack)?,

        _ => unimplemented!()
    }

    Ok(())
}

fn exec_varinstr<'a, 'b: 'a>(instr: &VarInstr, stack: &mut Stack<'a>, store: &mut Store<'b>) -> Result<(), ExecError> {
    match instr {
        VarInstr::LocalGet(idx) => {
            let index = to_index(idx)?;
            let frame = stack.current_frame().unwrap();
            let v = *frame.locals.get(index).unwrap();
            stack.push(Entry::Value(v));
        },
        VarInstr::LocalSet(idx) => {
            let index = to_index(idx)?;
            let val = stack.pop_val()?;
            let mut frame = stack.current_frame().unwrap();
            frame.locals[index] = val;
        },
        VarInstr::LocalTee(idx) => {
            let val = stack.pop_val()?;
            stack.push_val(val);
            stack.push_val(val);
            exec_varinstr(&VarInstr::LocalSet(idx.clone()), stack, store)?;
        },
        VarInstr::GlobalGet(idx) => {
            let index = to_index(idx)?;
            let frame = stack.current_frame().unwrap();
            let globaladdr = frame.module.globaladdrs[index];
            let glob = &store.globals[globaladdr];
            stack.push_val(glob.value);
        },
        VarInstr::GlobalSet(idx) => {
            let index = to_index(idx)?;
            let frame = stack.current_frame().unwrap();
            let globaladdr = frame.module.globaladdrs[index];
            let glob = &mut store.globals[globaladdr];
            let val = stack.pop_val().unwrap();
            glob.value = val;
        },
    }

    Ok(())
}

fn to_index<'a>(idx: &Index) -> Result<usize, ExecError> {
    match idx {
        Index::Val(idx) => Ok(*idx as usize),
        Index::Id(_) => unimplemented!(),
    }
}

fn exec_return<'a>(stack: &mut Stack<'a>) -> Result<(), ExecError> {
    let frame = stack.current_frame().unwrap();
    let arity = frame.arity;
    let vals = stack.pop_n_vals(arity)?;

    loop {
        match stack.pop() {
            Some(Entry::Activation(_)) => break,
            _ => continue,
            None => return Err(ExecError::InvalidStack("Frame not found".to_owned())),
        }
    }

    stack.push_vals(&vals);

    Ok(())
}

fn exec_block<'a, 'b: 'a>(block: &'b Block, stack: &mut Stack<'a>) -> Result<(), ExecError> {

    //TODO: treat stack according to blocktype

    enter_instrs_with_label(stack, &block.body);

    Ok(())
}

fn exec_loop<'a, 'b: 'a>(block: &'b Block, stack: &mut Stack<'a>) -> Result<(), ExecError> {
    let mut label = stack.current_label().unwrap();
    label.pc -= 1; // next instr is this loop itself

    //TODO: treat stack according to blocktype

    enter_instrs_with_label(stack, &block.body);

    Ok(())
}

fn exec_br<'a>(l: u32, stack: &mut Stack<'a>) -> Result<(), ExecError> {

    let mut vals = Vec::new();
    let mut label_cnt = 0;
    loop {
        match stack.pop() {
            Some(Entry::Label(label)) => {
                label_cnt += 1;
                if label_cnt == l+1 {
                    let arity = label.arity;
                    vals[0..arity].iter().rev().for_each(|v| stack.push_val(*v));
                    break
                }
            },
            Some(Entry::Value(v)) => vals.push(v),
            Some(_) => return Err(ExecError::InvalidStack("exec_br: stack have unexpected frame".to_owned())),
            None => return Err(ExecError::InvalidStack(format!("exec_br: stack have not {}th label", l+1))),
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {

    use super::*;
    use crate::watparser::*;

    fn init() {
        let _ = env_logger::builder()
            .filter_level(log::LevelFilter::max())
            .is_test(true).try_init();
    }

    #[test]
    fn should_alloc_module() {
        init();

        let (_, module) = module("(module (type (;0;) (func)) (func $_start (type 0) (result i32) i32.const 42))").unwrap();

        let mut store = Store::new();
        let modinst = allocmodule(&mut store, &module);
    }

    #[test]
    fn should_invoke_function() {
        init();

        let (_, module) = module("(module (type (;0;) (func)) (func $_start (type 0) (result i32) i32.const 42))").unwrap();

        let mut store = Store::new();
        let modinst = allocmodule(&mut store, &module);

        let res = invoke(&mut store, 0, Vec::new());
        assert!(res.is_ok());

        assert_eq!(res.unwrap(), vec![Value::I32(42)]);
    }

    #[test]
    fn should_alloc_global() {
        init();

        let (_, global) = global("(global (;0;) (mut i32) (i32.const 1048576))").unwrap();

        let mut store = Store::new();
        let globaladdr = allocglobal(&mut store, &global);
        assert_eq!(store.globals[globaladdr].value, Value::I32(1048576));
    }
}
