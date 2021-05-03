
use crate::structure::*;
use crate::watparser::*;

use thiserror::Error;

use std::collections::HashMap;
use std::rc::Rc;

type FuncAddr = usize;

#[derive(Error, Debug)]
pub enum ExecError {
    #[error("type mismatch (expected {expected:?}, found {found:?})")]
    TypeMismatch{ expected: String, found: String},

    #[error("stack empty")]
    StackEmpty,

    #[error("invalid stack value: {message}")]
    InvalidStackValue{ message: String},

    #[error("function index not found: index = {index}")]
    FuncIdxNotFound{ index: usize },

    #[error("function not defined: name = {name}")]
    FuncUndefined{ name: String },

}

#[derive(Debug, Default)]
struct Store<'a> {
    funcs: Vec<FuncInstance<'a>>,
}

impl<'a> Store<'a> {
    fn new() -> Self {
        Store::new()
    }
}

#[derive(Debug)]
struct Frame<'a> {
    module: Rc<ModuleInstance<'a>>,
    locals: Vec<Value>,
    arity: usize,
}

#[derive(Debug)]
struct Label<'a> {
    instrs: &'a Vec<Instr>,
    pc: usize,
}

#[derive(Debug, Clone)]
struct ModuleInstance<'a> {
    module: &'a Module,
    funcs: Vec<FuncAddr>,

    funcnamemap: HashMap<String, FuncAddr>
}

#[derive(Debug, Clone)]
struct FuncInstance<'a> {
    module: Rc<ModuleInstance<'a>>,
    code: &'a Func,
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
    Activation(Frame<'a>)
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
}

impl Value {
    fn typedescr(&self) -> String {
        match self {
            Value::I32(_) => "i32".to_owned(),
            Value::I64(_) => "i64".to_owned(),
            Value::F32(_) => "f32".to_owned(),
            Value::F64(_) => "f64".to_owned(),
        }
    }
}

struct Stack<'a>(Vec<Entry<'a>>);

impl<'a> Stack<'a> {
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

    fn current_frame(&mut self) -> &mut Frame<'a> {
        for entry in self.0.iter_mut().rev() {
            match entry {
                Entry::Activation(frame) => return frame,
                _ => continue
            }
        }

        unimplemented!()
    }

    fn current_label(&mut self) -> Option<&mut Label<'a>> {
        for entry in self.0.iter_mut().rev() {
            match entry {
                Entry::Label(label) => return Some(label),
                Entry::Activation(frame) => return None,
                _ => continue
            }
        }

        unimplemented!()
    }

    fn pop_n_val(&mut self, n: usize) -> Result<Vec<Value>, ExecError> {
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

    fn push_frame(&mut self, frame: Frame<'a>) {
        self.0.push(Entry::Activation(frame))
    }

    fn push_label(&mut self, label: Label<'a>) {
        self.0.push(Entry::Label(label))
    }
}


fn exec_func(func: &Func, stack: &mut Stack) -> Result<(), ExecError> {
    for instr in func.body.iter() {
        match instr {
            Instr::ConstInstr(instr) => exec_constinstr(instr, stack),
            Instr::NumInstr(instr) => exec_numinstr(instr, stack)?,
            _ => unimplemented!()
        }
    }

    Ok(())
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

    let frame = stack.current_frame();
    let module = &frame.module;

    let idx = match funcidx {
        Index::Val(idx) => *idx as usize,
        Index::Id(s) => *module.funcnamemap.get(s)
            .ok_or_else(|| ExecError::FuncUndefined{ name: s.clone()})? as usize
    };

    let func = store.funcs.get(idx)
        .ok_or_else(|| ExecError::FuncIdxNotFound{index: idx})?;

    //TODO: pop n val from stack

    let next_frame = Frame{ module: Rc::clone(&func.module),
                            locals: Vec::new(), // TODO
                            arity: 0, // TODO
                            };

    stack.push_frame(next_frame);

    let label = Label { instrs: &func.code.body,
                        pc: 0};
    stack.push_label(label);

    Ok(())
}

fn allocmodule<'a>(store: &'a mut Store<'a>, module: &'a Module) -> Rc<ModuleInstance<'a>> {

    let mut mod_inst = Rc::new(ModuleInstance{ module ,
                                               funcs: Vec::new(),

                                               funcnamemap: HashMap::new()});

    for func in module.func.iter() {
        let funcaddr = allocfunc(store, func, Rc::clone(&mod_inst));
        Rc::get_mut(&mut mod_inst).unwrap().funcs.push(funcaddr);
        if let Some(name) = &func.id {
            Rc::get_mut(&mut mod_inst).unwrap().funcnamemap.insert(name.clone(), funcaddr);
        }
    }

    mod_inst
}

fn allocfunc<'a>(store: &mut Store<'a>, func: &'a Func, mod_inst: Rc<ModuleInstance<'a>>) -> FuncAddr {
    let funcaddr = store.funcs.len();

    let funcinst = FuncInstance{ module: mod_inst, code: func };
    store.funcs.push(funcinst);

    funcaddr
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

// dummy code
fn exec<'a>(stack: &mut Stack<'a>, store: &Store<'a>) {
    loop {
        let mut label = stack.current_label();
        if label.is_none() {
            // maybe end of function

            let frame = stack.current_frame();
            let arity = frame.arity;

            let vals = stack.pop_n_val(arity).unwrap();
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

fn exec_instr<'a>(instr: &Instr, stack: &mut Stack<'a>, store: &Store<'a>) -> Result<(), ExecError> {
    match instr {
        Instr::ConstInstr(instr) => exec_constinstr(instr, stack),
        Instr::NumInstr(instr) => exec_numinstr(instr, stack)?,

        Instr::Call(funcidx) => {
            exec_call(funcidx, stack, store)?;
            return Ok(());
        },
        _ => unimplemented!()
    }

    Ok(())
}

#[cfg(test)]
mod Test {

    use super::*;
    use crate::watparser::*;

    #[test]
    fn should_exec_func() {
        let mut stack = Stack::new();
        let (_, func) = func("(func $_start (result i32) i32.const 42)").unwrap();
        exec_func(&func, &mut stack);

        assert_eq!(stack.pop(), Some(Entry::Value(Value::I32(42))));
    }

}
