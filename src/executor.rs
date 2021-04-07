
use crate::structure::*;
use crate::watparser::*;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum ExecError {
    #[error("type mismatch (expected {expected:?}, found {found:?})")]
    TypeMismatch{ expected: String, found: String},
    #[error("stack empty")]
    StackEmpty
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Entry {
    Value(Value),
    // Label(Label),
    // Activation
}

impl Entry {
    fn typedescr(&self) -> String {
        match self {
            Entry::Value(v) => v.typedescr()
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

struct Stack(Vec<Entry>);

impl Stack {
    fn pop(&mut self) -> Option<Entry> {
        self.0.pop()
    }

    fn push(&mut self, entry: Entry) {
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
