
use crate::structure::*;
use crate::watparser::{Func};


#[derive(Debug, PartialEq, Clone, Copy)]
enum Entry {
    Value(Value),
    // Label(Label),
    // Activation
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

type Stack = Vec<Entry>;

fn exec_func(func: &Func, stack: &mut Stack) {
    
}

#[cfg(test)]
mod Test {

    use super::*;
    use crate::watparser::*;

    #[test]
    fn should_exec_func() {
        let mut stack = Stack::new();
        let (_, func) = func(r#"(func $_start (type 0))"#).unwrap();
        exec_func(&func, &mut stack);

        assert!(stack.is_empty());
    }

}
