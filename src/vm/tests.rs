use super::*;

use crate::parser::parse_repl_input;

fn run_repl_inputs(input: &[&str], expected: Vec<PrimitiveValue>) {
    let mut vm = VM::new();

    let mut actual = Vec::with_capacity(expected.len());

    for line in input {
        match parse_repl_input(line).unwrap() {
            ReplInput::Expr(expr) => {
                actual.push(vm.evaluate_expr(expr).unwrap());
            }
            ReplInput::VarDefn(id, expr) => {
                let val = vm.evaluate_expr(expr).unwrap();
                vm.define_variable(&id, val);
            }
            other => {
                panic!("Expected executable input, got {:?}", other);
            }
        }
    }

    assert_eq!(actual.len(), expected.len());

    expected
        .into_iter()
        .zip(actual.into_iter())
        .enumerate()
        .for_each(|(i, (exp, act))| {
            let act: Value = act;
            let exp: Value = Rc::new(exp);
            assert_eq!(act, exp, "Index {}", i);
        });
}

#[test]
fn expr_tests() {
    run_repl_inputs(&["1+2"], vec![PrimitiveValue::Float(3.0)]);
    run_repl_inputs(&["1+2*3"], vec![PrimitiveValue::Float(7.0)]);
    run_repl_inputs(
        &["1+[3,4]*2"],
        vec![PrimitiveValue::FloatList(vec![7., 9.])],
    );
    run_repl_inputs(
        &["[1,4]*[2,6]+3"],
        vec![PrimitiveValue::FloatList(vec![5., 27.])],
    );
}

#[test]
fn var_tests() {
    run_repl_inputs(&["x=1;", "x+2"], vec![PrimitiveValue::Float(3.)]);
    run_repl_inputs(&["x=2*3;", "1+x*7"], vec![PrimitiveValue::Float(43.)]);
    run_repl_inputs(
        &["x=2;", "y=[3,4];", "1+x*y"],
        vec![PrimitiveValue::FloatList(vec![7., 9.])],
    );
    run_repl_inputs(
        &["x=[1,3]+[0,1];", "y=[17];", "z=3;", "x*[2,6]+z"],
        vec![PrimitiveValue::FloatList(vec![5., 27.])],
    );
}
