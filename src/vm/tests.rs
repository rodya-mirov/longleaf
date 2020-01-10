use super::*;

use crate::parser::parse_repl_input;

fn run_repl_inputs(input: &[&str], expected: Vec<EvalValue>) {
    let mut vm = VM::new();

    let mut actual = Vec::with_capacity(expected.len());

    for line in input {
        match parse_repl_input(line).unwrap() {
            ReplInput::Expr(expr) => {
                actual.push(vm.evaluate_expr(expr).unwrap());
            }
            ReplInput::VarDefn(id, expr) => {
                let val = vm.evaluate_expr(expr).unwrap();
                vm.define_variable(&id, val).unwrap();
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
            let act: EvalValue = act;
            let exp: EvalValue = exp;
            assert_eq!(act, exp, "Index {}", i);
        });
}

#[test]
fn expr_tests() {
    run_repl_inputs(&["1+2"], vec![(3.0).into()]);
    run_repl_inputs(&["1+2*3"], vec![(7.0).into()]);
    run_repl_inputs(&["1+[3,4]*2"], vec![(vec![7., 9.].into())]);
    run_repl_inputs(&["[1,4]*[2,6]+3"], vec![(vec![5., 27.].into())]);
}

#[test]
fn var_tests() {
    run_repl_inputs(&["x=1;", "x+2"], vec![(3.0).into()]);
    run_repl_inputs(&["x=2*3;", "1+x*7"], vec![(43.0).into()]);
    run_repl_inputs(&["x=2;", "y=[3,4];", "1+x*y"], vec![(vec![7., 9.]).into()]);
    run_repl_inputs(
        &["x=[1,3]+[0,1];", "y=[17];", "z=3;", "x*[2,6]+z"],
        vec![(vec![5., 27.]).into()],
    );
}

#[test]
fn make_range_tests() {
    fn do_range_test(start: f64, end: f64, step: f64, expected: Vec<f64>) {
        let actual = make_range(start.into(), end.into(), step.into()).unwrap();

        let expected: EvalValue = expected.into();

        assert_eq!(actual, expected);
    }

    do_range_test(1., 2., 1., vec![1.]);
    do_range_test(1., 2., 0.5, vec![1., 1.5]);

    do_range_test(1., 1., 1., vec![]);

    do_range_test(2., 1., -1., vec![2.]);
    do_range_test(2., 1., -0.5, vec![2., 1.5]);
    do_range_test(
        2.,
        1.,
        -0.3,
        vec![2., 2. - 0.3, 2. - 0.3 - 0.3, 2. - 0.3 - 0.3 - 0.3],
    );
}
