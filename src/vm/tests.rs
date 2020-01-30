use super::*;

use crate::parser::{parse_repl_input, ReplInput};

fn run_repl_inputs<T: Into<LongleafValue>>(input: &[&str], expected: Vec<T>) {
    let mut vm = VM::new();

    let mut actual = Vec::with_capacity(expected.len());

    for line in input {
        match parse_repl_input(line).unwrap() {
            ReplInput::Expr(expr) => {
                actual.push(vm.evaluate_expr(expr).unwrap());
            }
            ReplInput::Statement(stmt) => {
                vm.evaluate_statement(stmt).unwrap();
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
            let act: LongleafValue = act;
            let exp: LongleafValue = exp.into();
            assert_eq!(act, exp, "Index {}", i);
        });
}

fn to_ll_values(data: Vec<Vec<f64>>) -> Vec<LongleafValue> {
    let dummy: VectorStore = VectorStore::default();

    data.into_iter()
        .map(|v| dummy.track_vector(v).into())
        .collect()
}

#[test]
fn expr_tests() {
    run_repl_inputs(&["1+2"], vec![3.0]);
    run_repl_inputs(&["1+2*3"], vec![7.0]);
    run_repl_inputs(&["1+[3,4]*2"], to_ll_values(vec![vec![7., 9.]]));
    run_repl_inputs(&["[1,4]*[2,6]+3"], to_ll_values(vec![vec![5., 27.]]));
}

#[test]
fn trig_tests() {
    run_repl_inputs(&["sin(0)", "sin(2)"], vec![0.0, (2.0 as f64).sin()]);
    run_repl_inputs(&["cos(0)", "cos(2)"], vec![1.0, (2.0 as f64).cos()]);
    run_repl_inputs(&["tan(0)", "tan(2)"], vec![0.0, (2.0 as f64).tan()]);
}

#[test]
fn exp_tests() {
    run_repl_inputs(
        &["exp(0)", "exp(1)", "exp(-12.05)"],
        vec![1.0, (1.0 as f64).exp(), (-12.05 as f64).exp()],
    );

    run_repl_inputs(
        &["exp([0, 1, -12.05])"],
        to_ll_values(vec![
            // Only one value, which is three numbers
            vec![1.0, (1.0 as f64).exp(), (-12.05 as f64).exp()],
        ]),
    );

    run_repl_inputs(
        &["ln(0)", "ln(1)", "ln(12.05)"],
        vec![(0.0 as f64).ln(), 0.0, (12.05 as f64).ln()],
    );
}

#[test]
fn var_tests() {
    run_repl_inputs(&["x=1;", "x+2"], vec![3.0]);
    run_repl_inputs(&["x=2*3;", "1+x*7"], vec![43.]);
    run_repl_inputs(
        &["x=2;", "y=[3,4];", "1+x*y"],
        to_ll_values(vec![vec![7., 9.]]),
    );
    run_repl_inputs(
        &["x=[1,3]+[0,1];", "y=[17];", "z=3;", "x*[2,6]+z"],
        to_ll_values(vec![vec![5., 27.]]),
    );
}

#[test]
fn func_tests() {
    // Simple function, testing late bound values
    run_repl_inputs(
        &[
            "f = \\x => x + 15 * y;",
            "y = [12, 11];",
            "f([1, 2])",
            "f([0, 0])",
        ],
        // 15 * y is [180, 165]
        to_ll_values(vec![vec![181., 167.], vec![180., 165.]]),
    );

    // Closures don't work and nested calls don't work, there is an open issue for this
}

#[test]
fn block_func_tests() {
    // Simple function, testing late bound values
    run_repl_inputs(
        &[
            "f = \\x => {z = x + 1; return z * y;};",
            "y = [12, 11];",
            "f([1, 2])",
            "f([-1, -1])",
        ],
        to_ll_values(vec![vec![24., 33.], vec![0., 0.]]),
    );

    // Closures don't work and nested calls don't work, there is an open issue for this
}
