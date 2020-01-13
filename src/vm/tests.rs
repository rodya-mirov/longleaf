use super::*;

use crate::parser::{parse_repl_input, ReplInput};

fn run_repl_inputs(input: &[&str], expected: Vec<LongleafValue>) {
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
            let exp: LongleafValue = exp;
            assert_eq!(act, exp, "Index {}", i);
        });
}

fn to_ll_values(data: Vec<Vec<f64>>) -> Vec<LongleafValue> {
    let dummy: VectorStore = VectorStore::new();

    data.into_iter()
        .map(|v| dummy.track_vector(v).into())
        .collect()
}

#[test]
fn expr_tests() {
    run_repl_inputs(&["1+2"], vec![(3.0).into()]);
    run_repl_inputs(&["1+2*3"], vec![(7.0).into()]);
    run_repl_inputs(&["1+[3,4]*2"], to_ll_values(vec![vec![7., 9.]]));
    run_repl_inputs(&["[1,4]*[2,6]+3"], to_ll_values(vec![vec![5., 27.]]));
}

#[test]
fn var_tests() {
    run_repl_inputs(&["x=1;", "x+2"], vec![(3.0).into()]);
    run_repl_inputs(&["x=2*3;", "1+x*7"], vec![(43.0).into()]);
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

    // Nested functions don't work, there is an open issue for this
}

#[test]
fn make_range_tests() {
    fn do_range_test(start: f64, end: f64, step: f64, expected: Vec<f64>) {
        let store = VectorStore::new();

        let actual = make_range(start.into(), end.into(), step.into(), &store).unwrap();

        let expected: LongleafValue = store.track_vector(expected).into();

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
