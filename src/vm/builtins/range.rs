use super::*;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Range;

impl Operation for Range {
    fn num_args(&self) -> usize {
        3
    }

    fn name(&self) -> &'static str {
        "range"
    }

    fn process(&self, args: Vec<LongleafValue>, store: &VectorStore) -> VmResult<LongleafValue> {
        let (a, b, c) = get_three_args(self.name(), args)?;

        make_range(a, b, c, store)
    }
}

fn make_range(
    start: LongleafValue,
    end: LongleafValue,
    step: LongleafValue,
    arena: &VectorStore,
) -> VmResult<LongleafValue> {
    let start = get_float_helper(start, "0 (start)")?;
    let end = get_float_helper(end, "1 (end)")?;
    let step = get_float_helper(step, "2 (step)")?;

    if start < end && step <= 0. {
        return Err(EvalError::IllegalArgument(
            "Since start<end, expected argument 2 (step) to be a positive number".to_string(),
        ));
    } else if start > end && step >= 0. {
        return Err(EvalError::IllegalArgument(
            "Since start>end, expected argument 2 (step) to be a negative number".to_string(),
        ));
    }

    let len: usize = { ((end - start) / step).ceil() as usize };

    let mut running = start;
    let mut out = arena.get_vector(len)?;

    for i in 0..len {
        out[i] = running;
        running += step;
    }

    Ok(out.into())
}

fn get_float_helper(f: LongleafValue, arg_name: &str) -> VmResult<f64> {
    match f {
        LongleafValue::Float(f) => Ok(f),
        LongleafValue::FloatList(_) => Err(EvalError::TypeMismatch(format!(
            "Argument '{}' needed to be a float, but got a float list",
            arg_name
        ))),
        LongleafValue::FunctionDefinition(_, _) => Err(EvalError::TypeMismatch(format!(
            "Argument '{}' needed to be a float, but got a function",
            arg_name
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn make_range_tests() {
        fn do_range_test(start: f64, end: f64, step: f64, expected: Vec<f64>) {
            let store = VectorStore::default();

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
}
