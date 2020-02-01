use rayon::prelude::*;

use super::*;

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub struct Length;

impl Operation for Length {
    fn num_args(&self) -> usize {
        1
    }

    fn name(&self) -> &'static str {
        "length"
    }

    fn process(
        &self,
        args: Vec<LongleafValue>,
        _store: &mut VectorStore,
    ) -> VmResult<LongleafValue> {
        let list = get_only_arg(self.name(), args)?;
        let type_name = list.type_name();

        match list {
            LongleafValue::FloatList(vals) => {
                let vals_slice: &[_] = &**vals;
                Ok(LongleafValue::Float(vals_slice.len() as f64))
            }
            LongleafValue::BoolList(vals) => {
                let vals_slice: &[_] = &**vals;
                Ok(LongleafValue::Float(vals_slice.len() as f64))
            }
            _ => Err(EvalError::TypeMismatch(format!(
                "sum needs a list of floats, but got {}",
                type_name
            ))),
        }
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub struct Sum;

impl Operation for Sum {
    fn num_args(&self) -> usize {
        1
    }

    fn name(&self) -> &'static str {
        "sum"
    }

    fn process(
        &self,
        args: Vec<LongleafValue>,
        _store: &mut VectorStore,
    ) -> VmResult<LongleafValue> {
        let list = get_only_arg(self.name(), args)?;
        let type_name = list.type_name();

        match list {
            LongleafValue::FloatList(vals) => {
                let vals_slice: &[f64] = &**vals;
                let sum: f64 = vals_slice.into_par_iter().sum();
                Ok(LongleafValue::Float(sum))
            }
            _ => Err(EvalError::TypeMismatch(format!(
                "sum needs a list of floats, but got {}",
                type_name
            ))),
        }
    }
}
