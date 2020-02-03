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

    fn process(&self, args: Vec<LongleafValue>, _ctx: &mut VMContext) -> VmResult<LongleafValue> {
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

    fn process(&self, args: Vec<LongleafValue>, ctx: &mut VMContext) -> VmResult<LongleafValue> {
        let list = get_only_arg(self.name(), args)?;
        let type_name = list.type_name();

        match list {
            LongleafValue::FloatList(vals) => {
                let vals_slice: &[f64] = &**vals;
                let sum: f64 = ctx.pool.install(|| vals_slice.into_par_iter().sum());
                Ok(LongleafValue::Float(sum))
            }
            _ => Err(EvalError::TypeMismatch(format!(
                "sum needs a list of floats, but got {}",
                type_name
            ))),
        }
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub struct Dot;

impl Operation for Dot {
    fn num_args(&self) -> usize {
        2
    }

    fn name(&self) -> &'static str {
        "dot"
    }

    fn process(&self, args: Vec<LongleafValue>, ctx: &mut VMContext) -> VmResult<LongleafValue> {
        let (a, b) = get_two_args(self.name(), args)?;

        let type_name_a = a.type_name();
        let type_name_b = b.type_name();

        let err_maker = || {
            Err(EvalError::TypeMismatch(format!(
                "{} needs two float lists, but got {} and {}",
                self.name(),
                type_name_a,
                type_name_b
            )))
        };

        match a {
            LongleafValue::FloatList(a) => match b {
                LongleafValue::FloatList(b) => {
                    let a_slice: &[f64] = &**a;
                    let b_slice: &[f64] = &**b;

                    if a_slice.len() != b_slice.len() {
                        return Err(EvalError::DimensionMismatch(format!(
                            "{} requires two lists of the same length, but got {} and {}",
                            self.name(),
                            a_slice.len(),
                            b_slice.len()
                        )));
                    }

                    let dot_product: f64 = ctx.pool.install(|| {
                        a_slice
                            .into_par_iter()
                            .zip(b_slice)
                            .map(|(a, b)| a * b)
                            .sum()
                    });

                    Ok(LongleafValue::Float(dot_product))
                }
                _ => err_maker(),
            },
            _ => err_maker(),
        }
    }
}
