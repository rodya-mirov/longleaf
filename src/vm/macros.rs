macro_rules! unary_fn_switcher {
    ($func:expr, $a:expr, $store:expr) => {{
        let a: LongleafValue = $a;
        let func = $func;

        match a {
            LongleafValue::Float(a) => Ok(LongleafValue::Float(func(a))),
            LongleafValue::FloatList(a) => {
                Ok(LongleafValue::FloatList(unary_fn_vector!(func, a, $store)))
            }
            LongleafValue::FunctionDefinition(_, _) => Err(EvalError::TypeMismatch(format!(
                "Expected a float or vector; got a function"
            ))),
        }
    }};
}

macro_rules! unary_fn_vector {
    ($func:expr, $a:expr, $store:expr) => {{
        let a: &[f64] = &**$a; // TODO: for real?

        let func = $func;

        let store = $store;

        let mut out = store.get_vector(a.len());
        assert_eq!(out.len(), a.len());

        let out_slice: &mut [f64] = &mut out;

        use rayon::prelude::*;

        out_slice
            .par_iter_mut()
            .zip(a.par_iter().copied())
            .with_min_len(PAR_CHUNK_LEN)
            .for_each(|(o, i)| {
                *o = func(i);
            });

        out.into()
    }};
}

macro_rules! unary_switcher {
    ($op:tt, $a:expr, $store:expr) => {
        {
            let a: LongleafValue = $a;

            match a {
                LongleafValue::Float(a) => Ok(LongleafValue::Float($op a)),
                LongleafValue::FloatList(a) => Ok(LongleafValue::FloatList(unary_op_vector!($op, a, $store))),
                LongleafValue::FunctionDefinition(_, _) => Err(EvalError::TypeMismatch(format!(
                    "Expected a float or vector; got a function"
                ))),
            }
        }
    }
}

macro_rules! unary_op_vector {
    ($op:tt, $a:expr, $store:expr) => {
        {
            let a: &[f64] = &**$a; // TODO: for real?

            let mut out = ($store).get_vector(a.len());
            assert_eq!(out.len(), a.len());

            let out_slice: &mut [f64] = &mut out;

                use rayon::prelude::*;

                out_slice.par_iter_mut().zip(a.par_iter()).with_min_len(PAR_CHUNK_LEN).for_each(|(o, i)| {
                    *o = $op i;
                });

            out.into()
        }
    };
}

macro_rules! binary_switcher {
    ($op:tt, $a:expr, $b:expr, $store:expr) => {
        {
            let a: LongleafValue = $a;
            let b: LongleafValue = $b;

            match a {
                LongleafValue::Float(a) => match b {
                    LongleafValue::Float(b) => Ok(LongleafValue::Float(a $op b)),
                    LongleafValue::FloatList(b) => Ok(LongleafValue::FloatList(scalar_vector!($op, a, b, $store))),
                    LongleafValue::FunctionDefinition(_, _) => Err(EvalError::TypeMismatch(format!(
                        "Expected a float or vector; got a function"
                    ))),
                },
                LongleafValue::FloatList(a) => match b {
                    LongleafValue::Float(b) => Ok(LongleafValue::FloatList(vector_scalar!($op, a, b, $store))),
                    LongleafValue::FloatList(b) => Ok(LongleafValue::FloatList(vector_vector!($op, a, b, $store))),
                    LongleafValue::FunctionDefinition(_, _) => Err(EvalError::TypeMismatch(format!(
                        "Expected a float or vector; got a function"
                    ))),
                },
                LongleafValue::FunctionDefinition(_, _) => Err(EvalError::TypeMismatch(format!(
                    "Expected a float or vector; got a function"
                ))),
            }
        }
    };
}

macro_rules! scalar_vector {
    ($op:tt, $a:expr, $b:expr, $store:expr) => {
        {
            let a: f64 = $a;
            let b: &Vec<f64> = &**$b; // TODO: :rust-triggered:

            let mut out = ($store).get_vector(b.len());
            assert_eq!(out.len(), b.len());

            let out_slice: &mut [f64] = &mut out;

                use rayon::prelude::*;

                out_slice.par_iter_mut().zip(b.par_iter()).with_min_len(PAR_CHUNK_LEN).for_each(|(o, bi)| {
                    *o = a $op bi;
                });

            out.into()
        }
    };
}

macro_rules! vector_scalar {
    ($op:tt, $a:expr, $b:expr, $store:expr) => {
        {
            let a: &Vec<f64> = &**$a; // TODO: ugh
            let b: f64 = $b;

            let mut out = ($store).get_vector(a.len());
            assert_eq!(out.len(), a.len());

            let out_slice: &mut [f64] = &mut out;


                use rayon::prelude::*;

                out_slice.par_iter_mut().zip(a.par_iter()).with_min_len(PAR_CHUNK_LEN).for_each(|(o, ai)| {
                    *o = ai $op b;
                });

            out.into()
        }
    };
}

macro_rules! vector_vector {
    ($op:tt, $a:expr, $b:expr, $store:expr) => {
        {
            let a: &Vec<f64> = &**$a; // TODO: ugh
            let b: &Vec<f64> = &**$b; // TODO: :rust-triggered:

            if a.len() != b.len() {
                return Err(EvalError::DimensionMismatch(format!("Vector arguments must have same length; got {} and {}", a.len(), b.len())));
            }

            let mut out = ($store).get_vector(a.len());
            assert_eq!(out.len(), a.len());

            let out_slice: &mut [f64] = &mut out;

                use rayon::prelude::*;

                out_slice.par_iter_mut().zip(a.par_iter()).zip(b.par_iter()).with_min_len(PAR_CHUNK_LEN).for_each(|((o, ai), bi)| {
                    *o = ai $op bi;
                });

            out.into()
        }
    };
}
