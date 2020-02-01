macro_rules! impl_float_unary {
    ($kind:ident, $name:expr, $func:expr) => {
        #[derive(Eq, PartialEq, Copy, Clone, Debug)]
        pub struct $kind;

        impl Operation for $kind {
            fn num_args(&self) -> usize {
                1
            }

            fn name(&self) -> &'static str {
                $name
            }

            fn process(
                &self,
                args: Vec<LongleafValue>,
                store: &mut VectorStore,
            ) -> VmResult<LongleafValue> {
                float_unary!(self.name(), args, $func, store)
            }
        }
    };
}

macro_rules! float_unary {
    ($name:expr, $args:expr, $func:expr, $store:expr) => {{
        let arg = get_only_arg($name, $args)?;

        let type_name = arg.type_name();

        match arg {
            LongleafValue::Float(f) => Ok(LongleafValue::Float($func(f))),
            LongleafValue::FloatList(vals) => Ok(LongleafValue::FloatList(unary_fn_vector!(
                $func, vals, $store
            ))),
            _ => Err(EvalError::TypeMismatch(format!(
                "{} expects a float-like argument; got {}",
                $name, type_name
            ))),
        }
    }};
}

macro_rules! unary_fn_vector {
    ($func:expr, $a:expr, $store:expr) => {{
        let a: &[f64] = &*$a;

        let func = $func;

        let store = $store;

        let mut out = store.get_vector(a.len())?;
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

macro_rules! impl_float_binary {
    ($kind:ident, $name:expr, $func:expr) => {
        #[derive(Eq, PartialEq, Copy, Clone, Debug)]
        pub struct $kind;

        impl Operation for $kind {
            fn num_args(&self) -> usize {
                2
            }

            fn name(&self) -> &'static str {
                $name
            }

            fn process(
                &self,
                args: Vec<LongleafValue>,
                store: &mut VectorStore,
            ) -> VmResult<LongleafValue> {
                float_binary!(self.name(), args, $func, store)
            }
        }
    };
}

macro_rules! float_binary {
    ($name:expr, $args:expr, $func:expr, $store:expr) => {{
        let (a, b) = get_two_args($name, $args)?;

        let type_a = a.type_name();
        let type_b = b.type_name();

        let make_err = || {
            EvalError::TypeMismatch(format!(
                "{} expects two float-like arguments; got {} and {}",
                $name, type_a, type_b
            ))
        };

        match a {
            LongleafValue::Float(a) => match b {
                LongleafValue::Float(b) => Ok(LongleafValue::Float($func(a, b))),
                LongleafValue::FloatList(b) => Ok(LongleafValue::FloatList(scalar_vector!(
                    $func, a, b, $store
                ))),
                _ => Err(make_err()),
            },
            LongleafValue::FloatList(a) => match b {
                LongleafValue::Float(b) => Ok(LongleafValue::FloatList(vector_scalar!(
                    $func, a, b, $store
                ))),
                LongleafValue::FloatList(b) => Ok(LongleafValue::FloatList(vector_vector!(
                    $func, a, b, $store
                ))),
                _ => Err(make_err()),
            },
            _ => Err(make_err()),
        }
    }};
}

macro_rules! scalar_vector {
    ($op:expr, $a:expr, $b:expr, $store:expr) => {{
        let a: f64 = $a;
        let b: &[f64] = &*$b;

        let mut out = ($store).get_vector(b.len())?;
        assert_eq!(out.len(), b.len());

        let out_slice: &mut [f64] = &mut out;

        use rayon::prelude::*;

        out_slice
            .par_iter_mut()
            .zip(b.par_iter())
            .with_min_len(PAR_CHUNK_LEN)
            .for_each(|(o, bi)| {
                *o = $op(a, bi);
            });

        out.into()
    }};
}

macro_rules! vector_scalar {
    ($op:tt, $a:expr, $b:expr, $store:expr) => {{
        let a: &[f64] = &*$a;
        let b: f64 = $b;

        let mut out = ($store).get_vector(a.len())?;
        assert_eq!(out.len(), a.len());

        let out_slice: &mut [f64] = &mut out;

        use rayon::prelude::*;

        out_slice
            .par_iter_mut()
            .zip(a.par_iter())
            .with_min_len(PAR_CHUNK_LEN)
            .for_each(|(o, &ai)| {
                *o = $op(ai, b);
            });

        out.into()
    }};
}

macro_rules! vector_vector {
    ($op:tt, $a:expr, $b:expr, $store:expr) => {{
        let a: &[f64] = &*$a;
        let b: &[f64] = &*$b;

        if a.len() != b.len() {
            return Err(EvalError::DimensionMismatch(format!(
                "Vector arguments must have same length; got {} and {}",
                a.len(),
                b.len()
            )));
        }

        let mut out = ($store).get_vector(a.len())?;
        assert_eq!(out.len(), a.len());

        let out_slice: &mut [f64] = &mut out;

        use rayon::prelude::*;

        out_slice
            .par_iter_mut()
            .zip(a.par_iter())
            .zip(b.par_iter())
            .with_min_len(PAR_CHUNK_LEN)
            .for_each(|((o, &ai), bi)| {
                *o = $op(ai, bi);
            });

        out.into()
    }};
}
