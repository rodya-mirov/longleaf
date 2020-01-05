macro_rules! unary_switcher {
    ($op:tt, $a:expr, $store:expr) => {
        {
            let a: EvalValue = $a;

            match a {
                EvalValue::Float(a) => EvalValue::Float($op a),
                EvalValue::FloatList(a) => EvalValue::FloatList(vector!($op, a, $store)),
            }
        }
    }
}

macro_rules! vector {
    ($op:tt, $a:expr, $store:expr) => {
        {
            let a: Box<dyn FloatListValue> = $a;
            let a: &Vec<f64> = &**a; // TODO: for real?

            let mut out = ($store).get_vector(a.len());
            assert_eq!(out.len(), a.len());

            for i in 0 .. a.len() {
                out[i] = $op a[i];
            }

            Box::new(out)
        }
    };
}

macro_rules! binary_switcher {
    ($op:tt, $a:expr, $b:expr, $store:expr) => {
        {
            let a: EvalValue = $a;
            let b: EvalValue = $b;

            match a {
                EvalValue::Float(a) => match b {
                    EvalValue::Float(b) => EvalValue::Float(a $op b),
                    EvalValue::FloatList(b) => EvalValue::FloatList(scalar_vector!($op, a, b, $store)),
                },
                EvalValue::FloatList(a) => match b {
                    EvalValue::Float(b) => EvalValue::FloatList(vector_scalar!($op, a, b, $store)),
                    EvalValue::FloatList(b) => EvalValue::FloatList(vector_vector!($op, a, b, $store)),
                },
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

            for i in 0 .. b.len() {
                out[i] = a $op b[i];
            }

            Box::new(out)
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

            for i in 0 .. a.len() {
                out[i] = a[i] $op b;
            }

            Box::new(out)
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

            for i in 0 .. a.len() {
                out[i] = a[i] $op b[i];
            }

            Box::new(out)
        }
    };
}
