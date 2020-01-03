macro_rules! unary_switcher {
    ($op:tt, $a:expr) => {
        {
            let a: Value = $a;

            match a.as_ref() {
                Float(a) => Float($op a),
                FloatList(a) => FloatList(vector!($op, a)),
            }
        }
    }
}

macro_rules! vector {
    ($op:tt, $a:expr) => {
        {
            let a: &[f64] = $a;

            let mut out = Vec::with_capacity(a.len());

            for a in a {
                out.push($op a);
            }

            out
        }
    };
}

macro_rules! binary_switcher {
    ($op:tt, $a:expr, $b:expr) => {
        {
            let a: Value = $a;
            let b: Value = $b;

            match a.as_ref() {
                Float(a) => match b.as_ref() {
                    Float(b) => Float(a $op b),
                    FloatList(b) => FloatList(scalar_vector!($op, *a, b)),
                },
                FloatList(a) => match b.as_ref() {
                    Float(b) => FloatList(vector_scalar!($op, a, *b)),
                    FloatList(b) => FloatList(vector_vector!($op, a, b)),
                },
            }
        }
    };
}

macro_rules! scalar_vector {
    ($op:tt, $a:expr, $b:expr) => {
        {
            let a: f64 = $a;
            let b: &[f64] = $b;

            let mut out = Vec::with_capacity(b.len());

            for b in b {
                out.push(a $op b);
            }

            out
        }
    };
}

macro_rules! vector_scalar {
    ($op:tt, $a:expr, $b:expr) => {
        {
            let a: &[f64] = $a;
            let b: f64 = $b;

            let mut out = Vec::with_capacity(a.len());

            for a in a {
                out.push(a $op b);
            }

            out
        }
    };
}

macro_rules! vector_vector {
    ($op:tt, $a:expr, $b:expr) => {
        {
            let a: &[f64] = $a;
            let b: &[f64] = $b;

            if a.len() != b.len() {
                return Err(EvalError::DimensionMismatch(format!("Vector arguments must have same length; got {} and {}", a.len(), b.len())));
            }

            let mut out = Vec::with_capacity(a.len());

            for i in 0 .. a.len() {
                out.push(a[i] $op b[i]);
            }

            out
        }
    };
}
