macro_rules! unary_fn_switcher {
    ($func:expr, $a:expr, $store:expr) => {{
        let a: EvalValue = $a;
        let func = $func;

        match a {
            EvalValue::Float(a) => EvalValue::Float(func(a)),
            EvalValue::FloatList(a) => EvalValue::FloatList(unary_fn_vector!(func, a, $store)),
        }
    }};
}

macro_rules! unary_fn_vector {
    ($func:expr, $a:expr, $store:expr) => {{
        let a: Box<dyn FloatListValue> = $a;
        let a: &Vec<f64> = &**a; // TODO: for real?

        let func = $func;

        let mut out = ($store).get_vector(a.len());
        assert_eq!(out.len(), a.len());

        for i in 0..a.len() {
            out[i] = func(a[i]);
        }

        Box::new(out)
    }};
}

macro_rules! unary_switcher {
    ($op:tt, $a:expr, $store:expr) => {
        {
            let a: EvalValue = $a;

            match a {
                EvalValue::Float(a) => EvalValue::Float($op a),
                EvalValue::FloatList(a) => EvalValue::FloatList(unary_op_vector!($op, a, $store)),
            }
        }
    }
}

macro_rules! unary_op_vector {
    ($op:tt, $a:expr, $store:expr) => {
        {
            let a: Box<dyn FloatListValue> = $a;
            let a: &[f64] = &**a; // TODO: for real?

            let mut out = ($store).get_vector(a.len());
            assert_eq!(out.len(), a.len());

            let out_slice: &mut [f64] = &mut out;

            #[cfg(feature = "parallel")]
            {
                use rayon::prelude::*;

                let len: usize = out_slice.len();
                // let out_ptr: SyncMutSlice<'_> = SyncMutSlice { slice: out_slice };
                let out_ptr: SyncMutPointer<f64> = SyncMutPointer::from(out_slice.as_mut_ptr());

                (0 ..len).step_by(PAR_CHUNK_LEN).collect::<Vec<usize>>().into_par_iter().for_each(|chunk_start| {
                    // println!("Chunk starting with {}, executed by thread {:?}", chunk_start, rayon::current_thread_index());
                    let chunk_end = std::cmp::min(len, chunk_start + PAR_CHUNK_LEN);
                    //let out: &mut [f64] = out_ptr.slice;
                    let out: &mut [f64] = unsafe { std::slice::from_raw_parts_mut(out_ptr.ptr, len) };

                    for i in chunk_start .. chunk_end {
                        out[i] = $op a[i];
                    }
                });
            }

            #[cfg(not(feature = "parallel"))]
            {
                for i in 0 .. a.len() {
                    out_slice[i] = $op a[i];
                }
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

            let out_slice: &mut [f64] = &mut out;

            #[cfg(not(feature = "parallel"))]
            {
                for i in 0 .. b.len() {
                    out_slice[i] = a $op b[i];
                }
            }

            #[cfg(feature = "parallel")]
            {
                use rayon::prelude::*;

                let len: usize = out_slice.len();
                // let out_ptr: SyncMutSlice<'_> = SyncMutSlice { slice: out_slice };
                let out_ptr: SyncMutPointer<f64> = SyncMutPointer::from(out_slice.as_mut_ptr());

                (0 ..len).step_by(PAR_CHUNK_LEN).collect::<Vec<usize>>().into_par_iter().for_each(|chunk_start| {
                    // println!("Chunk starting with {}, executed by thread {:?}", chunk_start, rayon::current_thread_index());
                    let chunk_end = std::cmp::min(len, chunk_start + PAR_CHUNK_LEN);
                    //let out: &mut [f64] = out_ptr.slice;
                    let out: &mut [f64] = unsafe { std::slice::from_raw_parts_mut(out_ptr.ptr, len) };

                    for i in chunk_start .. chunk_end {
                        out[i] = a $op b[i];
                    }
                });
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

            let out_slice: &mut [f64] = &mut out;

            #[cfg(not(feature = "parallel"))]
            {
                for i in 0 .. a.len() {
                    out_slice[i] = a[i] $op b;
                }
            }

            #[cfg(feature = "parallel")]
            {
                use rayon::prelude::*;

                let len: usize = out_slice.len();
                // let out_ptr: SyncMutSlice<'_> = SyncMutSlice { slice: out_slice };
                let out_ptr: SyncMutPointer<f64> = SyncMutPointer::from(out_slice.as_mut_ptr());

                (0 ..len).step_by(PAR_CHUNK_LEN).collect::<Vec<usize>>().into_par_iter().for_each(|chunk_start| {
                    // println!("Chunk starting with {}, executed by thread {:?}", chunk_start, rayon::current_thread_index());
                    let chunk_end = std::cmp::min(len, chunk_start + PAR_CHUNK_LEN);
                    //let out: &mut [f64] = out_ptr.slice;
                    let out: &mut [f64] = unsafe { std::slice::from_raw_parts_mut(out_ptr.ptr, len) };

                    for i in chunk_start .. chunk_end {
                        out[i] = a[i] $op b;
                    }
                });
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

            let out_slice: &mut [f64] = &mut out;

            #[cfg(not(feature = "parallel"))]
            {
                for i in 0 .. b.len() {
                    out_slice[i] = a[i] $op b[i];
                }
            }

            #[cfg(feature = "parallel")]
            {
                use rayon::prelude::*;

                let len: usize = out_slice.len();
                // let out_ptr: SyncMutSlice<'_> = SyncMutSlice { slice: out_slice };
                let out_ptr: SyncMutPointer<f64> = SyncMutPointer::from(out_slice.as_mut_ptr());

                (0 ..len).step_by(PAR_CHUNK_LEN).collect::<Vec<usize>>().into_par_iter().for_each(|chunk_start| {
                    // println!("Chunk starting with {}, executed by thread {:?}", chunk_start, rayon::current_thread_index());
                    let chunk_end = std::cmp::min(len, chunk_start + PAR_CHUNK_LEN);
                    //let out: &mut [f64] = out_ptr.slice;
                    let out: &mut [f64] = unsafe { std::slice::from_raw_parts_mut(out_ptr.ptr, len) };

                    for i in chunk_start .. chunk_end {
                        out[i] = a[i] $op b[i];
                    }
                });
            }

            Box::new(out)
        }
    };
}
