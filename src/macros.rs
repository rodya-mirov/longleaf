#[macro_export]
macro_rules! timed {
    ($to_eval:expr) => {{
        #[cfg(feature = "timing")]
        {
            use std::time::Instant;

            let start = Instant::now();

            let out = $to_eval;

            let dur = start.elapsed();

            println!("Took {} ms", dur.as_millis());

            out
        }

        #[cfg(not(feature = "timing"))]
        {
            $to_eval
        }
    }};
}
