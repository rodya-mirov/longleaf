Two forms of the benchmarks are done (unscientific):
- `cargo build --release --features="timing" && cat perf/bench.leaf | ./target/release/longleaf > perf/bench.results.serial`
- `cargo build --release --features="parallel,timing" && cat perf/bench.leaf | ./target/release/longleaf > perf/bench.results.parallel`

This was used to tune the chunk length (which turned out not to be very important) and to
make sure it was more performant than the serial version (which, it is, yay).

Moving forward we'll need to make sure the CI runs tests, etc. against both feature sets