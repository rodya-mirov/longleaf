use criterion::{criterion_group, criterion_main, BatchSize, Criterion};

use longleaf::{parser, vm::VM};

const RANGE_SETUP: &str = "x = range(0, 10000, 0.001);";

fn to_stmt(line: &str) -> parser::StatementNode {
    match parser::parse_repl_input(line).unwrap() {
        parser::ReplInput::Statement(stmt) => stmt,
        _ => panic!(),
    }
}

fn create_vm(num_threads: usize) -> VM {
    VM::new(1 << 32, num_threads)
}

fn setup_vm(num_threads: usize) -> VM {
    let mut vm = create_vm(num_threads);
    vm.evaluate_statement(to_stmt(RANGE_SETUP)).unwrap();
    vm
}

fn range_test(vm: &mut VM) {
    vm.evaluate_statement(to_stmt(RANGE_SETUP)).unwrap();
}

fn four_adds_test(vm: &mut VM) {
    vm.evaluate_statement(to_stmt("ans = x+x+x+x+x;")).unwrap();
}

fn four_scalar_mults(vm: &mut VM) {
    vm.evaluate_statement(to_stmt("ans = (((x*3)*4)*5)*6;"))
        .unwrap();
}

fn four_negs(vm: &mut VM) {
    vm.evaluate_statement(to_stmt("ans = ----x;")).unwrap();
}

fn four_sines(vm: &mut VM) {
    vm.evaluate_statement(to_stmt("ans = sin(sin(sin(sin(x))));"))
        .unwrap();
}

fn big_sum(vm: &mut VM) {
    vm.evaluate_statement(to_stmt("ans = sum(x);")).unwrap();
}

#[macro_use]
mod helpers {
    macro_rules! make_batched {
        ($call:ident, $num_threads:expr) => {{
            |b| {
                b.iter_batched_ref(
                    || setup_vm($num_threads),
                    |mut machine| $call(&mut machine),
                    BatchSize::LargeInput,
                )
            }
        }};
    }
}

fn bench_set(c: &mut Criterion, num_threads: usize) {
    let mut group = c.benchmark_group(format!("{} threads", num_threads));

    let runner = group.sample_size(20);

    let make_name = |name| format!("{}_t{}", name, num_threads);

    runner.bench_function(&make_name("range"), |b| {
        b.iter_batched_ref(
            // Note "create_vm" creates it but doesn't preload it with a big vector under x;
            // this benchmarks the creation of that vector
            || create_vm(num_threads),
            |mut machine| range_test(&mut machine),
            BatchSize::LargeInput,
        )
    });
    runner.bench_function(
        &make_name("four_adds"),
        make_batched!(four_adds_test, num_threads),
    );
    runner.bench_function(
        &make_name("four_negs"),
        make_batched!(four_negs, num_threads),
    );
    runner.bench_function(
        &make_name("four_scalar_mults"),
        make_batched!(four_scalar_mults, num_threads),
    );
    runner.bench_function(
        &make_name("four_sines"),
        make_batched!(four_sines, num_threads),
    );
    runner.bench_function(&make_name("big_sum"), make_batched!(big_sum, num_threads));
}

fn criterion_benchmark(c: &mut Criterion) {
    for num_threads in &[1, 4] {
        bench_set(c, *num_threads);
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
