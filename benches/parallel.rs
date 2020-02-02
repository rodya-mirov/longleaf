use criterion::{black_box, criterion_group, criterion_main, BatchSize, Criterion};
use rayon;

use longleaf::{parser, vm::VM};

const RANGE_SETUP: &str = "x = range(0, 10000, 0.001);";

fn to_stmt(line: &str) -> parser::StatementNode {
    match parser::parse_repl_input(line).unwrap() {
        parser::ReplInput::Statement(stmt) => stmt,
        _ => panic!(),
    }
}

fn exec(lines: &[&str], size: usize) {
    let mut vm = VM::new(size);

    for line in lines {
        let stmt = to_stmt(line);
        vm.evaluate_statement(stmt).unwrap();
    }
}

fn setup_vm() -> VM {
    let mut vm = VM::new(1 << 32);
    vm.evaluate_statement(to_stmt(RANGE_SETUP)).unwrap();
    vm
}

fn range_test(lshift: usize) {
    exec(&[RANGE_SETUP], 1 << lshift);
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
        ($call:ident) => {
            |b| {
                b.iter_batched_ref(
                    setup_vm,
                    |mut machine| $call(&mut machine),
                    BatchSize::LargeInput,
                )
            }
        };
    }
}

fn bench_set(c: &mut Criterion, num_threads: usize) {
    let mut group = c.benchmark_group(format!("{} threads", num_threads));

    let runner = group.sample_size(20);

    let make_name = |name| format!("{}_t{}", name, num_threads);

    rayon::ThreadPoolBuilder::new()
        .num_threads(num_threads)
        .build_global()
        .unwrap();

    runner.bench_function(&make_name("range"), |b| {
        b.iter(|| range_test(black_box(32)))
    });
    runner.bench_function(&make_name("four_adds"), make_batched!(four_adds_test));
    runner.bench_function(&make_name("four_negs"), make_batched!(four_negs));
    runner.bench_function(
        &make_name("four_scalar_mults"),
        make_batched!(four_scalar_mults),
    );
    runner.bench_function(&make_name("four_sines"), make_batched!(four_sines));
    runner.bench_function(&make_name("big_sum"), make_batched!(big_sum));
}

fn criterion_benchmark(c: &mut Criterion) {
    for num_threads in &[4] {
        bench_set(c, *num_threads);
    }
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
