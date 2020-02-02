use criterion::{black_box, criterion_group, criterion_main, BatchSize, Criterion};
use rayon;

use longleaf_lib::{parser, vm::VM};

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
    vm.evaluate_statement(to_stmt("x = range(0, 1000, 0.001);"))
        .unwrap();
    vm
}

fn range_test(lshift: usize) {
    exec(&["x = range(0, 1000, 0.001);"], 1 << lshift);
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

fn criterion_benchmark(c: &mut Criterion) {
    rayon::ThreadPoolBuilder::new()
        .num_threads(1)
        .build_global()
        .unwrap();

    c.bench_function("range_st", |b| b.iter(|| range_test(black_box(32))));
    c.bench_function("fourAdds_st", make_batched!(four_adds_test));
    c.bench_function("fourNegs_st", make_batched!(four_negs));
    c.bench_function("fourScalarMults_st", make_batched!(four_scalar_mults));
    c.bench_function("fourSines_st", make_batched!(four_sines));
    c.bench_function("bigSum_st", make_batched!(big_sum));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
