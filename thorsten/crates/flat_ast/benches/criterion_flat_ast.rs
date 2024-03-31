use criterion::{black_box, criterion_group, criterion_main, Criterion};
use flat_ast::flat_eval::eval;

fn criterion_benchmark(c: &mut Criterion) {
    let mut group = c.benchmark_group("eval");
    group.bench_function("flat_ast", |b| {
        b.iter(|| {
            let input = r#"
            let fibo = fn(x) {
                if ( x < 1 ) { return 0 }
                if ( x < 2 ) { return 1 }
                return fibo(x - 1) + fibo(x - 2)
            }
            fibo(10)
            "#;
            black_box(eval(input));
        })
    });
    group.finish();
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
