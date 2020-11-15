use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use rdl_parse::num::BigUint;
use std::any::Any;
use std::ops::{Add, Mul};

#[inline]
fn some_math_copy<T: Add<Output = T> + Mul<Output = T> + Copy>(n: T) -> T {
    (n + n) * n
}

#[inline]
fn some_math_clone<'a, T: Add<&'a T, Output = T> + Mul<&'a T, Output = T> + Clone>(n: &'a T) -> T {
    T::mul(T::add(n.clone(), &n), &n)
}

#[inline]
fn some_math_any<T: Any>(n: &T) -> Option<Box<dyn Any>> {
    let n_any = n as &dyn Any;
    match n_any.downcast_ref::<u64>() {
        Some(n) => Some(Box::new((n + n) * n)),
        None => match n_any.downcast_ref::<BigUint>() {
            Some(n) => Some(Box::new((n + n) * n)),
            None => None,
        },
    }
}

#[derive(Clone)]
enum Enum {
    Big(BigUint),
    U64(u64),
}
impl Add<&Self> for Enum {
    type Output = Self;
    #[inline]
    fn add(self, other: &Self) -> Self::Output {
        match (self, other) {
            (Enum::Big(ref i), Enum::Big(ref other_i)) => Enum::Big(i + other_i),
            (Enum::Big(ref i), Enum::U64(ref other_i)) => Enum::Big(i + other_i),

            (Enum::U64(i), Enum::Big(ref other_i)) => Enum::Big(i + other_i),
            (Enum::U64(i), Enum::U64(ref other_i)) => Enum::U64(i + other_i),
        }
    }
}
impl Mul<&Self> for Enum {
    type Output = Self;
    #[inline]
    fn mul(self, other: &Self) -> Self::Output {
        match (self, other) {
            (Enum::Big(ref i), Enum::Big(ref other_i)) => Enum::Big(i * other_i),
            (Enum::Big(ref i), Enum::U64(ref other_i)) => Enum::Big(i * other_i),

            (Enum::U64(i), Enum::Big(ref other_i)) => Enum::Big(i * other_i),
            (Enum::U64(i), Enum::U64(ref other_i)) => Enum::U64(i * other_i),
        }
    }
}

fn bench_fibs(c: &mut Criterion) {
    let mut group = c.benchmark_group("Add");
    for i in [21u64].iter() {
        let big_i = BigUint::from(*i);
        let e_big = Enum::Big(big_i.clone());
        let e_64 = Enum::U64(*i);

        group.bench_with_input(BenchmarkId::new("EBigUint", i), &e_big, |b, i| {
            b.iter(|| some_math_clone(i))
        });
        group.bench_with_input(BenchmarkId::new("Eu64", i), &e_64, |b, i| {
            b.iter(|| some_math_clone(i))
        });

        group.bench_with_input(BenchmarkId::new("BigUint", i), &big_i, |b, i| {
            b.iter(|| some_math_clone(i))
        });
        group.bench_with_input(BenchmarkId::new("u64Clone", i), i, |b, i| {
            b.iter(|| some_math_clone(i))
        });
        group.bench_with_input(BenchmarkId::new("u64", i), i, |b, i| {
            b.iter(|| some_math_copy(*i))
        });

        group.bench_with_input(BenchmarkId::new("AnyBigUint", i), &big_i, |b, i| {
            b.iter(|| some_math_any(i))
        });
        group.bench_with_input(BenchmarkId::new("Anyu64", i), i, |b, i| {
            b.iter(|| some_math_any(i))
        });
    }
    group.finish();
}

criterion_group!(benches, bench_fibs);
criterion_main!(benches);
