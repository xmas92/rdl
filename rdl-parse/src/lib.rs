#![feature(arc_new_cyclic)]
#![feature(map_first_last)]
#![feature(or_patterns)]
#![feature(bindings_after_at)]

pub mod grammar;

#[macro_use]
extern crate lazy_static;

#[macro_use]
extern crate im;
#[macro_use]
mod list;
pub mod runtime;

pub mod error;

//pub mod intrinsic_move;

mod intrinsic;

pub use num;

#[cfg(test)]
mod tests {
    #[test]
    fn lib_test() {
        assert_eq!(2 + 2, 4);
    }
}
