use std::sync::Arc;

use im::{HashMap, HashSet, Vector};

use crate::{
    error::{ArityError, GeneralError, RuntimeError},
    list::List,
    runtime::{Context, RuntimeResult, RuntimeValue},
};

macro_rules! intrinsic_function_internal {
    (() $function_block:block) => {
        pub fn internal0() -> RuntimeResult {
            $function_block
        }
    };
    (($arg0:ident) $function_block:block) => {
        pub fn internal1($arg0: &RuntimeValue) -> RuntimeResult {
            $function_block
        }
    };
    (($arg0:ident, $arg1:ident) $function_block:block) => {
        pub fn internal2($arg0: &RuntimeValue, $arg1: &RuntimeValue) -> RuntimeResult {
            $function_block
        }
    };
    (($arg0:ident, $arg1:ident, $arg2:ident) $function_block:block) => {
        pub fn internal3(
            $arg0: &RuntimeValue,
            $arg1: &RuntimeValue,
            $arg2: &RuntimeValue,
        ) -> RuntimeResult {
            $function_block
        }
    };
    (($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident) $function_block:block) => {
        pub fn internal4(
            $arg0: &RuntimeValue,
            $arg1: &RuntimeValue,
            $arg2: &RuntimeValue,
            $arg3: &RuntimeValue,
        ) -> RuntimeResult {
            $function_block
        }
    };
    (($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident, $arg4:ident) $function_block:block) => {
        pub fn internal5(
            $arg0: &RuntimeValue,
            $arg1: &RuntimeValue,
            $arg2: &RuntimeValue,
            $arg3: &RuntimeValue,
            $arg4: &RuntimeValue,
        ) -> RuntimeResult {
            $function_block
        }
    };
    (($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident, $arg4:ident, $arg5:ident) $function_block:block) => {
        pub fn internal6(
            $arg0: &RuntimeValue,
            $arg1: &RuntimeValue,
            $arg2: &RuntimeValue,
            $arg3: &RuntimeValue,
            $arg4: &RuntimeValue,
            $arg5: &RuntimeValue,
        ) -> RuntimeResult {
            $function_block
        }
    };
}

macro_rules! intrinsic_function_builder {
    ($args:tt $function_block:block) => {
        #[inline(always)]
        intrinsic_function_internal!($args $function_block);
    };
}

macro_rules! intrinsic_function_matcher_arity {
    (()) => {
        0
    };
    (($arg0:ident)) => {
        1
    };
    (($arg0:ident, $arg1:ident)) => {
        2
    };
    (($arg0:ident, $arg1:ident, $arg2:ident)) => {
        3
    };
    (($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident)) => {
        4
    };
    (($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident, $arg4:ident)) => {
        5
    };
    (($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident, $arg4:ident, $arg5:ident)) => {
        6
    };
}

macro_rules! intrinsic_function_matcher_internal_call {
    ($name:ident,$values:expr, ()) => {
        $name::internal0()
    };
    ($name:ident,$values:expr, ($arg0:ident)) => {
        $name::internal1(&$values[0])
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident)) => {
        $name::internal2(&$values[0], &$values[1])
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident, $arg2:ident)) => {
        $name::internal3(&$values[0], &$values[1], &$values[2])
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident)) => {
        $name::internal4(&$values[0], &$values[1], &$values[2], &$values[3])
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident, $arg4:ident)) => {
        $name::internal5(
            &$values[0],
            &$values[1],
            &$values[2],
            &$values[3],
            &$values[4],
        )
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident, $arg4:ident, $arg5:ident)) => {
        $name::internal6(
            &$values[0],
            &$values[1],
            &$values[2],
            &$values[3],
            &$values[4],
            &$values[5],
        )
    };
}

macro_rules! intrinsic_function_matcher_internal_variadic_arg {
    ($values:expr, $n:expr) => {
        &if $values.len() > $n {
            RuntimeValue::Vector(Box::new($values.skip($n)))
        } else {
            RuntimeValue::None
        }
    };
}

macro_rules! intrinsic_function_matcher_internal_variadic_call {
    ($name:ident,$values:expr, ($arg0:ident)) => {
        $name::internal1(intrinsic_function_matcher_internal_variadic_arg!(
            $values, 0
        ))
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident)) => {
        $name::internal2(
            &$values[0],
            intrinsic_function_matcher_internal_variadic_arg!($values, 1),
        )
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident, $arg2:ident)) => {
        $name::internal3(
            &$values[0],
            &$values[1],
            intrinsic_function_matcher_internal_variadic_arg!($values, 2),
        )
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident)) => {
        $name::internal4(
            &$values[0],
            &$values[1],
            &$values[2],
            intrinsic_function_matcher_internal_variadic_arg!($values, 3),
        )
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident, $arg4:ident)) => {
        $name::internal5(
            &$values[0],
            &$values[1],
            &$values[2],
            &$values[3],
            intrinsic_function_matcher_internal_variadic_arg!($values, 4),
        )
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident, $arg4:ident, $arg5:ident)) => {
        $name::internal6(
            &$values[0],
            &$values[1],
            &$values[2],
            &$values[3],
            &$values[4],
            intrinsic_function_matcher_internal_variadic_arg!($values, 5),
        )
    };
}

macro_rules! intrinsic_function_matcher {
    ($name:ident, $values:expr, $n:expr, function $args:tt) => {
        if $n == intrinsic_function_matcher_arity!($args) {
            intrinsic_function_matcher_internal_call!($name, $values, $args)
        } else {
            Err(RuntimeError::new(ArityError::new($n, String::from(stringify!($name)))))
        }
    };
    ($name:ident, $values:expr, $n:expr, function $args:tt $($types:ident $rest:tt)+) => {
        if $n == intrinsic_function_matcher_arity!($args) {
            intrinsic_function_matcher_internal_call!($name, $values, $args)
        } else {
            intrinsic_function_matcher!($name, $values, $n, $($types $rest)+)
        }
    };
    ($name:ident, $values:expr, $n:expr, variadic $args:tt) => {
        if $n >= intrinsic_function_matcher_arity!($args)-1 {
            intrinsic_function_matcher_internal_variadic_call!($name, $values, $args)
        } else {
            Err(RuntimeError::new(ArityError::new($n, String::from(stringify!($name)))))
        }
    };
    // Move variadic last as it has lowest priority. Only one variadic function allowed.
    ($name:ident, $values:expr, $n:expr, variadic $args:tt $($types:ident $rest:tt)+) => {
        intrinsic_function_matcher!($name, $values, $n, $($types $rest)+ variadic $args:tt)
    };
}

macro_rules! intrinsic_function {
    ($name:ident$($type:ident $args:tt $function_block:block)+) => {
        #[allow(dead_code)]
        #[allow(non_camel_case_types)]
        pub struct $name();
        impl $name {
            #[inline]
            pub fn function() -> RuntimeValue {
                RuntimeValue::Function(
                    Arc::new(
                        move |_: Context, args: Vector<RuntimeValue>| {
                            let n = args.len();
                            intrinsic_function_matcher!($name, args, n, $($type $args)+)
                        }
                    )
                )
            }
            $(intrinsic_function_builder!($args $function_block);)+
        }
    };
}

intrinsic_function!(
    plus
    function () {
        Ok(RuntimeValue::Integer(0))
    }
    function (x) {
        match x {
            RuntimeValue::Integer(_) |
            RuntimeValue::BigInteger(_) |
            RuntimeValue::Float(_) |
            RuntimeValue::BigFloat(_) => { Ok(x.clone()) }
            _ => {Err(RuntimeError::new(GeneralError::new(format!("({:?}) is not a number", x))))}
        }
    }
    function (x, y) {
        match (x, y) {
            (RuntimeValue::Integer(n1),RuntimeValue::Integer(n2)) => {
                if let Some(n) = n1.checked_add(*n2) {
                    Ok(RuntimeValue::Integer(n))
                } else {
                    Err(RuntimeError::new(GeneralError::new(format!("Integer overflow + ({:?}, {:?})", x, y))))
                }
            }
            (RuntimeValue::BigInteger(n1),RuntimeValue::BigInteger(n2)) => {
                Ok(RuntimeValue::BigInteger(Box::new(n1.as_ref() + n2.as_ref())))
            }
            (RuntimeValue::Float(n1),RuntimeValue::Float(n2)) => {
                Ok(RuntimeValue::Float(n1 + n2))
            }
            (RuntimeValue::BigFloat(n1),RuntimeValue::BigFloat(n2)) => {
                Ok(RuntimeValue::BigFloat(Box::new(n1.as_ref() + n2.as_ref())))
            }
            _ => {Err(RuntimeError::new(GeneralError::new(format!("Type mismatch plus ({:?}, {:?})", x, y))))}
        }
    }
    variadic (x, y, more) {
        match more {
            RuntimeValue::Vector(v) => {
                v.iter().try_fold(plus::internal2(x,y)?, |x, y| plus::internal2(&x,y))
            }
            RuntimeValue::None => { plus::internal2(x, y) }
            _ => unreachable!()
        }
    }
);

intrinsic_function!(
    minus
    function () {
        Ok(RuntimeValue::Integer(0))
    }
    function (x) {
        match x {
            RuntimeValue::Integer(_) |
            RuntimeValue::BigInteger(_) |
            RuntimeValue::Float(_) |
            RuntimeValue::BigFloat(_) => { Ok(x.clone()) }
            _ => {Err(RuntimeError::new(GeneralError::new(format!("({:?}) is not a number", x))))}
        }
    }
    function (x, y) {
        match (x, y) {
            (RuntimeValue::Integer(n1),RuntimeValue::Integer(n2)) => {
                if let Some(n) = n1.checked_sub(*n2) {
                    Ok(RuntimeValue::Integer(n))
                } else {
                    Err(RuntimeError::new(GeneralError::new(format!("Integer underflow minus ({:?}, {:?})", x, y))))
                }
            }
            (RuntimeValue::BigInteger(n1),RuntimeValue::BigInteger(n2)) => {
                Ok(RuntimeValue::BigInteger(Box::new(n1.as_ref() - n2.as_ref())))
            }
            (RuntimeValue::Float(n1),RuntimeValue::Float(n2)) => {
                Ok(RuntimeValue::Float(n1 - n2))
            }
            (RuntimeValue::BigFloat(n1),RuntimeValue::BigFloat(n2)) => {
                Ok(RuntimeValue::BigFloat(Box::new(n1.as_ref() - n2.as_ref())))
            }
            _ => {Err(RuntimeError::new(GeneralError::new(format!("Type mismatch minus ({:?}, {:?})", x, y))))}
        }
    }
    variadic (x, y, more) {
        match more {
            RuntimeValue::Vector(v) => {
                v.iter().try_fold(minus::internal2(x,y)?, |x, y| minus::internal2(&x,y))
            }
            RuntimeValue::None => { minus::internal2(x, y) }
            _ => unreachable!()
        }
    }
);

intrinsic_function!(
    star
    function () {
        Ok(RuntimeValue::Integer(0))
    }
    function (x) {
        match x {
            RuntimeValue::Integer(_) |
            RuntimeValue::BigInteger(_) |
            RuntimeValue::Float(_) |
            RuntimeValue::BigFloat(_) => { Ok(x.clone()) }
            _ => {Err(RuntimeError::new(GeneralError::new(format!("({:?}) is not a number", x))))}
        }
    }
    function (x, y) {
        match (x, y) {
            (RuntimeValue::Integer(n1),RuntimeValue::Integer(n2)) => {
                if let Some(n) = n1.checked_mul(*n2) {
                    Ok(RuntimeValue::Integer(n))
                } else {
                    Err(RuntimeError::new(GeneralError::new(format!("Integer overflow star ({:?}, {:?})", x, y))))
                }
            }
            (RuntimeValue::BigInteger(n1),RuntimeValue::BigInteger(n2)) => {
                Ok(RuntimeValue::BigInteger(Box::new(n1.as_ref() * n2.as_ref())))
            }
            (RuntimeValue::Float(n1),RuntimeValue::Float(n2)) => {
                Ok(RuntimeValue::Float(n1*n2))
            }
            (RuntimeValue::BigFloat(n1),RuntimeValue::BigFloat(n2)) => {
                Ok(RuntimeValue::BigFloat(Box::new(n1.as_ref() * n2.as_ref())))
            }
            _ => {Err(RuntimeError::new(GeneralError::new(format!("Type mismatch star ({:?}, {:?})", x, y))))}
        }
    }
    variadic (x, y, more) {
        match more {
            RuntimeValue::Vector(v) => {
                v.iter().try_fold(star::internal2(x,y)?, |x, y| star::internal2(&x, y))
            }
            RuntimeValue::None => { star::internal2(x, y) }
            _ => unreachable!()
        }
    }
);
#[cfg(test)]
use num::{BigInt, BigRational};

#[test]
fn arithmetic_basic_test() {
    let int0 = RuntimeValue::Integer(0);
    let int1 = RuntimeValue::Integer(1);
    let bigint0 = RuntimeValue::BigInteger(Box::new(BigInt::from(0)));
    let bigint1 = RuntimeValue::BigInteger(Box::new(BigInt::from(1)));
    let float0 = RuntimeValue::Float(0.);
    let float1 = RuntimeValue::Float(1.);
    let bigfloat0 = RuntimeValue::BigFloat(Box::new(BigRational::from_float(0.).unwrap()));
    let bigfloat1 = RuntimeValue::BigFloat(Box::new(BigRational::from_float(1.).unwrap()));

    let plus_fn = plus::function();
    let minus_fn = minus::function();
    let star_fn = star::function();


    let args = vector![int0.clone(), int0.clone()];
    assert_eq!(int0.clone(), plus_fn.evaluate_global_context_with_args(args.clone()).unwrap());
    assert_eq!(int0.clone(), star_fn.evaluate_global_context_with_args(args.clone()).unwrap());
    assert_eq!(int0.clone(), minus_fn.evaluate_global_context_with_args(args.clone()).unwrap());

    let args = vector![int1.clone(), int0.clone()];
    assert_eq!(int1.clone(), plus_fn.evaluate_global_context_with_args(args.clone()).unwrap());
    assert_eq!(int1.clone(), minus_fn.evaluate_global_context_with_args(args.clone()).unwrap());
    assert_eq!(int0.clone(), star_fn.evaluate_global_context_with_args(args.clone()).unwrap());

    let args = vector![int1.clone(), int0.clone(), int1.clone(), int1.clone()];
    assert_eq!(RuntimeValue::Integer(3), plus_fn.evaluate_global_context_with_args(args.clone()).unwrap());
    assert_eq!(RuntimeValue::Integer(-1), minus_fn.evaluate_global_context_with_args(args.clone()).unwrap());
    assert_eq!(int0.clone(), star_fn.evaluate_global_context_with_args(args.clone()).unwrap());

    let args = vector![bigint1.clone(), bigint0.clone()];
    assert_eq!(bigint1.clone(), plus_fn.evaluate_global_context_with_args(args.clone()).unwrap());
    assert_eq!(bigint1.clone(), minus_fn.evaluate_global_context_with_args(args.clone()).unwrap());
    assert_eq!(bigint0.clone(), star_fn.evaluate_global_context_with_args(args.clone()).unwrap());

    let args = vector![float1.clone(), float0.clone()];
    assert_eq!(float1.clone(), plus_fn.evaluate_global_context_with_args(args.clone()).unwrap());
    assert_eq!(float1.clone(), minus_fn.evaluate_global_context_with_args(args.clone()).unwrap());
    assert_eq!(float0.clone(), star_fn.evaluate_global_context_with_args(args.clone()).unwrap());

    let args = vector![bigfloat1.clone(), bigfloat0.clone()];
    assert_eq!(bigfloat1.clone(), plus_fn.evaluate_global_context_with_args(args.clone()).unwrap());
    assert_eq!(bigfloat1.clone(), minus_fn.evaluate_global_context_with_args(args.clone()).unwrap());
    assert_eq!(bigfloat0.clone(), star_fn.evaluate_global_context_with_args(args.clone()).unwrap());

    let args = vector![bigfloat1.clone(), bigfloat0.clone()];
    assert_eq!(bigfloat1.clone(), plus_fn.evaluate_global_context_with_args(args.clone()).unwrap());
    assert_eq!(bigfloat1.clone(), minus_fn.evaluate_global_context_with_args(args.clone()).unwrap());
    assert_eq!(bigfloat0.clone(), star_fn.evaluate_global_context_with_args(args.clone()).unwrap());

    let args = vector![bigint1.clone(), bigint1.clone()];
    assert_eq!(bigint0.clone(), minus_fn.evaluate_global_context_with_args(args.clone()).unwrap());
    assert_eq!(bigint1.clone(), star_fn.evaluate_global_context_with_args(args.clone()).unwrap());

    let args = vector![float1.clone(), float1.clone()];
    assert_eq!(float0.clone(), minus_fn.evaluate_global_context_with_args(args.clone()).unwrap());
    assert_eq!(float1.clone(), star_fn.evaluate_global_context_with_args(args.clone()).unwrap());

    let args = vector![bigfloat1.clone(), bigfloat1.clone()];
    assert_eq!(bigfloat0.clone(), minus_fn.evaluate_global_context_with_args(args.clone()).unwrap());
    assert_eq!(bigfloat1.clone(), star_fn.evaluate_global_context_with_args(args.clone()).unwrap());
}

#[test]
fn arithmetic_error_test() {
    let int = RuntimeValue::Integer(0);
    let bigint = RuntimeValue::BigInteger(Box::new(BigInt::from(0)));
    let float = RuntimeValue::Float(0.);
    let bigfloat = RuntimeValue::BigFloat(Box::new(BigRational::from_float(0.).unwrap()));

    let plus_fn = plus::function();
    let minus_fn = minus::function();
    let star_fn = star::function();

    let args = vector![int.clone(), bigint.clone(), float.clone(), bigfloat.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_err());

    let args = vector![int.clone(), bigint.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_err());

    let args = vector![int.clone(), float.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_err());
    
    let args = vector![int.clone(), bigfloat.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_err());

    let args = vector![bigint.clone(), int.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_err());

    let args = vector![bigint.clone(), float.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_err());

    let args = vector![bigint.clone(), bigfloat.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_err());

    let args = vector![float.clone(), int.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_err());

    let args = vector![float.clone(), bigint.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_err());

    let args = vector![float.clone(), bigfloat.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_err());
    
    let args = vector![bigfloat.clone(), int.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_err());
    
    let args = vector![bigfloat.clone(), bigint.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_err());

    let args = vector![bigfloat.clone(), float.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_err());
}

#[test]
fn arithmetic_overflow_underflow_test() {
    let int0 = RuntimeValue::Integer(0);
    let int1 = RuntimeValue::Integer(1);
    let int_max = RuntimeValue::Integer(i64::MAX);
    let int_min = RuntimeValue::Integer(i64::MIN);

    let plus_fn = plus::function();
    let minus_fn = minus::function();
    let star_fn = star::function();
    
    let args = vector![int_min.clone(), int0.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_ok());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_ok());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_ok());

    let args = vector![int_min.clone(), int1.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_ok());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_ok());

    let args = vector![int_max.clone(), int0.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_ok());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_ok());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_ok());

    let args = vector![int_max.clone(), int1.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_ok());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_ok());
    
    let args = vector![int_max.clone(), int_max.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_ok());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_err());

    let args = vector![int_max.clone(), int_min.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_ok());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_err());

    let args = vector![int_min.clone(), int_min.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_ok());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_err());

    let args = vector![int_min.clone(), int_max.clone()];
    assert!(plus_fn.evaluate_global_context_with_args(args.clone()).is_ok());
    assert!(minus_fn.evaluate_global_context_with_args(args.clone()).is_err());
    assert!(star_fn.evaluate_global_context_with_args(args.clone()).is_err());
}

intrinsic_function!(
    get
    function (collection, key) {
        get::internal3(collection,key,&RuntimeValue::None)
    }
    function (collection, key, default) {
        match (collection, key) {
            (RuntimeValue::Map(m), key) => {
                if let Some(value) = m.get(key) {
                    Ok(value.clone())
                } else {
                    Ok(default.clone())
                }
            }
            (RuntimeValue::Set(s), key) => {
                if s.contains(key) {
                    Ok(key.clone())
                } else {
                    Ok(default.clone())
                }
            }
            (RuntimeValue::Vector(_) | RuntimeValue::List(_) | RuntimeValue::String(_) | RuntimeValue::Iterator(_), RuntimeValue::Integer(_)) => {
                nth::internal3(collection, key, default)
            }
            _ => {Ok(default.clone())}
        }
    }
);

#[test]
fn get_test() {
    let a = RuntimeValue::Keyword(Box::new(String::from("a")));
    let b = RuntimeValue::Keyword(Box::new(String::from("b")));
    let int0 = RuntimeValue::Integer(0);
    let int1 = RuntimeValue::Integer(1);
    let map = RuntimeValue::Map(Box::new(hashmap! {a.clone() => int0.clone(), b.clone() => int1.clone() }));
    let vec = RuntimeValue::Vector(Box::new(vector! [a.clone()]));
    let set = RuntimeValue::Set(Box::new(hashset! [a.clone(), int0.clone()]));

    let get_fn = get::function();

    let args = vector![map.clone(), int0.clone()];
    assert_eq!(RuntimeValue::None, get_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![map.clone(), int0.clone(), vec.clone()];
    assert_eq!(vec.clone(), get_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![map.clone(), a.clone()];
    assert_eq!(int0.clone(), get_fn.evaluate_global_context_with_args(args).unwrap());

    let args = vector![vec.clone(), int1.clone()];
    assert_eq!(RuntimeValue::None, get_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![vec.clone(), int1.clone(), int0.clone()];
    assert_eq!(int0.clone(), get_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![vec.clone(), int0.clone()];
    assert_eq!(a.clone(), get_fn.evaluate_global_context_with_args(args).unwrap());

    let args = vector![set.clone(), int1.clone()];
    assert_eq!(RuntimeValue::None, get_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![set.clone(), int1.clone(), int0.clone()];
    assert_eq!(int0.clone(), get_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![set.clone(), int0.clone()];
    assert_eq!(int0.clone(), get_fn.evaluate_global_context_with_args(args).unwrap());

    let args = vector![vec.clone(), a.clone()];
    assert_eq!(RuntimeValue::None, get_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![vec.clone(), a.clone(), int0.clone()];
    assert_eq!(int0.clone(), get_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![a.clone(), a.clone()];
    assert_eq!(RuntimeValue::None, get_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![a.clone(), a.clone(), int0.clone()];
    assert_eq!(int0.clone(), get_fn.evaluate_global_context_with_args(args).unwrap());
}

intrinsic_function!(
    get_or_else
    function (collection, key, else_fn) {
        match (collection, key) {
            (RuntimeValue::Map(m), key) => {
                if let Some(value) = m.get(key) {
                    Ok(value.clone())
                } else {
                    else_fn.evaluate_global_context_with_args(Vector::new())
                }
            }
            (RuntimeValue::Set(s), key) => {
                if s.contains(key) {
                    Ok(key.clone())
                } else {
                    else_fn.evaluate_global_context_with_args(Vector::new())
                }
            }
            (RuntimeValue::Vector(_) | RuntimeValue::List(_) | RuntimeValue::String(_) | RuntimeValue::Iterator(_), RuntimeValue::Integer(_)) => {
                nth_or_else::internal3(collection, key, else_fn)
            }
            _ => { else_fn.evaluate_global_context_with_args(Vector::new()) }
        }
    }
);

#[test]
fn get_or_else_test() {
    let a = RuntimeValue::Keyword(Box::new(String::from("a")));
    let b = RuntimeValue::Keyword(Box::new(String::from("b")));
    let int0 = RuntimeValue::Integer(0);
    let int1 = RuntimeValue::Integer(1);
    let map = RuntimeValue::Map(Box::new(hashmap! {a.clone() => int0.clone(), b.clone() => int1.clone() }));
    let vec = RuntimeValue::Vector(Box::new(vector! [a.clone()]));
    let set = RuntimeValue::Set(Box::new(hashset! [a.clone(), int0.clone()]));

    let else_fn = RuntimeValue::Function(Arc::new(|_,_| Ok(RuntimeValue::Integer(1))));

    let get_or_else_fn = get_or_else::function();


    let args = vector![map.clone(), int0.clone(), else_fn.clone()];
    assert_eq!(int1.clone(), get_or_else_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![map.clone(), a.clone(), else_fn.clone()];
    assert_eq!(int0.clone(), get_or_else_fn.evaluate_global_context_with_args(args).unwrap());

    let args = vector![vec.clone(), int1.clone(), else_fn.clone()];
    assert_eq!(int1.clone(), get_or_else_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![vec.clone(), int0.clone(), else_fn.clone()];
    assert_eq!(a.clone(), get_or_else_fn.evaluate_global_context_with_args(args).unwrap());

    let args = vector![set.clone(), int1.clone(), else_fn.clone()];
    assert_eq!(int1.clone(), get_or_else_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![set.clone(), int0.clone(), else_fn.clone()];
    assert_eq!(int0.clone(), get_or_else_fn.evaluate_global_context_with_args(args).unwrap());

    let args = vector![vec.clone(), a.clone(), else_fn.clone()];
    assert_eq!(int1.clone(), get_or_else_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![a.clone(), a.clone(), else_fn.clone()];
    assert_eq!(int1.clone(), get_or_else_fn.evaluate_global_context_with_args(args).unwrap());
}

intrinsic_function!(
    nth
    function (collection, index) {
        match (collection, index) {
            (RuntimeValue::Vector(v), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Index out of range {:?}", n)))))
                }
                else if let Some(v) = v.get(n as usize) {
                    Ok(v.clone())
                } else {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Index out of range {:?}", n)))))
               }
            }
            (RuntimeValue::List(l), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 || n as usize >= l.len() {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Index out of range {:?}", n)))))
                } else {
                    Ok(l.iter().nth(n as usize).unwrap())
               }
            }
            (RuntimeValue::String(s), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Index out of range {:?}", n)))))
                } else if let Some(c) = s.chars().skip(n as usize).next() {
                    Ok(RuntimeValue::Character(c))
                } else {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Index out of range {:?}", n)))))
                }
            }
            (RuntimeValue::Iterator(_), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Index out of range {:?}", n)))))
                } else {
                    match collection.evaluate_global_context_with_args(vector![n.into()])? {
                        RuntimeValue::Vector(mut v) if v.len() == 2 => {
                            match v.last().unwrap() {
                                RuntimeValue::None => Err(RuntimeError::new(GeneralError::new(String::from(format!("Index out of range {:?}", n))))),
                                _ => Ok(v.pop_front().unwrap())
                            }
                        }
                        _ => unreachable!(),
                    }
                    
                }
            }
            (RuntimeValue::Vector(_)|RuntimeValue::List(_)|RuntimeValue::String(_)|RuntimeValue::Iterator(_),_) => {
                Err(RuntimeError::new(GeneralError::new(String::from(format!("Index must be an integer")))))
            }
            _ => {Err(RuntimeError::new(GeneralError::new(String::from(format!("nth not supported for ({:?})", collection)))))}
        }
    }
    function (collection, index, default) {
        match (collection, index) {
            (RuntimeValue::Vector(v), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 {
                    Ok(default.clone())
                }
                else if let Some(v) = v.get(n as usize) {
                    Ok(v.clone())
                } else {
                    Ok(default.clone())
               }
            }
            (RuntimeValue::List(l), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 || n as usize >= l.len() {
                    Ok(default.clone())
                } else {
                    Ok(l.iter().nth(n as usize).unwrap())
               }
            }
            (RuntimeValue::String(s), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 {
                    Ok(default.clone())
                } else if let Some(c) = s.chars().skip(n as usize).next() {
                    Ok(RuntimeValue::Character(c))
                } else {
                    Ok(default.clone())
                }
            }
            (RuntimeValue::Iterator(_), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 {
                    Ok(default.clone())
                } else {
                    match collection.evaluate_global_context_with_args(vector![n.into()])? {
                        RuntimeValue::Vector(mut v) if v.len() == 2 => {
                            match v.last().unwrap() {
                                RuntimeValue::None => Ok(default.clone()),
                                _ => Ok(v.pop_front().unwrap())
                            }
                        }
                        _ => unreachable!(),
                    }
                    
                }
            }
            (RuntimeValue::Vector(_)|RuntimeValue::List(_)|RuntimeValue::String(_)|RuntimeValue::Iterator(_),_) => {
                Err(RuntimeError::new(GeneralError::new(String::from(format!("Index must be an integer")))))
            }
            _ => {Err(RuntimeError::new(GeneralError::new(String::from(format!("nth not supported for ({:?})", collection)))))}
        }
    }
);

#[test]
fn nth_test() {
    let a = RuntimeValue::Character('a');
    let b = RuntimeValue::Character('b');
    let c = RuntimeValue::Character('c');
    let int0 = RuntimeValue::Integer(0);
    let int1 = RuntimeValue::Integer(1);
    let int2 = RuntimeValue::Integer(2);
    let vec = RuntimeValue::Vector(Box::new(vector! [a.clone(), b.clone()]));
    let lst = RuntimeValue::List(Box::new(list! [a.clone(), b.clone()]));
    let string = RuntimeValue::String(Box::new(String::from("ab")));

    let nth_fn = nth::function();

    let args = vector![vec.clone(), int0.clone()];
    assert_eq!(a.clone(), nth_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![vec.clone(), int1.clone()];
    assert_eq!(b.clone(), nth_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![vec.clone(), int2.clone()];
    assert!(nth_fn.evaluate_global_context_with_args(args).is_err());
    let args = vector![vec.clone(), int2.clone(), c.clone()];
    assert_eq!(c.clone(), nth_fn.evaluate_global_context_with_args(args).unwrap());

    let args = vector![lst.clone(), int0.clone()];
    assert_eq!(a.clone(), nth_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![lst.clone(), int1.clone()];
    assert_eq!(b.clone(), nth_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![lst.clone(), int2.clone()];
    assert!(nth_fn.evaluate_global_context_with_args(args).is_err());
    let args = vector![lst.clone(), int2.clone(), c.clone()];
    assert_eq!(c.clone(), nth_fn.evaluate_global_context_with_args(args).unwrap());

    let args = vector![string.clone(), int0.clone()];
    assert_eq!(a.clone(), nth_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![string.clone(), int1.clone()];
    assert_eq!(b.clone(), nth_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![string.clone(), int2.clone()];
    assert!(nth_fn.evaluate_global_context_with_args(args).is_err());
    let args = vector![string.clone(), int2.clone(), c.clone()];
    assert_eq!(c.clone(), nth_fn.evaluate_global_context_with_args(args).unwrap());

    
    let args = vector![vec.clone(), a.clone()];
    assert!(nth_fn.evaluate_global_context_with_args(args).is_err());
    let args = vector![lst.clone(), b.clone()];
    assert!(nth_fn.evaluate_global_context_with_args(args).is_err());
    let args = vector![string.clone(), c.clone()];
    assert!(nth_fn.evaluate_global_context_with_args(args).is_err());
    let args = vector![c.clone(), c.clone()];
    assert!(nth_fn.evaluate_global_context_with_args(args).is_err());
}

intrinsic_function!(
    nth_or_else
    function (collection, index, else_fn) {
        match (collection, index) {
            (RuntimeValue::Vector(v), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 {
                    else_fn.evaluate_global_context()
                }
                else if let Some(v) = v.get(n as usize) {
                    Ok(v.clone())
                } else {
                    else_fn.evaluate_global_context_with_args(Vector::new())
               }
            }
            (RuntimeValue::List(l), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 || n as usize >= l.len() {
                    else_fn.evaluate_global_context_with_args(Vector::new())
                } else {
                    Ok(l.iter().nth(n as usize).unwrap())
               }
            }
            (RuntimeValue::String(s), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 {
                    else_fn.evaluate_global_context_with_args(Vector::new())
                } else if let Some(c) = s.chars().skip(n as usize).next() {
                    Ok(RuntimeValue::Character(c))
                } else {
                    else_fn.evaluate_global_context_with_args(Vector::new())
                }
            }
            (RuntimeValue::Iterator(_), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 {
                    else_fn.evaluate_global_context_with_args(Vector::new())
                } else {
                    match collection.evaluate_global_context_with_args(vector![n.into()])? {
                        RuntimeValue::Vector(mut v) if v.len() == 2 => {
                            match v.last().unwrap() {
                                RuntimeValue::None => else_fn.evaluate_global_context_with_args(Vector::new()),
                                _ => Ok(v.pop_front().unwrap())
                            }
                        }
                        _ => unreachable!(),
                    }
                    
                }
            }
            (RuntimeValue::Vector(_)|RuntimeValue::List(_)|RuntimeValue::String(_)|RuntimeValue::Iterator(_),_) => {
                Err(RuntimeError::new(GeneralError::new(String::from(format!("Index must be an integer")))))
            }
            _ => {Err(RuntimeError::new(GeneralError::new(String::from(format!("nth not supported for ({:?})", collection)))))}
        }
    }
);

#[test]
fn nth_or_else_test() {
    let a = RuntimeValue::Character('a');
    let b = RuntimeValue::Character('b');
    let c = RuntimeValue::Character('c');
    let int0 = RuntimeValue::Integer(0);
    let int1 = RuntimeValue::Integer(1);
    let int2 = RuntimeValue::Integer(2);
    let vec = RuntimeValue::Vector(Box::new(vector! [a.clone(), b.clone()]));
    let lst = RuntimeValue::List(Box::new(list! [a.clone(), b.clone()]));
    let string = RuntimeValue::String(Box::new(String::from("ab")));

    let else_fn = RuntimeValue::Function(Arc::new(|_,_| Ok(RuntimeValue::Character('c'))));

    let nth_or_else_fn = nth_or_else::function();

    let args = vector![vec.clone(), int0.clone(), else_fn.clone()];
    assert_eq!(a.clone(), nth_or_else_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![vec.clone(), int1.clone(), else_fn.clone()];
    assert_eq!(b.clone(), nth_or_else_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![vec.clone(), int2.clone(), else_fn.clone()];
    assert_eq!(c.clone(), nth_or_else_fn.evaluate_global_context_with_args(args).unwrap());

    let args = vector![lst.clone(), int0.clone(), else_fn.clone()];
    assert_eq!(a.clone(), nth_or_else_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![lst.clone(), int1.clone(), else_fn.clone()];
    assert_eq!(b.clone(), nth_or_else_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![lst.clone(), int2.clone(), else_fn.clone()];
    assert_eq!(c.clone(), nth_or_else_fn.evaluate_global_context_with_args(args).unwrap());

    let args = vector![string.clone(), int0.clone(), else_fn.clone()];
    assert_eq!(a.clone(), nth_or_else_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![string.clone(), int1.clone(), else_fn.clone()];
    assert_eq!(b.clone(), nth_or_else_fn.evaluate_global_context_with_args(args).unwrap());
    let args = vector![string.clone(), int2.clone(), else_fn.clone()];
    assert_eq!(c.clone(), nth_or_else_fn.evaluate_global_context_with_args(args).unwrap());

    
    let args = vector![vec.clone(), a.clone(), else_fn.clone()];
    assert!(nth_or_else_fn.evaluate_global_context_with_args(args).is_err());
    let args = vector![lst.clone(), b.clone(), else_fn.clone()];
    assert!(nth_or_else_fn.evaluate_global_context_with_args(args).is_err());
    let args = vector![string.clone(), c.clone(), else_fn.clone()];
    assert!(nth_or_else_fn.evaluate_global_context_with_args(args).is_err());
    let args = vector![c.clone(), c.clone(), else_fn.clone()];
    assert!(nth_or_else_fn.evaluate_global_context_with_args(args).is_err());
}

intrinsic_function!(
    nthrest
    function (collection, index) {
        match (collection, index) {
            (RuntimeValue::Vector(v), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n)))))
                }
                else {
                    Ok(RuntimeValue::Vector(Box::new(v.iter().skip(n as usize).cloned().collect())))
                }
            }
            (RuntimeValue::List(l), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n)))))
                } else {
                    Ok(RuntimeValue::Vector(Box::new(l.iter().skip(n as usize).collect())))
                }
            }
            (RuntimeValue::String(s), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n)))))
                } else {
                    Ok(RuntimeValue::Vector(Box::new(s.chars().into_iter().skip(n as usize).map(|c|
                        RuntimeValue::Character(c)).collect())))
                }
            }
            (RuntimeValue::Map(m), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n)))))
                }
                else {
                    Ok(RuntimeValue::Vector(Box::new(
                        m.iter().skip(n as usize).map(
                            |(k,v)| RuntimeValue::Vector(Box::new(vector![k.clone(),v.clone()]))
                        ).collect())))
                }
            }
            (RuntimeValue::Set(s), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n)))))
                }
                else {
                    Ok(RuntimeValue::Vector(Box::new(s.iter().skip(n as usize).cloned().collect())))
                }
            }
            (RuntimeValue::Iterator(_), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n)))))
                } else if n == 0 {
                    Ok(collection.clone())
                } else {
                    match collection.evaluate_global_context_with_args(vector![(n-1).into()])? {
                        RuntimeValue::Vector(mut v) if v.len() == 2 => {
                            match v.pop_back().unwrap() {
                                RuntimeValue::None => iter::internal0(),
                                v => Ok(v)
                            }
                        }
                        _ => unreachable!(),
                    }
                    
                }
            }
            (RuntimeValue::Vector(_)|RuntimeValue::List(_)|RuntimeValue::String(_)|RuntimeValue::Map(_)|RuntimeValue::Set(_)|RuntimeValue::Iterator(_),_) => {
                Err(RuntimeError::new(GeneralError::new(String::from(format!("Index must be an integer")))))
            }
            _ => {Err(RuntimeError::new(GeneralError::new(String::from(format!("nthrest not supported for ({:?})", collection)))))}
        }
    }
);

intrinsic_function!(
    nthnext
    function (collection, index) {
        match (collection, index) {
            (RuntimeValue::Vector(v), RuntimeValue::Integer(n)) => {
                match *n {
                    n if n < 0 => Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n))))),
                    n if n as usize >= v.len() => Ok(RuntimeValue::None),
                    n => Ok(RuntimeValue::Vector(Box::new(v.iter().skip(n as usize).cloned().collect()))),
                }
            }
            (RuntimeValue::List(l), RuntimeValue::Integer(n)) => {
                match *n {
                    n if n < 0 => Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n))))),
                    n if n as usize >= l.len() => Ok(RuntimeValue::None),
                    n => Ok(RuntimeValue::Vector(Box::new(l.iter().skip(n as usize).collect()))),
                }
            }
            (RuntimeValue::String(s), RuntimeValue::Integer(n)) => {
                match *n {
                    n if n < 0 => Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n))))),
                    // TODO: Fix iterating over chars() twice.
                    n if n as usize >= s.chars().count() => Ok(RuntimeValue::None),
                    n => Ok(RuntimeValue::Vector(Box::new(s.chars().into_iter().skip(n as usize).map(|c|
                            RuntimeValue::Character(c)).collect()))),
                }
            }
            (RuntimeValue::Map(m), RuntimeValue::Integer(n)) => {
                match *n {
                    n if n < 0 => Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n))))),
                    n if n as usize >= m.len() => Ok(RuntimeValue::None),
                    n => Ok(RuntimeValue::Vector(Box::new(
                            m.iter().skip(n as usize).map(
                                |(k,v)| RuntimeValue::Vector(Box::new(vector![k.clone(), v.clone()]))
                            ).collect()))),
                }
            }
            (RuntimeValue::Set(s), RuntimeValue::Integer(n)) => {
                match *n {
                    n if n < 0 => Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n))))),
                    n if n as usize >= s.len() => Ok(RuntimeValue::None),
                    n =>  Ok(RuntimeValue::Vector(Box::new(s.iter().skip(n as usize).cloned().collect()))),
                }
            }
            (RuntimeValue::Iterator(_), RuntimeValue::Integer(n)) => {
                let n = *n;
                if n < 0 {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n)))))
                } else if n == 0 {
                    Ok(collection.clone())
                } else {
                    match collection.evaluate_global_context_with_args(vector![(n-1).into()])? {
                        RuntimeValue::Vector(mut v) if v.len() == 2 => Ok(v.pop_back().unwrap()),
                        _ => unreachable!(),
                    }
                    
                }
            }
            (RuntimeValue::Vector(_)|RuntimeValue::List(_)|RuntimeValue::String(_)|RuntimeValue::Map(_)|RuntimeValue::Set(_)|RuntimeValue::Iterator(_),_) => {
                Err(RuntimeError::new(GeneralError::new(String::from(format!("Index must be an integer")))))
            }
            _ => {Err(RuntimeError::new(GeneralError::new(String::from(format!("nthrest not supported for ({:?})", collection)))))}
        }
    }
);

intrinsic_function!(
    cons
    function (element, collection) {
        match collection {
            RuntimeValue::List(l) => {
                Ok(RuntimeValue::List(Box::new(List::cons(element.clone(),l.as_ref()))))
            },
            RuntimeValue::Map(_) | RuntimeValue::Set(_) | RuntimeValue::Vector(_) | RuntimeValue::String(_) => {
                cons::internal2(element, &seq::internal1(collection)?)
            }
            // TODO Add iterator? Should cons seq the iterator or alias chain?
            RuntimeValue::None => Ok(RuntimeValue::List(Box::new(list!(element.clone())))),
            _ => {Err(RuntimeError::new(GeneralError::new(String::from(format!("cons not supported for ({:?})", collection)))))}
        }
    }
);

intrinsic_function!(
    seq
    function (collection) {
        match collection {
            RuntimeValue::List(l) => {
                if l.len() == 0 {
                    Ok(RuntimeValue::None)
                } else {
                    Ok(collection.clone())
                }
            }
            RuntimeValue::Map(m) => {
                if m.len() == 0 {
                    Ok(RuntimeValue::None)
                } else {
                    Ok(RuntimeValue::List(Box::new(
                        m.iter().map(|(k, v)| RuntimeValue::Vector(Box::new(vector![k.clone(),v.clone()]))).collect()
                    )))
                }
            } 
            RuntimeValue::Set(s) => {
                if s.len() == 0 {
                    Ok(RuntimeValue::None)
                } else {
                    Ok(RuntimeValue::List(Box::new(
                        s.iter().cloned().collect()
                    )))
                }
            } 
            RuntimeValue::Vector(v) => {
                if v.len() == 0 {
                    Ok(RuntimeValue::None)
                } else {
                    Ok(RuntimeValue::List(Box::new(
                        v.iter().rev().cloned().collect()
                    )))
                }
            } 
            RuntimeValue::String(s) => {
                if s.len() == 0 {
                    Ok(RuntimeValue::None)
                } else {
                    Ok(RuntimeValue::List(Box::new(
                        s.chars().into_iter().map(|c| RuntimeValue::Character(c)).rev().collect()
                    )))
                }
            }
            RuntimeValue::Iterator(_) => {
                let mut it = collection.clone();
                let v: Result<Vec<_>,_> = std::iter::from_fn(move || {
                    match it.evaluate_global_context_with_args(vector![]) {
                        Ok(RuntimeValue::Vector(mut v)) if v.len() == 2 => {
                            match v.pop_back().unwrap() {
                                RuntimeValue::None => None,
                                rest_it => {
                                    it = rest_it;
                                    Some(Ok(v.pop_front().unwrap()))
                                }
                            }    
                        }
                        e @ Err(_) => Some(e),
                        _ => unreachable!(),
                    }
                }).collect();
                let v = v?;
                if v.len() == 0 {
                    Ok(RuntimeValue::None)
                } else {
                    Ok(RuntimeValue::List(Box::new(v.into_iter().rev().collect())))
                }
            }
            _ => {Err(RuntimeError::new(GeneralError::new(String::from(format!("seq not supported for ({:?})", collection)))))}
        }
    }
);

intrinsic_function!(
    sequence
    function (collection) {
        match collection {
            RuntimeValue::List(_) => {
                Ok(collection.clone())
            }
            RuntimeValue::Map(m) => {
                if m.len() == 0 {
                    Ok(RuntimeValue::List(Box::new(List::empty())))
                } else {
                    Ok(RuntimeValue::List(Box::new(
                        m.iter().map(|(k, v)| RuntimeValue::Vector(Box::new(vector![k.clone(),v.clone()]))).collect()
                    )))
                }
            } 
            RuntimeValue::Set(s) => {
                if s.len() == 0 {
                    Ok(RuntimeValue::List(Box::new(List::empty())))
                } else {
                    Ok(RuntimeValue::List(Box::new(
                        s.iter().cloned().collect()
                    )))
                }
            } 
            RuntimeValue::Vector(v) => {
                if v.len() == 0 {
                    Ok(RuntimeValue::List(Box::new(List::empty())))
                } else {
                    Ok(RuntimeValue::List(Box::new(
                        v.iter().rev().cloned().collect()
                    )))
                }
            } 
            RuntimeValue::String(s) => {
                if s.len() == 0 {
                    Ok(RuntimeValue::List(Box::new(List::empty())))
                } else {
                    Ok(RuntimeValue::List(Box::new(
                        s.chars().into_iter().map(|c| RuntimeValue::Character(c)).rev().collect()
                    )))
                }
            }
            RuntimeValue::Iterator(_) => {
                let mut it = collection.clone();
                let v: Result<Vec<_>,_> = std::iter::from_fn(move || {
                    match it.evaluate_global_context_with_args(vector![]) {
                        Ok(RuntimeValue::Vector(mut v)) if v.len() == 2 => {
                            match v.pop_back().unwrap() {
                                RuntimeValue::None => None,
                                rest_it => {
                                    it = rest_it;
                                    Some(Ok(v.pop_front().unwrap()))
                                }
                            }    
                        }
                        e @ Err(_) => Some(e),
                        _ => unreachable!(),
                    }
                }).collect();
                let v = v?;
                Ok(RuntimeValue::List(Box::new(v.into_iter().rev().collect())))
            }
            _ => {Err(RuntimeError::new(GeneralError::new(String::from(format!("seq not supported for ({:?})", collection)))))}
        }
    }
);

intrinsic_function!(
    first
    function (collection) {
        match collection {
            RuntimeValue::None => Ok(RuntimeValue::None),
            RuntimeValue::List(_) | RuntimeValue::Vector(_) | RuntimeValue::String(_) | RuntimeValue::Iterator(_) => {
                nth::internal3(collection, &RuntimeValue::Integer(0), &RuntimeValue::None)
            }
            RuntimeValue::Map(m) => {
                if m.len() == 0 {
                    Ok(RuntimeValue::None)
                } else {
                    let (k, v) = m.iter().next().unwrap();
                    Ok(RuntimeValue::Vector(Box::new(vector![k.clone(), v.clone()])))
                }
            } 
            RuntimeValue::Set(s) => {
                if s.len() == 0 {
                    Ok(RuntimeValue::None)
                } else {
                    Ok(s.iter().next().unwrap().clone())
                }
            }
            _ => {Err(RuntimeError::new(GeneralError::new(String::from(format!("first not supported for ({:?})", collection)))))}
        }
    }
);

intrinsic_function!(
    rest
    function (collection) {
        match collection {
            RuntimeValue::None => Ok(RuntimeValue::List(Box::new(List::empty()))),
            RuntimeValue::List(_) | RuntimeValue::Vector(_) | RuntimeValue::String(_) | RuntimeValue::Map(_) |RuntimeValue::Set(_) | RuntimeValue::Iterator(_) => {
                nthrest::internal2(collection, &RuntimeValue::Integer(1))
            }
            _ => {Err(RuntimeError::new(GeneralError::new(String::from(format!("rest not supported for ({:?})", collection)))))}
        }
    }
);

intrinsic_function!(
    next
    function (collection) {
        match collection {
            RuntimeValue::None => Ok(RuntimeValue::List(Box::new(List::empty()))),
            RuntimeValue::List(_) | RuntimeValue::Vector(_) | RuntimeValue::String(_) | RuntimeValue::Map(_) |RuntimeValue::Set(_) | RuntimeValue::Iterator(_) => {
                nthnext::internal2(collection, &RuntimeValue::Integer(1))
            }
            _ => {Err(RuntimeError::new(GeneralError::new(String::from(format!("next not supported for ({:?})", collection)))))}
        }
    }
);

intrinsic_function!(
    eq
    function (_x) {
        Ok(RuntimeValue::Boolean(true))
    }
    function (x, y) {
        Ok(RuntimeValue::Boolean(x == y))
    }
    variadic (x, y, more) {
        if x == y {
            if let RuntimeValue::None = more {
                Ok(RuntimeValue::Boolean(true))
            } else {
                Err(RuntimeError::Recur(vector![y.clone(), first::internal1(more)?, next::internal1(more)?]))
            }
        } else {
            Ok(RuntimeValue::Boolean(false))
        }
    }
);

intrinsic_function!(
    list
    variadic (more) {
        match more {
            RuntimeValue::None =>
                Ok(RuntimeValue::List(Box::new(List::empty()))),
            RuntimeValue::Vector(v) =>
                Ok(RuntimeValue::List(Box::new(
                    v.iter().rev().cloned().collect()
                ))),
            _ => unreachable!()
        }
    }
);

intrinsic_function!(
    concat
    variadic (more) {
        match more {
            RuntimeValue::None => Ok(RuntimeValue::List(Box::new(List::empty()))),
            RuntimeValue::Vector(v) => {
                // TODO Add iterator? Should concat seq the iterator or alias chain?
                let seqs: Result<Vec<_>,_> =
                v.iter().map(|v| sequence::internal1(v))
                .flat_map(|value| match value {
                    Ok(RuntimeValue::List(l)) => {
                        l.iter().map(|value| Ok(value)).collect()
                    }
                    Ok(_) => unreachable!(),
                    e @ Err(_) => vec![e],
                })
                .collect();
                match seqs {
                    Ok(v) => Ok(RuntimeValue::List(Box::new(v.into_iter().rev().collect()))),
                    Err(e) => Err(RuntimeError::new(GeneralError::new(String::from(format!("concat failed: ({:?})", e))))),
                }

            }
            _ => unreachable!()
        }
    }
);

intrinsic_function!(
    into
    function (to, from) {
        match (to, from) {
            // into List, Reverse vector as list FromIterator builds the list last -> first
            (RuntimeValue::List(l1),RuntimeValue::List(l2)) => {
                let v1: Vec<_> = l1.iter().collect();
                let v2: Vec<_> = l2.iter().collect();
                Ok(RuntimeValue::List(Box::new(
                    v2.into_iter().rev().chain(
                        v1.into_iter().rev()).collect())))
            }
            (RuntimeValue::List(l1),RuntimeValue::Map(m2)) => {
                let v1: Vec<_> = l1.iter().collect();
                Ok(RuntimeValue::List(Box::new(
                    m2.iter().map(|(k,v)|
                        RuntimeValue::Vector(Box::new(vector![k.clone(), v.clone()]))
                    ).chain(v1.into_iter().rev()).collect())))
            }
            (RuntimeValue::List(l1),RuntimeValue::Vector(v2)) => {
                let v1: Vec<_> = l1.iter().collect();
                Ok(RuntimeValue::List(Box::new(
                    v2.iter().rev().cloned().chain(
                        v1.into_iter().rev()).collect())))
            }
            (RuntimeValue::List(l1),RuntimeValue::Set(s2)) => {
                let v1: Vec<_> = l1.iter().collect();
                Ok(RuntimeValue::List(Box::new(
                    s2.iter().cloned().chain(
                        v1.into_iter().rev()).collect())))
            }
            (RuntimeValue::List(l1),RuntimeValue::String(s2)) => {
                let v1: Vec<_> = l1.iter().collect();
                Ok(RuntimeValue::List(Box::new(
                    s2.chars().map(|c| RuntimeValue::Character(c))
                        .rev().chain(v1.into_iter().rev()).collect())))
            }

            // into Map
            (RuntimeValue::Map(m1),RuntimeValue::List(l2)) => {
                let m2: Result<HashMap<_,_>,_> = l2.iter().map(
                    |v| match v {
                        RuntimeValue::Vector(mut v) if v.len() == 2 =>
                            Ok((v.pop_front().unwrap(), v.pop_front().unwrap())),
                        _ => Err(RuntimeError::new(GeneralError::new(String::from(format!("into map from list must have [key values]")))))
                    }
                ).collect();
                let m2 = m2?;
                Ok(RuntimeValue::Map(Box::new(
                    if m1.len() < m2.len() {
                        m2.union_with(m1.as_ref().clone(), |v,_| v)
                    } else {
                        m1.clone().union_with(m2, |_,v| v)
                    }
                )))
            }
            (RuntimeValue::Map(m1),RuntimeValue::Map(m2)) => Ok(RuntimeValue::Map(Box::new(
                if m1.len() < m2.len() {
                    m2.clone().union_with(m1.as_ref().clone(), |v,_| v)
                } else {
                    m1.clone().union_with(m2.as_ref().clone(), |_,v| v)
                }
            ))),
            (RuntimeValue::Map(m1),RuntimeValue::Vector(v2)) => {
                let m2: Result<HashMap<_,_>,_> = v2.iter().map(
                    |v| match v {
                        RuntimeValue::Vector(v) if v.len() == 2 =>
                        Ok((v.get(0).unwrap().clone(), v.get(1).unwrap().clone())),
                        _ => Err(RuntimeError::new(GeneralError::new(String::from(format!("into map from vector must have [key values]")))))
                    }
                ).collect();
                let m2 = m2?;
                Ok(RuntimeValue::Map(Box::new(
                    if m1.len() < m2.len() {
                        m2.union_with(m1.as_ref().clone(), |v,_| v)
                    } else {
                        m1.clone().union_with(m2, |_,v| v)
                    }
                )))
            }

            // into Set
            (RuntimeValue::Set(s1),RuntimeValue::List(l2)) => {
                let s2: HashSet<_> = l2.into_iter().collect();
                Ok(RuntimeValue::Set(Box::new(
                    if s1.len() < s2.len() {
                        s2.union(s1.as_ref().clone())
                    } else {
                        s1.clone().union(s2)
                    }
                )))
            }
            (RuntimeValue::Set(s1),RuntimeValue::Map(m2)) => {
                let s2: HashSet<_> = m2.as_ref().clone().into_iter().map(|(k,v)|
                        RuntimeValue::Vector(Box::new(vector![k, v]))
                    ).collect();
                Ok(RuntimeValue::Set(Box::new(
                    if s1.len() < s2.len() {
                        s2.union(s1.as_ref().clone())
                    } else {
                        s1.clone().union(s2)
                    }
                )))
            }
            (RuntimeValue::Set(s1),RuntimeValue::Set(s2)) => {
                Ok(RuntimeValue::Set(Box::new(
                    if s1.len() < s2.len() {
                        s2.clone().union(s1.as_ref().clone())
                    } else {
                        s1.clone().union(s2.as_ref().clone())
                    }
                )))
            }
            (RuntimeValue::Set(s1),RuntimeValue::Vector(v2)) => {
                let s2: HashSet<_> = v2.as_ref().clone().into_iter().collect();
                Ok(RuntimeValue::Set(Box::new(
                    if s1.len() < s2.len() {
                        s2.union(s1.as_ref().clone())
                    } else {
                        s1.clone().union(s2)
                    }
                )))
            }
            (RuntimeValue::Set(s1),RuntimeValue::String(s2)) => {
                let s2: HashSet<_> = s2.chars().into_iter().map(|c|
                        RuntimeValue::Character(c)
                    ).collect();
                Ok(RuntimeValue::Set(Box::new(
                    if s1.len() < s2.len() {
                        s2.union(s1.as_ref().clone())
                    } else {
                        s1.clone().union(s2)
                    }
                )))
            }

            // into Vector
            (RuntimeValue::Vector(v1),RuntimeValue::List(l2)) => {
                let v2: Vector<_> = l2.iter().collect();
                let mut v1 = v1.as_ref().clone();
                v1.append(v2);
                Ok(RuntimeValue::Vector(Box::new(v1)))
            }
            (RuntimeValue::Vector(v1),RuntimeValue::Map(m2)) => {
                let v2: Vector<_> = m2.iter().map(|(k,v)|
                        RuntimeValue::Vector(Box::new(vector![k.clone(), v.clone()]))
                    ).collect();
                let mut v1 = v1.as_ref().clone();
                v1.append(v2);
                Ok(RuntimeValue::Vector(Box::new(v1)))
            }
            (RuntimeValue::Vector(v1),RuntimeValue::Set(s2)) => {
                let v2: Vector<_> = s2.iter().cloned().collect();
                let mut v1 = v1.as_ref().clone();
                v1.append(v2);
                Ok(RuntimeValue::Vector(Box::new(v1)))
            }
            (RuntimeValue::Vector(v1),RuntimeValue::Vector(v2)) => {
                let mut v1 = v1.as_ref().clone();
                v1.append(v2.as_ref().clone());
                Ok(RuntimeValue::Vector(Box::new(v1)))
            }
            (RuntimeValue::Vector(v1),RuntimeValue::String(s2)) => {
                let v2: Vector<_> = s2.chars().into_iter().map(|c|
                        RuntimeValue::Character(c)
                    ).collect();
                let mut v1 = v1.as_ref().clone();
                v1.append(v2);
                Ok(RuntimeValue::Vector(Box::new(v1)))
            }

            // into String
            (RuntimeValue::String(s1),RuntimeValue::List(l2)) => {
                let s1: Result<String,_> = s1.chars().into_iter().map(|c| Ok(c)).chain(
                    l2.iter().map(
                    |v| match v {
                        RuntimeValue::Character(c)  =>
                        Ok(c),
                        _ => Err(RuntimeError::new(GeneralError::new(String::from(format!("into string from list must have char values"))))),
                    })
                ).collect();
                Ok(RuntimeValue::String(Box::new(s1?)))
            }
            (RuntimeValue::String(s1),RuntimeValue::Set(s2)) => {
                let s1: Result<String,_> = s1.chars().into_iter().map(|c| Ok(c)).chain(
                    s2.as_ref().clone().into_iter().map(
                    |v| match v {
                        RuntimeValue::Character(c)  =>
                        Ok(c),
                        _ => Err(RuntimeError::new(GeneralError::new(String::from(format!("into string from set must have char values"))))),
                    })
                ).collect();
                Ok(RuntimeValue::String(Box::new(s1?)))
            }
            (RuntimeValue::String(s1),RuntimeValue::Vector(v2)) => {
                let s1: Result<String,_> = s1.chars().into_iter().map(|c| Ok(c)).chain(
                    v2.iter().map(
                    |v| match v {
                        RuntimeValue::Character(c)  =>
                        Ok(*c),
                        _ => Err(RuntimeError::new(GeneralError::new(String::from(format!("into string from vector must have char values"))))),
                    })
                ).collect();
                Ok(RuntimeValue::String(Box::new(s1?)))
            }
            (RuntimeValue::String(s1),RuntimeValue::String(s2)) => {
                Ok(RuntimeValue::String(Box::new(format!("{}{}",s1.as_ref() ,s2.as_ref()))))
            }

            // TODO Add iterator support. How should this work? into iter is just chain.

            (_,_) => Err(RuntimeError::new(GeneralError::new(String::from(format!("into not supported for ({:?} into {:?})", from, to))))),
        }
    }
);

intrinsic_function!(
    apply
    variadic (f, more) {
        match into::internal2(&RuntimeValue::Vector(Box::new(Vector::new())), &concat::internal1(more)?)? {
            RuntimeValue::Vector(args) => f.evaluate_global_context_with_args(*args),
            _ => unreachable!()
        }

    }
);

intrinsic_function!(
    println
    variadic (more) {
        match more {
            RuntimeValue::Vector(v) =>
                println!("{:?}", v.iter().map(|v| format!("{:?}",v)).collect::<Vec<String>>().join(" ")),
            _ => println!()
        };
        Ok(RuntimeValue::None)
    }
);

intrinsic_function!(
    is_seq
    function (x) {
        match x {
            RuntimeValue::List(_) => Ok(RuntimeValue::Boolean(true)),
            _ => Ok(RuntimeValue::Boolean(false))
        }

    }
);

intrinsic_function!(
    iter
    function () {
        Ok(RuntimeValue::Iterator(Arc::new(
            move |_,_| {
                Ok(RuntimeValue::Vector(Box::new(vector![RuntimeValue::None,RuntimeValue::None])))
            }
        )))
    }
    function (c) {
        let c = c.clone();
        match c {
            RuntimeValue::Vector(v) => {
                if v.is_empty() {
                    iter::internal0()
                } else {
                    Ok(RuntimeValue::Iterator(Arc::new(
                        move |_,args| {
                            match args.len() {
                                0 => {
                                    let mut v = v.as_ref().clone();
                                    Ok(vector![v.pop_front().unwrap(), iter::internal1(&v.into())?].into())
                                },
                                1 => match &args[0] {
                                    RuntimeValue::Integer(n) => {
                                        let n = *n;
                                        if n < 0 {
                                            Err(RuntimeError::new(GeneralError::new(String::from(
                                                "Index into iterator cannot be negative",
                                            ))))
                                        } else if (n as usize) < v.len() {
                                            let n = n as usize;
                                            Ok(vector![v[n].clone(), iter::internal1(&v.skip(n + 1).into())?].into())
                                        } else {
                                            Ok(vector![RuntimeValue::None, RuntimeValue::None].into())
                                        }
                                    }
                                    _ => Err(RuntimeError::new(GeneralError::new(String::from(
                                        "Index into iterator must be an integer.",
                                    )))),
                                },
                                n => Err(RuntimeError::new(ArityError::new(
                                    n,
                                    String::from("Iter"),
                                ))),
                            }
                        }
                    )))
                }
            }
            RuntimeValue::Map(m) => {
                if m.is_empty() {
                    iter::internal0()
                } else {
                    Ok(RuntimeValue::Iterator(Arc::new(
                        move |_,args| {
                            match args.len() {
                                0 => {
                                    let (k, v) = m.iter().next().unwrap();
                                    Ok(vector![vector![k.clone(), v.clone()].into(), 
                                                iter::internal1(&m.without(k).into())?].into())
                                },
                                1 => match &args[0] {
                                    RuntimeValue::Integer(n) => {
                                        let n = *n;
                                        if n < 0 {
                                            Err(RuntimeError::new(GeneralError::new(String::from(
                                                "Index into iterator cannot be negative",
                                            ))))
                                        } else if (n as usize) < m.len() {
                                            let (mut k, mut v) = m.iter().next().unwrap();
                                            let mut m = m.as_ref().clone();
                                            for _ in 0..n {
                                                m = m.without(k);
                                                let (k_, v_)= m.iter().next().unwrap();
                                                k = k_; v= v_;
                                            }
                                            Ok(vector![vector![k.clone(), v.clone()].into(), 
                                                        iter::internal1(&m.without(k).into())?].into())
                                        } else {
                                            Ok(vector![RuntimeValue::None, RuntimeValue::None].into())
                                        }
                                    }
                                    _ => Err(RuntimeError::new(GeneralError::new(String::from(
                                        "Index into iterator must be an integer.",
                                    )))),
                                },
                                n => Err(RuntimeError::new(ArityError::new(
                                    n,
                                    String::from("Iter"),
                                ))),
                            }
                        }
                    )))
                }
            }
            RuntimeValue::Set(s) => {
                if s.is_empty() {
                    iter::internal0()
                } else {
                    Ok(RuntimeValue::Iterator(Arc::new(
                        move |_,args| {
                            match args.len() {
                                0 => {
                                    let v = s.iter().next().unwrap();
                                    Ok(vector![v.clone(),iter::internal1(&s.without(v).into())?].into())
                                },
                                1 => match &args[0] {
                                    RuntimeValue::Integer(n) => {
                                        let n = *n;
                                        if n < 0 {
                                            Err(RuntimeError::new(GeneralError::new(String::from(
                                                "Index into iterator cannot be negative",
                                            ))))
                                        } else if (n as usize) < s.len() {
                                            let mut v = s.iter().next().unwrap();
                                            let mut s = s.as_ref().clone();
                                            for _ in 0..n {
                                                s = s.without(v);
                                                v = s.iter().next().unwrap();
                                            }
                                            Ok(vector![v.clone(), iter::internal1(&s.without(v).into())?].into())
                                        } else {
                                            Ok(vector![RuntimeValue::None, RuntimeValue::None].into())
                                        }
                                    }
                                    _ => Err(RuntimeError::new(GeneralError::new(String::from(
                                        "Index into iterator must be an integer.",
                                    )))),
                                },
                                n => Err(RuntimeError::new(ArityError::new(
                                    n,
                                    String::from("Iter"),
                                ))),
                            }
                        }
                    )))
                }
            }
            RuntimeValue::List(l) => {
                if l.is_empty() {
                    iter::internal0()
                } else {
                    Ok(RuntimeValue::Iterator(Arc::new(
                        move |_,args| {
                            match args.len() {
                                0 => {
                                    let v = l.first().unwrap();
                                    Ok(vector![v.clone(),iter::internal1(&l.rest().into())?].into())
                                },
                                1 => match &args[0] {
                                    RuntimeValue::Integer(n) => {
                                        let n = *n;
                                        if n < 0 {
                                            Err(RuntimeError::new(GeneralError::new(String::from(
                                                "Index into iterator cannot be negative",
                                            ))))
                                        } else if (n as usize) < l.len() {
                                            let l = l.skip(n as usize);
                                            let v = l.first().unwrap();
                                            Ok(vector![v.clone(),iter::internal1(&l.rest().into())?].into())
                                        } else {
                                            Ok(vector![RuntimeValue::None, RuntimeValue::None].into())
                                        }
                                    }
                                    _ => Err(RuntimeError::new(GeneralError::new(String::from(
                                        "Index into iterator must be an integer.",
                                    )))),
                                },
                                n => Err(RuntimeError::new(ArityError::new(
                                    n,
                                    String::from("Iter"),
                                ))),
                            }
                        }
                    )))
                }
            }
            RuntimeValue::String(s) => {
                if s.is_empty() {
                    iter::internal0()
                } else {
                    Ok(RuntimeValue::Iterator(Arc::new(
                        move |_,args| {
                            match args.len() {
                                0 => {
                                    let mut s = s.as_ref().clone();
                                    Ok(vector![s.pop().unwrap().into(), iter::internal1(&s.into())?].into())
                                },
                                1 => match &args[0] {
                                    RuntimeValue::Integer(n) => {
                                        let n = *n;
                                        if n < 0 {
                                            Err(RuntimeError::new(GeneralError::new(String::from(
                                                "Index into iterator cannot be negative",
                                            ))))
                                        } else if (n as usize) < s.len() {
                                            let mut s = s.chars().skip(n as usize).collect::<String>();
                                            Ok(vector![s.pop().unwrap().into(), iter::internal1(&s.into())?].into())
                                        } else {
                                            Ok(vector![RuntimeValue::None, RuntimeValue::None].into())
                                        }
                                    }
                                    _ => Err(RuntimeError::new(GeneralError::new(String::from(
                                        "Index into iterator must be an integer.",
                                    )))),
                                },
                                n => Err(RuntimeError::new(ArityError::new(
                                    n,
                                    String::from("Iter"),
                                ))),
                            }
                        }
                    )))
                }
            }
            c @ RuntimeValue::Function(_) => {
                iter::internal2(&c.evaluate_global_context_with_args(vector![].into())?, &c)
            }
            c @ RuntimeValue::Iterator(_) => Ok(c),
            c => Ok(RuntimeValue::Iterator(Arc::new(
                move |_,args| {
                    match args.len() {
                        0 => {
                            Ok(vector![c.clone(), iter::internal0()?].into())
                        },
                        1 => match &args[0] {
                            RuntimeValue::Integer(n) => {
                                let n = *n;
                                if n < 0 {
                                    Err(RuntimeError::new(GeneralError::new(String::from(
                                        "Index into iterator cannot be negative",
                                    ))))
                                } else if n < 1 {
                                    Ok(vector![c.clone(), iter::internal0()?].into())
                                } else {
                                    Ok(vector![RuntimeValue::None, RuntimeValue::None].into())
                                }
                            }
                            _ => Err(RuntimeError::new(GeneralError::new(String::from(
                                "Index into iterator must be an integer.",
                            )))),
                        },
                        n => Err(RuntimeError::new(ArityError::new(
                            n,
                            String::from("Iter"),
                        ))),
                    }
                }
            )))
        }
    }
    function (initial, function) {
        match function {
            RuntimeValue::Function(_) => {
                let initial = initial.clone();
                let f = function.clone();
                Ok(RuntimeValue::Iterator(Arc::new(
                    move |_, args| {
                        match args.len() {
                            0 => {
                                if let RuntimeValue::Vector(mut v) = f.evaluate_global_context_with_args(vector![initial.clone()])? {
                                    if v.len() == 2 {
                                        Ok(vector![v.pop_front().unwrap(), iter::internal2(&v.pop_front().unwrap(), &f)?].into())
                                    } else if v.len() == 1 {
                                        Ok(vector![v.pop_front().unwrap(), iter::internal0()?].into())
                                    } else {
                                        Err(RuntimeError::new(GeneralError::new(String::from(
                                            "Iter function must contain signature: state -> [next, state?])",
                                        ))))
                                    }
                                } else {
                                    Err(RuntimeError::new(GeneralError::new(String::from(
                                        "Iter function must contain signature: state -> [next, state?])",
                                    ))))
                                }
                            },
                            1 => match &args[0] {
                                RuntimeValue::Integer(n) => {
                                    let n = *n;
                                    if n < 0 {
                                        Err(RuntimeError::new(GeneralError::new(String::from(
                                            "Index into iterator cannot be negative",
                                        ))))
                                    } else if let RuntimeValue::Vector(mut v) = f.evaluate_global_context_with_args(vector![initial.clone(), n.into()])? {
                                        if v.len() == 2 {
                                            Ok(vector![v.pop_front().unwrap(), iter::internal2(&v.pop_front().unwrap(), &f)?].into())
                                        } else if v.len() == 1 {
                                            Ok(vector![v.pop_front().unwrap(), iter::internal0()?].into())
                                        } else {
                                            Err(RuntimeError::new(GeneralError::new(String::from(
                                                "Iter function must contain signature: state, count -> [next, state?])",
                                            ))))
                                        }
                                    } else {
                                        Err(RuntimeError::new(GeneralError::new(String::from(
                                            "Iter function must contain signature: state, count -> [next, state?])",
                                        ))))
                                    }
                                }
                                _ => Err(RuntimeError::new(GeneralError::new(String::from(
                                    "Index into iterator must be an integer.",
                                )))),
                            },
                            n => Err(RuntimeError::new(ArityError::new(
                                n,
                                String::from("Iter"),
                            ))),
                        }
                    }
                )))
            }
            other => Err(RuntimeError::new(GeneralError::new(format!("Iter requires type ({:?}) to be a function", other))))
        }
    }
);

