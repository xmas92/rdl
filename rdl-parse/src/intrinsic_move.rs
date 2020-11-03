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
        pub fn internal1($arg0: RuntimeValue) -> RuntimeResult {
            $function_block
        }
    };
    (($arg0:ident, $arg1:ident) $function_block:block) => {
        pub fn internal2($arg0: RuntimeValue, $arg1: RuntimeValue) -> RuntimeResult {
            $function_block
        }
    };
    (($arg0:ident, $arg1:ident, $arg2:ident) $function_block:block) => {
        pub fn internal3(
            $arg0: RuntimeValue,
            $arg1: RuntimeValue,
            $arg2: RuntimeValue,
        ) -> RuntimeResult {
            $function_block
        }
    };
    (($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident) $function_block:block) => {
        pub fn internal4(
            $arg0: RuntimeValue,
            $arg1: RuntimeValue,
            $arg2: RuntimeValue,
            $arg3: RuntimeValue,
        ) -> RuntimeResult {
            $function_block
        }
    };
    (($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident, $arg4:ident) $function_block:block) => {
        pub fn internal5(
            $arg0: RuntimeValue,
            $arg1: RuntimeValue,
            $arg2: RuntimeValue,
            $arg3: RuntimeValue,
            $arg4: RuntimeValue,
        ) -> RuntimeResult {
            $function_block
        }
    };
    (($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident, $arg4:ident, $arg5:ident) $function_block:block) => {
        pub fn internal6(
            $arg0: RuntimeValue,
            $arg1: RuntimeValue,
            $arg2: RuntimeValue,
            $arg3: RuntimeValue,
            $arg4: RuntimeValue,
            $arg5: RuntimeValue,
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
        $name::internal1($values.pop_front().unwrap())
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident)) => {
        $name::internal2($values.pop_front().unwrap(), $values.pop_front().unwrap())
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident, $arg2:ident)) => {
        $name::internal3(
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
        )
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident)) => {
        $name::internal4(
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
        )
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident, $arg4:ident)) => {
        $name::internal5(
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
        )
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident, $arg4:ident, $arg5:ident)) => {
        $name::internal6(
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
        )
    };
}

macro_rules! intrinsic_function_matcher_internal_variadic_arg {
    ($values:expr) => {
        if $values.len() > 0 {
            RuntimeValue::Vector(Box::new($values))
        } else {
            RuntimeValue::None
        }
    };
}

macro_rules! intrinsic_function_matcher_internal_variadic_call {
    ($name:ident,$values:expr, ($arg0:ident)) => {
        $name::internal1(intrinsic_function_matcher_internal_variadic_arg!($values))
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident)) => {
        $name::internal2(
            $values.pop_front().unwrap(),
            intrinsic_function_matcher_internal_variadic_arg!($values),
        )
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident, $arg2:ident)) => {
        $name::internal3(
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            intrinsic_function_matcher_internal_variadic_arg!($values),
        )
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident)) => {
        $name::internal4(
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            intrinsic_function_matcher_internal_variadic_arg!($values),
        )
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident, $arg4:ident)) => {
        $name::internal5(
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            intrinsic_function_matcher_internal_variadic_arg!($values),
        )
    };
    ($name:ident,$values:expr, ($arg0:ident, $arg1:ident, $arg2:ident, $arg3:ident, $arg4:ident, $arg5:ident)) => {
        $name::internal6(
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            $values.pop_front().unwrap(),
            intrinsic_function_matcher_internal_variadic_arg!($values),
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
            #[allow(unused_mut)]
            pub fn function() -> RuntimeValue {
                RuntimeValue::Function(
                    Arc::new(
                        move |_: Context, mut args: Vector<RuntimeValue>| {
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
            x @ (RuntimeValue::Integer(_) |
            RuntimeValue::BigInteger(_) |
            RuntimeValue::Float(_) |
            RuntimeValue::BigFloat(_)) => { Ok(x) }
            x => {Err(RuntimeError::new(GeneralError::new(format!("({:?}) is not a number", x))))}
        }
    }
    function (x, y) {
        match (x, y) {
            (ref x @ RuntimeValue::Integer(n1), ref y @ RuntimeValue::Integer(n2)) => {
                if let Some(n) = n1.checked_add(n2) {
                    Ok(RuntimeValue::Integer(n))
                } else {
                    Err(RuntimeError::new(GeneralError::new(format!("Integer overflow + ({:?}, {:?})", x, y))))
                }
            }
            (RuntimeValue::BigInteger(n1),RuntimeValue::BigInteger(n2)) => {
                Ok(RuntimeValue::BigInteger(Box::new(n1.as_ref() + n2.as_ref())))
            }
            (RuntimeValue::Float(n1),RuntimeValue::Float(n2)) => {
                Ok(RuntimeValue::Float(n1+n2))
            }
            (RuntimeValue::BigFloat(n1),RuntimeValue::BigFloat(n2)) => {
                Ok(RuntimeValue::BigFloat(Box::new(n1.as_ref() + n2.as_ref())))
            }
            (x,y) => {Err(RuntimeError::new(GeneralError::new(format!("Type mismatch plus ({:?}, {:?})", x, y))))}
        }
    }
    variadic (x, y, more) {
        match more {
            RuntimeValue::Vector(v) => {
                v.into_iter().try_fold(plus::internal2(x,y)?, |x, y| plus::internal2(x,y))
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
            x @ (RuntimeValue::Integer(_) |
            RuntimeValue::BigInteger(_) |
            RuntimeValue::Float(_) |
            RuntimeValue::BigFloat(_)) => { Ok(x) }
            x => {Err(RuntimeError::new(GeneralError::new(format!("({:?}) is not a number", x))))}
        }
    }
    function (x, y) {
        match (x, y) {
            (ref x @ RuntimeValue::Integer(n1), ref y @ RuntimeValue::Integer(n2)) => {
                if let Some(n) = n1.checked_sub(n2) {
                    Ok(RuntimeValue::Integer(n))
                } else {
                    Err(RuntimeError::new(GeneralError::new(format!("Integer underflow minus ({:?}, {:?})", x, y))))
                }
            }
            (RuntimeValue::BigInteger(n1),RuntimeValue::BigInteger(n2)) => {
                Ok(RuntimeValue::BigInteger(Box::new(n1.as_ref() - n2.as_ref())))
            }
            (RuntimeValue::Float(n1),RuntimeValue::Float(n2)) => {
                Ok(RuntimeValue::Float(n1-n2))
            }
            (RuntimeValue::BigFloat(n1),RuntimeValue::BigFloat(n2)) => {
                Ok(RuntimeValue::BigFloat(Box::new(n1.as_ref() - n2.as_ref())))
            }
            (x, y) => {Err(RuntimeError::new(GeneralError::new(format!("Type mismatch minus ({:?}, {:?})", x, y))))}
        }
    }
    variadic (x, y, more) {
        match more {
            RuntimeValue::Vector(v) => {
                v.into_iter().try_fold(minus::internal2(x,y)?, |x, y| minus::internal2(x,y))
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
            x @ (RuntimeValue::Integer(_) |
            RuntimeValue::BigInteger(_) |
            RuntimeValue::Float(_) |
            RuntimeValue::BigFloat(_)) => { Ok(x) }
            x => {Err(RuntimeError::new(GeneralError::new(format!("({:?}) is not a number", x))))}
        }
    }
    function (x, y) {
        match (x, y) {
            (ref x @ RuntimeValue::Integer(n1), ref y @ RuntimeValue::Integer(n2)) => {
                if let Some(n) = n1.checked_mul(n2) {
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
            (x, y) => {Err(RuntimeError::new(GeneralError::new(format!("Type mismatch star ({:?}, {:?})", x, y))))}
        }
    }
    variadic (x, y, more) {
        match more {
            RuntimeValue::Vector(v) => {
                v.into_iter().try_fold(star::internal2(x,y)?, |x, y| star::internal2(x,y))
            }
            RuntimeValue::None => { star::internal2(x, y) }
            _ => unreachable!()
        }
    }
);

intrinsic_function!(
    get
    function (collection, key) {
        get::internal3(collection,key,RuntimeValue::None)
    }
    function (collection, key, default) {
        match (collection, key) {
            (RuntimeValue::Map(m), key) => {
                if let Some(value) = m.get(&key) {
                    Ok(value.clone())
                } else {
                    Ok(default)
                }
            }
            (RuntimeValue::Set(s), key) => {
                if s.contains(&key) {
                    Ok(key)
                } else {
                    Ok(default)
                }
            }
            (collection @ (RuntimeValue::Vector(_) | RuntimeValue::List(_) | RuntimeValue::String(_)), key @ RuntimeValue::Integer(_)) => {
                nth::internal3(collection, key, default)
            }
            _ => {Ok(default)}
        }
    }
);

intrinsic_function!(
    get_or_else
    function (collection, key, else_fn) {
        match (collection, key) {
            (RuntimeValue::Map(m), key) => {
                if let Some(value) = m.get(&key) {
                    Ok(value.clone())
                } else {
                    else_fn.evaluate_global_context()
                }
            }
            (RuntimeValue::Set(s), key) => {
                if s.contains(&key) {
                    Ok(key)
                } else {
                    else_fn.evaluate_global_context()
                }
            }
            (collection @ (RuntimeValue::Vector(_) | RuntimeValue::List(_) | RuntimeValue::String(_)), key @ RuntimeValue::Integer(_)) => {
                nth_or_else::internal3(collection, key, else_fn)
            }
            _ => { else_fn.evaluate_global_context()}
        }
    }
);

intrinsic_function!(
    nth
    function (collection, index) {
        match (collection, index) {
            (RuntimeValue::Vector(v), RuntimeValue::Integer(n)) => {
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
                if n < 0 || n as usize >= l.len() {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Index out of range {:?}", n)))))
                } else {
                    Ok(l.iter().nth(n as usize).unwrap())
               }
            }
            (RuntimeValue::String(s), RuntimeValue::Integer(n)) => {
                if n < 0 {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Index out of range {:?}", n)))))
                } else if let Some(c) = s.chars().skip(n as usize).next() {
                    Ok(RuntimeValue::Character(c))
                } else {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Index out of range {:?}", n)))))
                }
            }
            (RuntimeValue::Vector(_)|RuntimeValue::List(_)|RuntimeValue::String(_),_) => {
                Err(RuntimeError::new(GeneralError::new(String::from(format!("Index must be an integer")))))
            }
            (collection,_) => {Err(RuntimeError::new(GeneralError::new(String::from(format!("nth not supported for ({:?})", collection)))))}
        }
    }
    function (collection, index, default) {
        match (collection, index) {
            (RuntimeValue::Vector(v), RuntimeValue::Integer(n)) => {
                if n < 0 {
                    Ok(default)
                }
                else if let Some(v) = v.get(n as usize) {
                    Ok(v.clone())
                } else {
                    Ok(default)
               }
            }
            (RuntimeValue::List(l), RuntimeValue::Integer(n)) => {
                if n < 0 || n as usize >= l.len() {
                    Ok(default)
                } else {
                    Ok(l.iter().nth(n as usize).unwrap())
               }
            }
            (RuntimeValue::String(s), RuntimeValue::Integer(n)) => {
                if n < 0 {
                    Ok(default)
                } else if let Some(c) = s.chars().skip(n as usize).next() {
                    Ok(RuntimeValue::Character(c))
                } else {
                    Ok(default)
                }
            }
            (RuntimeValue::Vector(_)|RuntimeValue::List(_)|RuntimeValue::String(_),_) => {
                Err(RuntimeError::new(GeneralError::new(String::from(format!("Index must be an integer")))))
            }
            (collection,_) => {Err(RuntimeError::new(GeneralError::new(String::from(format!("nth not supported for ({:?})", collection)))))}
        }
    }
);

intrinsic_function!(
    nth_or_else
    function (collection, index, else_fn) {
        match (collection, index) {
            (RuntimeValue::Vector(v), RuntimeValue::Integer(n)) => {
                if n < 0 {
                    else_fn.evaluate(Context::new())
                }
                else if let Some(v) = v.get(n as usize) {
                    Ok(v.clone())
                } else {
                    else_fn.evaluate_global_context()
               }
            }
            (RuntimeValue::List(l), RuntimeValue::Integer(n)) => {
                if n < 0 || n as usize >= l.len() {
                    else_fn.evaluate(Context::new())
                } else {
                    Ok(l.iter().nth(n as usize).unwrap())
               }
            }
            (RuntimeValue::String(s), RuntimeValue::Integer(n)) => {
                if n < 0 {
                    else_fn.evaluate_global_context()
                } else if let Some(c) = s.chars().skip(n as usize).next() {
                    Ok(RuntimeValue::Character(c))
                } else {
                    else_fn.evaluate_global_context()
                }
            }
            (RuntimeValue::Vector(_)|RuntimeValue::List(_)|RuntimeValue::String(_),_) => {
                Err(RuntimeError::new(GeneralError::new(String::from(format!("Index must be an integer")))))
            }
            (collection,_) => {Err(RuntimeError::new(GeneralError::new(String::from(format!("nth not supported for ({:?})", collection)))))}
        }
    }
);

intrinsic_function!(
    nthrest
    function (collection, index) {
        match (collection, index) {
            (RuntimeValue::Vector(v), RuntimeValue::Integer(n)) => {
                if n < 0 {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n)))))
                }
               else {
                Ok(RuntimeValue::Vector(Box::new(v.into_iter().skip(n as usize).collect())))
               }
            }
            (RuntimeValue::List(l), RuntimeValue::Integer(n)) => {
                if n < 0 {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n)))))
                } else {
                    Ok(RuntimeValue::Vector(Box::new(l.into_iter().skip(n as usize).collect())))
               }
            }
            (RuntimeValue::String(s), RuntimeValue::Integer(n)) => {
                if n < 0 {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n)))))
                } else {
                    Ok(RuntimeValue::Vector(Box::new(s.chars().into_iter().skip(n as usize).map(|c|
                    RuntimeValue::Character(c)).collect())))
               }
            }
            (RuntimeValue::Map(m), RuntimeValue::Integer(n)) => {
                if n < 0 {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n)))))
                }
               else {
                    Ok(RuntimeValue::Vector(Box::new(
                        m.into_iter().skip(n as usize).map(
                            |(k,v)| RuntimeValue::Vector(Box::new(vector![k,v]))).collect())))
               }
            }
            (RuntimeValue::Set(s), RuntimeValue::Integer(n)) => {
                if n < 0 {
                    Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n)))))
                }
               else {
                Ok(RuntimeValue::Vector(Box::new(s.into_iter().skip(n as usize).collect())))
               }
            }
            (RuntimeValue::Vector(_)|RuntimeValue::List(_)|RuntimeValue::String(_)|RuntimeValue::Map(_)|RuntimeValue::Set(_),_) => {
                Err(RuntimeError::new(GeneralError::new(String::from(format!("Index must be an integer")))))
            }
            (collection,_) => {Err(RuntimeError::new(GeneralError::new(String::from(format!("nthrest not supported for ({:?})", collection)))))}
        }
    }
);

intrinsic_function!(
    nthnext
    function (collection, index) {
        match (collection, index) {
            (RuntimeValue::Vector(v), RuntimeValue::Integer(n)) => {
                match n {
                    n if n < 0 => Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n))))),
                    n if n as usize >= v.len() => Ok(RuntimeValue::None),
                    n => Ok(RuntimeValue::Vector(Box::new(v.into_iter().skip(n as usize).collect()))),
                }
            }
            (RuntimeValue::List(l), RuntimeValue::Integer(n)) => {
                match n {
                    n if n < 0 => Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n))))),
                    n if n as usize >= l.len() => Ok(RuntimeValue::None),
                    n => Ok(RuntimeValue::Vector(Box::new(l.into_iter().skip(n as usize).collect()))),
                }
            }
            (RuntimeValue::String(s), RuntimeValue::Integer(n)) => {
                match n {
                    n if n < 0 => Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n))))),
                    n if n as usize >= s.chars().count() => Ok(RuntimeValue::None),
                    n => Ok(RuntimeValue::Vector(Box::new(s.chars().into_iter().skip(n as usize).map(|c|
                            RuntimeValue::Character(c)).collect()))),
                }
            }
            (RuntimeValue::Map(m), RuntimeValue::Integer(n)) => {
                match n {
                    n if n < 0 => Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n))))),
                    n if n as usize >= m.len() => Ok(RuntimeValue::None),
                    n => Ok(RuntimeValue::Vector(Box::new(
                            m.into_iter().skip(n as usize).map(
                                |(k,v)| RuntimeValue::Vector(Box::new(vector![k,v]))).collect()))),
                }
            }
            (RuntimeValue::Set(s), RuntimeValue::Integer(n)) => {
                match n {
                    n if n < 0 => Err(RuntimeError::new(GeneralError::new(String::from(format!("Negative index{:?}", n))))),
                    n if n as usize >= s.len() => Ok(RuntimeValue::None),
                    n =>  Ok(RuntimeValue::Vector(Box::new(s.into_iter().skip(n as usize).collect()))),
                }
            }
            (RuntimeValue::Vector(_)|RuntimeValue::List(_)|RuntimeValue::String(_)|RuntimeValue::Map(_)|RuntimeValue::Set(_),_) => {
                Err(RuntimeError::new(GeneralError::new(String::from(format!("Index must be an integer")))))
            }
            (collection,_) => {Err(RuntimeError::new(GeneralError::new(String::from(format!("nthrest not supported for ({:?})", collection)))))}
        }
    }
);

intrinsic_function!(
    cons
    function (element, collection) {
        match collection {
            RuntimeValue::List(l) => {
                Ok(RuntimeValue::List(Box::new(List::cons(element,l.as_ref()))))
            },
            collection @ (RuntimeValue::Map(_) | RuntimeValue::Set(_) | RuntimeValue::Vector(_)) => {
                cons::internal2(element, seq::internal1(collection)?)
            }
            RuntimeValue::None => Ok(RuntimeValue::List(Box::new(list!(element)))),
            collection => {Err(RuntimeError::new(GeneralError::new(String::from(format!("cons not supported for ({:?})", collection)))))}
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
                    Ok(RuntimeValue::List(l))
                }
            }
            RuntimeValue::Map(m) => {
                if m.len() == 0 {
                    Ok(RuntimeValue::None)
                } else {
                    Ok(RuntimeValue::List(Box::new(
                        m.into_iter().map(|(k, v)| RuntimeValue::Vector(Box::new(vector![k,v]))).collect()
                    )))
                }
            }
            RuntimeValue::Set(s) => {
                if s.len() == 0 {
                    Ok(RuntimeValue::None)
                } else {
                    Ok(RuntimeValue::List(Box::new(
                        s.into_iter().collect()
                    )))
                }
            }
            RuntimeValue::Vector(v) => {
                if v.len() == 0 {
                    Ok(RuntimeValue::None)
                } else {
                    Ok(RuntimeValue::List(Box::new(
                        v.into_iter().rev().collect()
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
            collection => {Err(RuntimeError::new(GeneralError::new(String::from(format!("seq not supported for ({:?})", collection)))))}
        }
    }
);

intrinsic_function!(
    sequence
    function (collection) {
        match collection {
            collection @ RuntimeValue::List(_) => {
                Ok(collection)
            }
            RuntimeValue::Map(m) => {
                if m.len() == 0 {
                    Ok(RuntimeValue::List(Box::new(List::empty())))
                } else {
                    Ok(RuntimeValue::List(Box::new(
                        m.into_iter().map(|(k, v)| RuntimeValue::Vector(Box::new(vector![k,v]))).collect()
                    )))
                }
            }
            RuntimeValue::Set(s) => {
                if s.len() == 0 {
                    Ok(RuntimeValue::List(Box::new(List::empty())))
                } else {
                    Ok(RuntimeValue::List(Box::new(
                        s.into_iter().collect()
                    )))
                }
            }
            RuntimeValue::Vector(v) => {
                if v.len() == 0 {
                    Ok(RuntimeValue::List(Box::new(List::empty())))
                } else {
                    Ok(RuntimeValue::List(Box::new(
                        v.into_iter().rev().collect()
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
            collection => {Err(RuntimeError::new(GeneralError::new(String::from(format!("sequence not supported for ({:?})", collection)))))}
        }
    }
);

intrinsic_function!(
    first
    function (collection) {
        match collection {
            RuntimeValue::None => Ok(RuntimeValue::None),
            collection @ (RuntimeValue::List(_) | RuntimeValue::Vector(_) | RuntimeValue::String(_)) => {
                nth::internal3(collection, RuntimeValue::Integer(0), RuntimeValue::None)
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
            collection => {Err(RuntimeError::new(GeneralError::new(String::from(format!("first not supported for ({:?})", collection)))))}
        }
    }
);

intrinsic_function!(
    rest
    function (collection) {
        match collection {
            RuntimeValue::None => Ok(RuntimeValue::List(Box::new(List::empty()))),
            RuntimeValue::List(_) | RuntimeValue::Vector(_) | RuntimeValue::String(_) | RuntimeValue::Map(_) |RuntimeValue::Set(_)=> {
                nthrest::internal2(collection, RuntimeValue::Integer(1))
            }
            collection => {Err(RuntimeError::new(GeneralError::new(String::from(format!("rest not supported for ({:?})", collection)))))}
        }
    }
);

intrinsic_function!(
    next
    function (collection) {
        match collection {
            RuntimeValue::None => Ok(RuntimeValue::List(Box::new(List::empty()))),
            collection @ (RuntimeValue::List(_) | RuntimeValue::Vector(_) | RuntimeValue::String(_) | RuntimeValue::Map(_) |RuntimeValue::Set(_)) => {
                nthnext::internal2(collection, RuntimeValue::Integer(1))
            }
            collection => {Err(RuntimeError::new(GeneralError::new(String::from(format!("next not supported for ({:?})", collection)))))}
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
                Err(RuntimeError::Recur(vector![y, first::internal1(more.clone())?, next::internal1(more)?]))
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
                    v.into_iter().rev().collect()
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
                let seqs: Result<Vec<_>,_> =
                v.into_iter().map(|v| sequence::internal1(v))
                .flat_map(|value| match value {
                    Ok(RuntimeValue::List(l)) => {
                        l.into_iter().map(|value| Ok(value)).collect()
                    }
                    Ok(_) => unreachable!(),
                    e @ Err(_) => vec![e],
                })
                .collect();
                match seqs {
                    Ok(v) => Ok(RuntimeValue::List(Box::new(v.into_iter().collect()))),
                    Err(e) => Err(RuntimeError::new(GeneralError::new(String::from(format!("concat not supported for ({:?})", e))))),
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
                let v1: Vec<_> = l1.into_iter().collect();
                let v2: Vec<_> = l2.into_iter().collect();
                Ok(RuntimeValue::List(Box::new(
                    v2.into_iter().rev().chain(
                        v1.into_iter().rev()).collect())))
            }
            (RuntimeValue::List(l),RuntimeValue::Map(m)) => {
                let v: Vec<_> = l.into_iter().collect();
                Ok(RuntimeValue::List(Box::new(
                    m.into_iter().map(|(k,v)|
                    RuntimeValue::Vector(Box::new(vector![k, v]))
                        ).chain(v.into_iter().rev()).collect())))
            }
            (RuntimeValue::List(l),RuntimeValue::Vector(v2)) => {
                let v1: Vec<_> = l.into_iter().collect();
                Ok(RuntimeValue::List(Box::new(
                    v2.into_iter().rev().chain(
                        v1.into_iter().rev()).collect())))
            }
            (RuntimeValue::List(l),RuntimeValue::Set(s)) => {
                let v: Vec<_> = l.into_iter().collect();
                Ok(RuntimeValue::List(Box::new(
                    s.into_iter().chain(
                        v.into_iter().rev()).collect())))
            }
            (RuntimeValue::List(l),RuntimeValue::String(s)) => {
                let v: Vec<_> = l.into_iter().collect();
                Ok(RuntimeValue::List(Box::new(
                    s.chars().map(|c| RuntimeValue::Character(c))
                        .rev().chain(v.into_iter().rev()).collect())))
            }

            // into Map
            (RuntimeValue::Map(m),RuntimeValue::List(l)) => {
                let m1: Result<HashMap<_,_>,_> = l.into_iter().map(
                    |v| match v {
                        RuntimeValue::Vector(mut v) if v.len() == 2 =>
                        match (v.pop_front(), v.pop_front()) {
                            (Some(v1), Some(v2)) => Ok((v1, v2)),
                            _ => unreachable!(),
                        },
                        _ => Err(RuntimeError::new(GeneralError::new(String::from(format!("into map from list must have [key values]")))))
                    }
                ).collect();
                let m1 = m1?;
                Ok(RuntimeValue::Map(Box::new(m.union(m1))))
            }
            (RuntimeValue::Map(m1),RuntimeValue::Map(m2)) => Ok(RuntimeValue::Map(Box::new(m1.union(*m2)))),
            (RuntimeValue::Map(m),RuntimeValue::Vector(v)) => {
                let m1: Result<HashMap<_,_>,_> = v.into_iter().map(
                    |v| match v {
                        RuntimeValue::Vector(mut v) if v.len() == 2 =>
                        match (v.pop_front(), v.pop_front()) {
                            (Some(v1), Some(v2)) => Ok((v1, v2)),
                            _ => unreachable!(),
                        },
                        _ => Err(RuntimeError::new(GeneralError::new(String::from(format!("into map from vector must have [key values]")))))
                    }
                ).collect();
                let m1 = m1?;
                Ok(RuntimeValue::Map(Box::new(m.union(m1))))
            }

            // into Set
            (RuntimeValue::Set(s),RuntimeValue::List(l)) => {
                let s1: HashSet<_> = l.into_iter().collect();
                Ok(RuntimeValue::Set(Box::new(s.union(s1))))
            }
            (RuntimeValue::Set(s),RuntimeValue::Map(m)) => {
                let s1: HashSet<_> = m.into_iter().map(|(k,v)|
                        RuntimeValue::Vector(Box::new(vector![k, v]))
                    ).collect();
                Ok(RuntimeValue::Set(Box::new(s.union(s1))))
            }
            (RuntimeValue::Set(s1),RuntimeValue::Set(s2)) => {
                Ok(RuntimeValue::Set(Box::new(s1.union(*s2))))
            }
            (RuntimeValue::Set(s),RuntimeValue::Vector(v)) => {
                let s1: HashSet<_> = v.into_iter().collect();
                Ok(RuntimeValue::Set(Box::new(s.union(s1))))
            }
            (RuntimeValue::Set(s1),RuntimeValue::String(s2)) => {
                let s2: HashSet<_> = s2.chars().into_iter().map(|c|
                        RuntimeValue::Character(c)
                    ).collect();
                Ok(RuntimeValue::Set(Box::new(s1.union(s2))))
            }

            // into Vector
            (RuntimeValue::Vector(mut v1),RuntimeValue::List(l)) => {
                let v2: Vector<_> = l.into_iter().collect();
                v1.append(v2);
                Ok(RuntimeValue::Vector(v1))
            }
            (RuntimeValue::Vector(mut v1),RuntimeValue::Map(m)) => {
                let v2: Vector<_> = m.into_iter().map(|(k,v)|
                        RuntimeValue::Vector(Box::new(vector![k, v]))
                    ).collect();
                v1.append(v2);
                Ok(RuntimeValue::Vector(v1))
            }
            (RuntimeValue::Vector(mut v1),RuntimeValue::Set(s)) => {
                let v2: Vector<_> = s.into_iter().collect();
                v1.append(v2);
                Ok(RuntimeValue::Vector(v1))
            }
            (RuntimeValue::Vector(mut v1),RuntimeValue::Vector(v2)) => {
                v1.append(*v2);
                Ok(RuntimeValue::Vector(v1))
            }
            (RuntimeValue::Vector(mut v1),RuntimeValue::String(s)) => {
                let v2: Vector<_> = s.chars().into_iter().map(|c|
                        RuntimeValue::Character(c)
                    ).collect();
                v1.append(v2);
                Ok(RuntimeValue::Vector(v1))
            }

            (RuntimeValue::String(s1),RuntimeValue::List(l)) => {
                let s1: Result<String,_> = s1.chars().into_iter().map(|c| Ok(c)).chain(
                    l.into_iter().map(
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
                    s2.into_iter().map(
                    |v| match v {
                        RuntimeValue::Character(c)  =>
                        Ok(c),
                        _ => Err(RuntimeError::new(GeneralError::new(String::from(format!("into string from list must have char values"))))),
                    })
                ).collect();
                Ok(RuntimeValue::String(Box::new(s1?)))
            }
            (RuntimeValue::String(s1),RuntimeValue::Vector(v)) => {
                let s1: Result<String,_> = s1.chars().into_iter().map(|c| Ok(c)).chain(
                    v.into_iter().map(
                    |v| match v {
                        RuntimeValue::Character(c)  =>
                        Ok(c),
                        _ => Err(RuntimeError::new(GeneralError::new(String::from(format!("into string from list must have char values"))))),
                    })
                ).collect();
                Ok(RuntimeValue::String(Box::new(s1?)))
            }
            (RuntimeValue::String(s1),RuntimeValue::String(s2)) => {
                Ok(RuntimeValue::String(Box::new(format!("{}{}",s1 ,s2))))
            }

            (to,from) => Err(RuntimeError::new(GeneralError::new(String::from(format!("into not supported for ({:?} into {:?})", from, to))))),
        }
    }
);

intrinsic_function!(
    apply
    variadic (f, more) {
        match into::internal2(RuntimeValue::Vector(Box::new(Vector::new())), concat::internal1(more)?)? {
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
