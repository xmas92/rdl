use std::{
    cmp,
    convert::{From, TryFrom},
    hash::Hash,
    mem,
    ops::{Add, Div, Mul, Rem, Sub},
};

use bigdecimal::BigDecimal;
use num::{complex::Complex64, rational::Rational64, BigInt, BigRational, Complex, Integer};
use num_traits::{
    CheckedAdd, CheckedDiv, CheckedMul, CheckedRem, CheckedSub, One, ToPrimitive, Zero,
    
};

pub type BigComplex = Complex<BigDecimal>;

///
///
///
#[derive(Clone, Debug)]
pub enum Number {
    Int(i64),
    Float(f64),
    Ratio(Rational64),
    Complex(Complex64),
    BigInt(Box<BigInt>),
    BigFloat(Box<BigDecimal>),
    BigRatio(Box<BigRational>),
    BigComplex(Box<BigComplex>),
}

// Promote Rules
//

pub struct ParseNumberError {}

impl Eq for Number {}

// impl Num for Number {
//     type FromStrRadixErr = ParseNumberError;

//     fn from_str_radix(str: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
//         todo!()
//     }
// }

macro_rules! match_macro_internal {
    (@parse {$($match_tree:tt)*};$match:expr,) => {
        match $match {
            $($match_tree)*
        }
    };
    (
        @parse {$($match_tree:tt)*};
        $match:expr,
        $_:ident {}
        $($tail:tt)*
    ) => {
        match_macro_internal! {
            @parse {$($match_tree)*};
            $match,
            $($tail)*
        }
    };
    (
        @parse {$($match_tree:tt)*};
        $match:expr,
        arm {
            $var:ident => $expr:expr,
            $($types:path),*
        }
        $($tail:tt)*
    ) => {
        match_macro_internal! {
            @parse {
                $($match_tree)*
                $($types($var) => $expr,)*
            };
            $match,
            $($tail)*
        }
    };
    (
        @parse {$($match_tree:tt)*};
        $match:expr,
        arm {
            ($($var:ident)+) => $expr:expr,
            $(($($types:path)+)),*
        }
        $($tail:tt)*
    ) => {
        match_macro_internal! {
            @parse {
                $($match_tree)*
                $(($($types($var),)+) => $expr,)*
            };
            $match,
            $($tail)*
        }
    };
    (
        @parse {$($match_tree:tt)*};
        $match:expr,
        bin_sym {
            ($v1:ident, $v2:ident) => $expr:expr,
            $(($ty1:path,$ty2:path)),*
        }
        $($tail:tt)*
    ) => {
        match_macro_internal! {
            @parse {
                $($match_tree)*
                $(($ty1($v1),$ty2($v2)) | ($ty2($v2),$ty1($v1)) => $expr,)*
            };
            $match,
            $($tail)*
        }
    };
    (
        @parse {$($match_tree:tt)*};
        $match:expr,
        bin_sym {
            ($v1:ident, $v2:ident) => $expr:expr,
            $($types:path),*
        }
        $($tail:tt)*
    ) => {
        match_macro_internal! {
            @parse {
                $($match_tree)*
                $(($types($v1),$types($v2)) => $expr,)*
            };
            $match,
            $($tail)*
        }
    };
    (
        @parse {$($match_tree:tt)*};
        $match:expr,
        bin_asym {
            ($v1:ident, $v2:ident) => ($e1:expr,$e2:expr),
            $(($ty1:path,$ty2:path)),*
        }
        $($tail:tt)*
    ) => {
        match_macro_internal! {
            @parse {
                $($match_tree)*
                $(
                    ($ty1($v1),$ty2($v2)) => $e1,
                    ($ty2($v1),$ty1($v2)) => $e2,
                )*
            };
            $match,
            $($tail)*
        }
    };
    (
        @parse {$($match_tree:tt)*};
        $match:expr,
        rest {
            _ => $expr:expr
        }
        $($tail:tt)*
    ) => {
        match_macro_internal! {
            @parse {
                $($match_tree)*
                _ => $expr,
            };
            $match,
            $($tail)*
        }
    };
    (
        @parse {$($match_tree:tt)*};
        $match:expr,
        rest {
            $var:ident => $expr:expr
        }
        $($tail:tt)*
    ) => {
        match_macro_internal! {
            @parse {
                $($match_tree)*
                $var => $expr,
            };
            $match,
            $($tail)*
        }
    };
    (
        @parse {$($match_tree:tt)*};
        $match:expr,
        ensure_rest {
            _ => $expr:expr,
            $(($ty1:path,$ty2:path)),* $(,)*
            $($types:path),*
        }
        $($tail:tt)*
    ) => {
        match_macro_internal! {
            @parse {
                $($match_tree)*
                $(
                    #[cfg(debug_assertions)]
                    ($ty1(_),$ty2(_)) | ($ty2(_),$ty1(_)) => $expr,
                )*
                $(
                    #[cfg(debug_assertions)]
                    ($types(_),$types(_)) => $expr,
                )*
                #[cfg(not(debug_assertions))]
                _  => $expr,

            };
            $match,
            $($tail)*
        }
    };
    (
        @parse {$($match_tree:tt)*};
        $match:expr,
        ensure_rest {
            $var:ident => $expr:expr,
            $(($ty1:path,$ty2:path)),* $(,)*
            $($types:path),*
        }
        $($tail:tt)*
    ) => {
        match_macro_internal! {
            @parse {
                $($match_tree)*
                $(
                    #[cfg(debug_assertions)]
                    $var @ ($ty1(_),$ty2(_)) | ($ty2(_),$ty1(_)) => $expr,
                )*
                $(
                    #[cfg(debug_assertions)]
                    ($types(_),$types(_)) => $expr,
                )*
                #[cfg(not(debug_assertions))]
                $var  => $expr,

            };
            $match,
            $($tail)*
        }
    };
    (
        @parse {$($match_tree:tt)*};
        $match:expr,
        ensure_rest {
            ($v1:ident, $v2:ident) => $expr:expr,
            $(($ty1:path,$ty2:path)),* $(,)*
            $($types:path),*
        }
        $($tail:tt)*
    ) => {
        match_macro_internal! {
            @parse {
                $($match_tree)*
                $(
                    #[cfg(debug_assertions)]
                    ($v1 @ $ty1(_),$v2 @ $ty2(_)) | ($v2 @ $ty2(_),$v1 @ $ty1(_)) => $expr,
                )*
                $(
                    #[cfg(debug_assertions)]
                    ($types($v1),$types($v2)) => $expr,
                )*
                #[cfg(not(debug_assertions))]
                ($v1, $v2)  => $expr,

            };
            $match,
            $($tail)*
        }
    };
}
macro_rules! match_macro {
    ($($cmd:tt)*) => {
        match_macro_internal!{@parse {};$($cmd)*}
    };
}

impl Zero for Number {
    fn zero() -> Self {
        Number::Int(0)
    }

    fn is_zero(&self) -> bool {
        match_macro! {
            self,
            arm {
                v => v.is_zero(),
                Number::Int,
                Number::Float,
                Number::Ratio,
                Number::Complex,
                Number::BigInt,
                Number::BigFloat,
                Number::BigRatio,
                Number::BigComplex
            }
        }
    }
}

impl One for Number {
    fn one() -> Self {
        Number::Int(1)
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        match_macro! {
            (self, other),
            bin_sym {
                (v1, v2) => cmp::PartialEq::eq(v1,v2),
                Number::Int,
                Number::Float,
                Number::Ratio,
                Number::Complex,
                Number::BigInt,
                Number::BigFloat,
                Number::BigRatio,
                Number::BigComplex
            }
            bin_sym {
                (v1, v2) => cmp::PartialEq::eq(v1,&Promote::<Rational64>::promote(v2)),
                (Number::Ratio,Number::Int)
            }
            bin_sym {
                (v1, v2) => cmp::PartialEq::eq(&Promote::<BigRational>::promote(v1),&Promote::<BigRational>::promote(v2)),
                (Number::Ratio,Number::BigInt)
            }
            bin_sym {
                (v1, v2) => cmp::PartialEq::eq(v1.as_ref(),&Promote::<BigInt>::promote(v2)),
                (Number::BigInt,Number::Int)
            }
            bin_sym {
                (v1, v2) => cmp::PartialEq::eq(v1,&Promote::<Complex64>::promote(v2)),
                (Number::Complex,Number::Float)
            }
            bin_sym {
                (v1, v2) => cmp::PartialEq::eq(v1.as_ref(),&Promote::<BigRational>::promote(v2)),
                (Number::BigRatio,Number::Int),
                (Number::BigRatio,Number::Ratio),
                (Number::BigRatio,Number::BigInt)
            }
            bin_sym {
                (v1, v2) => cmp::PartialEq::eq(v1.as_ref(),&Promote::<BigDecimal>::promote(v2)),
                (Number::BigFloat,Number::Int),
                (Number::BigFloat,Number::Ratio),
                (Number::BigFloat,Number::BigInt),
                (Number::BigFloat,Number::BigRatio)
            }
            bin_sym {
                (v1, v2) => cmp::PartialEq::eq(v1.as_ref(),&Promote::<BigComplex>::promote(v2)),
                (Number::BigComplex,Number::Int),
                (Number::BigComplex,Number::Ratio),
                (Number::BigComplex,Number::BigInt),
                (Number::BigComplex,Number::BigFloat),
                (Number::BigComplex,Number::BigRatio)
            }
            ensure_rest {
                _ => false,
                (Number::Float,Number::Int),
                (Number::Float,Number::Ratio),
                (Number::Float,Number::BigInt),
                (Number::Float,Number::BigFloat),
                (Number::Float,Number::BigRatio),
                (Number::Float,Number::BigComplex),
                (Number::Complex,Number::Int),
                (Number::Complex,Number::Ratio),
                (Number::Complex,Number::BigInt),
                (Number::Complex,Number::BigFloat),
                (Number::Complex,Number::BigRatio),
                (Number::Complex,Number::BigComplex)
            }
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match_macro! {
            (self, other),
            bin_sym {
                (v1, v2) => cmp::PartialOrd::partial_cmp(v1,v2),
                Number::Int,
                Number::Float,
                Number::Ratio,
                Number::BigInt,
                Number::BigFloat,
                Number::BigRatio
            }
            bin_asym {
                (v1, v2) => (
                    cmp::PartialOrd::partial_cmp(v1,&Promote::<Rational64>::promote(v2)),
                    cmp::PartialOrd::partial_cmp(&Promote::<Rational64>::promote(v1),v2)
                ),
                (Number::Ratio,Number::Int)
            }
            bin_asym {
                (v1, v2) => (
                    v2.to_f64().and_then(|ref v2|cmp::PartialOrd::partial_cmp(v1,v2)),
                    v1.to_f64().and_then(|ref v1|cmp::PartialOrd::partial_cmp(v1,v2))
                ),
                (Number::Float,Number::Int),
                (Number::Float,Number::Ratio),
                (Number::Float,Number::BigInt),
                (Number::Float,Number::BigFloat),
                (Number::Float,Number::BigRatio)
            }
            bin_sym {
                (v1, v2) => cmp::PartialOrd::partial_cmp(&Promote::<BigRational>::promote(v1),&Promote::<BigRational>::promote(v2)),
                (Number::Ratio,Number::BigInt)
            }
            bin_asym {
                (v1, v2) => (
                    cmp::PartialOrd::partial_cmp(v1.as_ref(),&Promote::<BigInt>::promote(v2)),
                    cmp::PartialOrd::partial_cmp(&Promote::<BigInt>::promote(v1),v2.as_ref())
                ),
                (Number::BigInt,Number::Int)
            }
            bin_asym {
                (v1, v2) => (
                    cmp::PartialOrd::partial_cmp(v1.as_ref(),&Promote::<BigRational>::promote(v2)),
                    cmp::PartialOrd::partial_cmp(&Promote::<BigRational>::promote(v1), v2.as_ref())
                ),
                (Number::BigRatio,Number::Int),
                (Number::BigRatio,Number::Ratio),
                (Number::BigRatio,Number::BigInt)
            }
            bin_asym {
                (v1, v2) => (
                    cmp::PartialOrd::partial_cmp(v1.as_ref(),&Promote::<BigDecimal>::promote(v2)),
                    cmp::PartialOrd::partial_cmp(&Promote::<BigDecimal>::promote(v1),v2.as_ref())
                ),
                (Number::BigFloat,Number::Int),
                (Number::BigFloat,Number::Ratio),
                (Number::BigFloat,Number::BigInt),
                (Number::BigFloat,Number::BigRatio)
            }
            ensure_rest {
                _ => None,
                (Number::Complex,Number::Int),
                (Number::Complex,Number::Ratio),
                (Number::Complex,Number::Float),
                (Number::Complex,Number::BigInt),
                (Number::Complex,Number::BigFloat),
                (Number::Complex,Number::BigRatio),
                (Number::Complex,Number::BigComplex),
                (Number::BigComplex,Number::Int),
                (Number::BigComplex,Number::Float),
                (Number::BigComplex,Number::Ratio),
                (Number::BigComplex,Number::BigInt),
                (Number::BigComplex,Number::BigFloat),
                (Number::BigComplex,Number::BigRatio),
                Number::BigComplex,
                Number::Complex
            }
        }
    }
}

impl Ord for Number {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // TODO Define a total ordering on floats? Like java. Probably need to change Eq then too.
        // same for Complex but here almost any mathematical operation breaks it, while for floats
        // it is only for NaN. Should probably always panic at this level and actually check the
        // the type usage at a higher order. Like never use complex or float as keys in orderer
        // (nor hashed for that matter) data structures.
        cmp::PartialOrd::partial_cmp(self, other).unwrap()
    }
}

impl Hash for Number {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Number::Int(v) => v.hash(state),
            Number::Float(v) => {
                // TODO Decide how to handle this for floats so it is consistent with Eq etc.
                // see comment in Ord
                unsafe { mem::transmute::<f64, u64>(*v).hash(state) }
            }
            Number::Ratio(v) => v.hash(state),
            Number::Complex(v) => {
                // TODO Decide how to handle this for floats so it is consistent with Eq etc.
                // see comment in Ord
                unsafe { mem::transmute::<f64, u64>(v.re).hash(state) }
                unsafe { mem::transmute::<f64, u64>(v.im).hash(state) }
            }
            Number::BigInt(v) => v.hash(state),
            Number::BigFloat(v) => v.hash(state),
            Number::BigRatio(v) => v.hash(state),
            Number::BigComplex(v) => v.hash(state),
        }
    }
}

macro_rules! define_binop_internal {
    (
        $imp:ident, $method:ident, $checked_imp:ident, $checked_method:ident
        $match:expr
    ) => {
        match $match {
            (Number::Int(v1), Number::Int(v2)) => match $checked_imp::$checked_method(v1, v2) {
                Some(n) => n.into(),
                None => $imp::$method(Into::<BigInt>::into(*v1), v2).into(),
            },
            (Number::Int(v1), Number::Float(v2)) => $imp::$method(*v1 as f64, v2).into(),
            (Number::Float(v1), Number::Int(v2)) => $imp::$method(v1, *v2 as f64).into(),
            (Number::Int(v1), Number::Ratio(v2)) => {
                match $checked_imp::$checked_method(&Into::<Rational64>::into(*v1), v2) {
                    Some(n) => {
                        if n.is_integer() {
                            n.numer().into()
                        } else {
                            n.into()
                        }
                    }
                    None => {
                        let n = $imp::$method(
                            Promote::<BigRational>::promote(v1),
                            Promote::<BigRational>::promote(v2),
                        );
                        if n.is_integer() {
                            n.numer().into()
                        } else {
                            n.into()
                        }
                    }
                }
            }
            (Number::Ratio(v1), Number::Int(v2)) => {
                match $checked_imp::$checked_method(v1, &Promote::<Rational64>::promote(v2)) {
                    Some(n) => {
                        if n.is_integer() {
                            n.numer().into()
                        } else {
                            n.into()
                        }
                    }
                    None => {
                        let n = $imp::$method(
                            Promote::<BigRational>::promote(v1),
                            Promote::<BigRational>::promote(v2),
                        );
                        if n.is_integer() {
                            n.numer().into()
                        } else {
                            n.into()
                        }
                    }
                }
            }
            (Number::Float(v1), Number::Float(v2)) => $imp::$method(v1, v2).into(),
            (Number::Float(v1), Number::Ratio(v2)) => {
                $imp::$method(v1, Promote::<f64>::promote(v2)).into()
            }
            (Number::Ratio(v1), Number::Float(v2)) => {
                $imp::$method(Promote::<f64>::promote(v1), v2).into()
            }
            (Number::Int(v1), Number::Complex(v2)) => {
                $imp::$method(Into::<Complex64>::into(*v1 as f64), v2).into()
            }
            (Number::Complex(v1), Number::Int(v2)) => {
                $imp::$method(v1, Into::<Complex64>::into(*v2 as f64)).into()
            }
            (Number::Int(v1), Number::BigInt(v2)) => {
                $imp::$method(Into::<BigInt>::into(*v1), v2.as_ref()).into()
            }
            (Number::BigInt(v1), Number::Int(v2)) => {
                $imp::$method(v1.as_ref(), Into::<BigInt>::into(*v2)).into()
            }
            (Number::Int(v1), Number::BigFloat(v2)) => {
                $imp::$method(Into::<BigDecimal>::into(*v1), v2.as_ref()).into()
            }
            (Number::BigFloat(v1), Number::Int(v2)) => {
                $imp::$method(v1.as_ref(), Into::<BigDecimal>::into(*v2)).into()
            }
            (Number::Int(v1), Number::BigRatio(v2)) => {
                let n = $imp::$method(&Promote::<BigRational>::promote(v1), v2.as_ref());
                if n.is_integer() {
                    n.numer().into()
                } else {
                    n.into()
                }
            }
            (Number::Int(v1), Number::BigComplex(v2)) => {
                $imp::$method(Promote::<BigComplex>::promote(v1), v2.as_ref()).into()
            }
            (Number::Float(v1), Number::Complex(v2)) => $imp::$method(v1, v2).into(),
            (Number::Float(v1), Number::BigInt(v2)) => {
                if let Ok(v1) = BigDecimal::try_from(*v1) {
                    $imp::$method(v1, Into::<BigDecimal>::into(v2.as_ref().clone())).into()
                } else {
                    $imp::$method(v1, v2.as_ref().to_f64().unwrap_or(f64::NAN)).into()
                }
            }
            (Number::Float(v1), Number::BigFloat(v2)) => {
                if let Ok(v1) = BigDecimal::try_from(*v1) {
                    $imp::$method(v1, v2.as_ref()).into()
                } else {
                    $imp::$method(v1, v2.as_ref().to_f64().unwrap_or(f64::NAN)).into()
                }
            }
            (Number::Float(v1), Number::BigRatio(v2)) => {
                if let Ok(v1) = BigDecimal::try_from(*v1) {
                    $imp::$method(v1, Promote::<BigDecimal>::promote(v2.as_ref())).into()
                } else {
                    $imp::$method(v1, v2.as_ref().to_f64().unwrap_or(f64::NAN)).into()
                }
            }
            (Number::Float(v1), Number::BigComplex(v2)) => {
                if let Ok(v1) = BigDecimal::try_from(*v1) {
                    $imp::$method(Promote::<BigComplex>::promote(v1), v2.as_ref()).into()
                } else {
                    $imp::$method(
                        Promote::<Complex64>::promote(v1),
                        Complex64::new(
                            v2.re.to_f64().unwrap_or(f64::NAN),
                            v2.im.to_f64().unwrap_or(f64::NAN),
                        ),
                    )
                    .into()
                }
            }
            (Number::Ratio(v1), Number::Ratio(v2)) => match $checked_imp::$checked_method(v1, v2) {
                Some(n) => {
                    if n.is_integer() {
                        n.numer().into()
                    } else {
                        n.into()
                    }
                }
                None => {
                    let n = $imp::$method(
                        Promote::<BigRational>::promote(v1),
                        Promote::<BigRational>::promote(v2),
                    );
                    if n.is_integer() {
                        n.numer().into()
                    } else {
                        n.into()
                    }
                }
            },
            (Number::Ratio(v1), Number::Complex(v2)) => {
                $imp::$method(Promote::<Complex64>::promote(v1), v2).into()
            }
            (Number::Ratio(v1), Number::BigInt(v2)) => {
                let n = $imp::$method(Promote::<BigRational>::promote(v1), v2.as_ref());
                if n.is_integer() {
                    n.numer().into()
                } else {
                    n.into()
                }
            }
            (Number::Ratio(v1), Number::BigFloat(v2)) => {
                $imp::$method(Promote::<BigDecimal>::promote(v1), v2.as_ref()).into()
            }
            (Number::Ratio(v1), Number::BigRatio(v2)) => {
                let n = $imp::$method(Promote::<BigRational>::promote(v1), v2.as_ref());
                if n.is_integer() {
                    n.numer().into()
                } else {
                    n.into()
                }
            }
            (Number::Ratio(v1), Number::BigComplex(v2)) => {
                $imp::$method(Promote::<BigComplex>::promote(v1), v2.as_ref()).into()
            }
            (Number::Complex(v1), Number::Float(v2)) => $imp::$method(v1, v2).into(),
            (Number::Complex(v1), Number::Ratio(v2)) => {
                $imp::$method(v1, *v2.numer() as f64 / *v2.denom() as f64).into()
            }
            (Number::Complex(v1), Number::Complex(v2)) => $imp::$method(v1, v2).into(),
            (Number::Complex(v1), Number::BigInt(v2)) => {
                if let (Ok(im), Ok(re)) = (BigDecimal::try_from(v1.im), BigDecimal::try_from(v1.re))
                {
                    $imp::$method(
                        Complex::new(re, im),
                        Promote::<BigComplex>::promote(v2.as_ref()),
                    )
                    .into()
                } else {
                    $imp::$method(v1, v2.as_ref().to_f64().unwrap_or(f64::NAN)).into()
                }
            }
            (Number::Complex(v1), Number::BigFloat(v2)) => {
                if let (Ok(im), Ok(re)) = (BigDecimal::try_from(v1.im), BigDecimal::try_from(v1.re))
                {
                    $imp::$method(
                        Complex::new(re, im),
                        Promote::<BigComplex>::promote(v2.as_ref()),
                    )
                    .into()
                } else {
                    $imp::$method(v1, v2.as_ref().to_f64().unwrap_or(f64::NAN)).into()
                }
            }
            (Number::Complex(v1), Number::BigRatio(v2)) => {
                if let (Ok(im), Ok(re)) = (BigDecimal::try_from(v1.im), BigDecimal::try_from(v1.re))
                {
                    $imp::$method(
                        Complex::new(re, im),
                        Promote::<BigComplex>::promote(v2.as_ref()),
                    )
                    .into()
                } else {
                    $imp::$method(v1, v2.as_ref().to_f64().unwrap_or(f64::NAN)).into()
                }
            }
            (Number::Complex(v1), Number::BigComplex(v2)) => {
                if let (Ok(im), Ok(re)) = (BigDecimal::try_from(v1.im), BigDecimal::try_from(v1.re))
                {
                    $imp::$method(Complex::new(re, im), v2.as_ref()).into()
                } else {
                    $imp::$method(
                        v1,
                        Complex64::new(
                            v2.re.to_f64().unwrap_or(f64::NAN),
                            v2.im.to_f64().unwrap_or(f64::NAN),
                        ),
                    )
                    .into()
                }
            }
            (Number::BigInt(v1), Number::Float(v2)) => {
                if let Ok(v2) = BigDecimal::try_from(*v2) {
                    $imp::$method(Into::<BigDecimal>::into(v1.as_ref().clone()), v2).into()
                } else {
                    $imp::$method(v1.as_ref().to_f64().unwrap_or(f64::NAN), v2).into()
                }
            }
            (Number::BigInt(v1), Number::Ratio(v2)) => {
                let n = $imp::$method(
                    Into::<BigRational>::into(v1.as_ref().clone()),
                    Promote::<BigRational>::promote(v2),
                );
                if n.is_integer() {
                    n.numer().into()
                } else {
                    n.into()
                }
            }
            (Number::BigInt(v1), Number::Complex(v2)) => {
                if let (Ok(im), Ok(re)) = (BigDecimal::try_from(v2.im), BigDecimal::try_from(v2.re))
                {
                    $imp::$method(
                        Promote::<BigComplex>::promote(v1.as_ref()),
                        Complex::new(re, im),
                    )
                    .into()
                } else {
                    $imp::$method(v1.as_ref().to_f64().unwrap_or(f64::NAN), v2).into()
                }
            }
            (Number::BigInt(v1), Number::BigInt(v2)) => {
                $imp::$method(v1.as_ref(), v2.as_ref()).into()
            }
            (Number::BigInt(v1), Number::BigFloat(v2)) => {
                $imp::$method(Into::<BigDecimal>::into(v1.as_ref().clone()), v2.as_ref()).into()
            }
            (Number::BigInt(v1), Number::BigRatio(v2)) => {
                let n = $imp::$method(Into::<BigRational>::into(v1.as_ref().clone()), v2.as_ref());
                if n.is_integer() {
                    n.numer().into()
                } else {
                    n.into()
                }
            }
            (Number::BigInt(v1), Number::BigComplex(v2)) => {
                $imp::$method(Promote::<BigComplex>::promote(v1.as_ref()), v2.as_ref()).into()
            }
            (Number::BigFloat(v1), Number::Float(v2)) => {
                if let Ok(v2) = BigDecimal::try_from(*v2) {
                    $imp::$method(Into::<BigDecimal>::into(v1.as_ref().clone()), v2).into()
                } else {
                    $imp::$method(v1.as_ref().to_f64().unwrap_or(f64::NAN), v2).into()
                }
            }
            (Number::BigFloat(v1), Number::Ratio(v2)) => {
                if let Ok(v2) = BigDecimal::try_from(*v2.numer() as f64 / *v2.denom() as f64) {
                    $imp::$method(v1.as_ref(), v2).into()
                } else {
                    $imp::$method(
                        v1.as_ref().to_f64().unwrap_or(f64::NAN),
                        v2.to_f64().unwrap_or(f64::NAN),
                    )
                    .into()
                }
            }
            (Number::BigFloat(v1), Number::Complex(v2)) => {
                if let (Ok(im), Ok(re)) = (BigDecimal::try_from(v2.im), BigDecimal::try_from(v2.re))
                {
                    $imp::$method(
                        Promote::<BigComplex>::promote(v1.as_ref()),
                        Complex::new(re, im),
                    )
                    .into()
                } else {
                    $imp::$method(v1.as_ref().to_f64().unwrap_or(f64::NAN), v2).into()
                }
            }
            (Number::BigFloat(v1), Number::BigInt(v2)) => {
                $imp::$method(v1.as_ref(), Into::<BigDecimal>::into(v2.as_ref().clone())).into()
            }
            (Number::BigFloat(v1), Number::BigFloat(v2)) => {
                $imp::$method(v1.as_ref(), v2.as_ref()).into()
            }
            (Number::BigFloat(v1), Number::BigRatio(v2)) => {
                $imp::$method(v1.as_ref(), Promote::<BigDecimal>::promote(v2.as_ref())).into()
            }
            (Number::BigFloat(v1), Number::BigComplex(v2)) => {
                $imp::$method(Promote::<BigComplex>::promote(v1.as_ref()), v2.as_ref()).into()
            }
            (Number::BigRatio(v1), Number::Int(v2)) => {
                let n = $imp::$method(v1.as_ref(), &Promote::<BigRational>::promote(v2));
                if n.is_integer() {
                    n.numer().into()
                } else {
                    n.into()
                }
            }
            (Number::BigRatio(v1), Number::Float(v2)) => {
                if let Ok(v2) = BigDecimal::try_from(*v2) {
                    $imp::$method(Promote::<BigDecimal>::promote(v1.as_ref()), v2).into()
                } else {
                    $imp::$method(v1.as_ref().to_f64().unwrap_or(f64::NAN), v2).into()
                }
            }
            (Number::BigRatio(v1), Number::Ratio(v2)) => {
                let n = $imp::$method(v1.as_ref(), Promote::<BigRational>::promote(v2));
                if n.is_integer() {
                    n.numer().into()
                } else {
                    n.into()
                }
            }
            (Number::BigRatio(v1), Number::Complex(v2)) => {
                if let (Ok(im), Ok(re)) = (BigDecimal::try_from(v2.im), BigDecimal::try_from(v2.re))
                {
                    $imp::$method(
                        Promote::<BigComplex>::promote(v1.as_ref()),
                        Complex::new(re, im),
                    )
                    .into()
                } else {
                    $imp::$method(v1.as_ref().to_f64().unwrap_or(f64::NAN), v2).into()
                }
            }
            (Number::BigRatio(v1), Number::BigInt(v2)) => $imp::$method(
                Promote::<BigDecimal>::promote(v1.as_ref()),
                Into::<BigDecimal>::into(v2.as_ref().clone()),
            )
            .into(),
            (Number::BigRatio(v1), Number::BigFloat(v2)) => {
                $imp::$method(Promote::<BigDecimal>::promote(v1.as_ref()), v2.as_ref()).into()
            }
            (Number::BigRatio(v1), Number::BigRatio(v2)) => {
                let n = $imp::$method(v1.as_ref(), v2.as_ref());
                if n.is_integer() {
                    n.numer().into()
                } else {
                    n.into()
                }
            }
            (Number::BigRatio(v1), Number::BigComplex(v2)) => {
                $imp::$method(Promote::<BigComplex>::promote(v1.as_ref()), v2.as_ref()).into()
            }
            (Number::BigComplex(v1), Number::Int(v2)) => {
                $imp::$method(v1.as_ref(), Promote::<BigComplex>::promote(v2)).into()
            }
            (Number::BigComplex(v1), Number::Float(v2)) => {
                if let Ok(v2) = BigDecimal::try_from(*v2) {
                    $imp::$method(v1.as_ref(), Promote::<BigComplex>::promote(v2)).into()
                } else {
                    $imp::$method(
                        Complex64::new(
                            v1.re.to_f64().unwrap_or(f64::NAN),
                            v1.im.to_f64().unwrap_or(f64::NAN),
                        ),
                        v2,
                    )
                    .into()
                }
            }
            (Number::BigComplex(v1), Number::Ratio(v2)) => {
                $imp::$method(v1.as_ref(), Promote::<BigComplex>::promote(v2)).into()
            }
            (Number::BigComplex(v1), Number::Complex(v2)) => {
                if let (Ok(im), Ok(re)) = (BigDecimal::try_from(v2.im), BigDecimal::try_from(v2.re))
                {
                    $imp::$method(v1.as_ref(), Complex::new(re, im)).into()
                } else {
                    $imp::$method(
                        Complex64::new(
                            v1.re.to_f64().unwrap_or(f64::NAN),
                            v1.im.to_f64().unwrap_or(f64::NAN),
                        ),
                        v2,
                    )
                    .into()
                }
            }
            (Number::BigComplex(v1), Number::BigInt(v2)) => {
                $imp::$method(v1.as_ref(), Promote::<BigComplex>::promote(v2.as_ref())).into()
            }
            (Number::BigComplex(v1), Number::BigFloat(v2)) => {
                $imp::$method(v1.as_ref(), Promote::<BigComplex>::promote(v2.as_ref())).into()
            }
            (Number::BigComplex(v1), Number::BigRatio(v2)) => {
                $imp::$method(v1.as_ref(), Promote::<BigComplex>::promote(v2.as_ref())).into()
            }
            (Number::BigComplex(v1), Number::BigComplex(v2)) => {
                $imp::$method(v1.as_ref(), v2.as_ref()).into()
            }
        }
    };
}

macro_rules! define_binop {
    (
        $imp:ident, $method:ident, $checked_imp:ident, $checked_method:ident
    ) => {
        impl $imp for Number {
            type Output = Number;

            #[inline]
            fn $method(self, rhs: Number) -> Self::Output {
                define_binop_internal! {
                    $imp, $method, $checked_imp, $checked_method
                    (&self, &rhs)
                }
            }
        }
        impl<'a> $imp<&'a Number> for Number {
            type Output = Number;

            #[inline]
            fn $method(self, rhs: &'a Number) -> Self::Output {
                define_binop_internal! {
                    $imp, $method, $checked_imp, $checked_method
                    (&self, rhs)
                }
            }
        }
        impl<'a, 'b> $imp<&'a Number> for &'b Number {
            type Output = Number;

            #[inline]
            fn $method(self, rhs: &'a Number) -> Self::Output {
                define_binop_internal! {
                    $imp, $method, $checked_imp, $checked_method
                    (self, rhs)
                }
            }
        }
        impl<'a> $imp<Number> for &'a Number {
            type Output = Number;

            #[inline]
            fn $method(self, rhs: Number) -> Self::Output {
                define_binop_internal! {
                    $imp, $method, $checked_imp, $checked_method
                    (self, &rhs)
                }
            }
        }
    };
}

define_binop! { Add, add, CheckedAdd, checked_add }
define_binop! { Mul, mul, CheckedMul, checked_mul }
define_binop! { Sub, sub, CheckedSub, checked_sub }
define_binop! { Div, div, CheckedDiv, checked_div }
define_binop! { Rem, rem, PrivateCheckedRem, checked_rem }

trait PrivateCheckedRem: Sized + Rem<Self, Output = Self> {
    fn checked_rem(&self, v: &Self) -> Option<Self>;
}
impl PrivateCheckedRem for Rational64 {
    #[inline]
    fn checked_rem(&self, v: &Self) -> Option<Self> {
        if self.denom() == v.denom() {
            return Some(Rational64::new(
                CheckedRem::checked_rem(self.numer(), v.numer())?,
                *v.denom(),
            ));
        }
        let lcm = self.denom().lcm(v.denom());
        let lhs_numer =
            CheckedMul::checked_mul(self.numer(), &CheckedDiv::checked_div(&lcm, self.denom())?)?;
        let rhs_numer =
            CheckedMul::checked_mul(v.numer(), &CheckedDiv::checked_div(&lcm, v.denom())?)?;
        Some(Rational64::new(
            CheckedRem::checked_rem(&lhs_numer, &rhs_numer)?,
            lcm,
        ))
    }
}
impl PrivateCheckedRem for i64 {
    #[inline]
    fn checked_rem(&self, v: &Self) -> Option<Self> {
        CheckedRem::checked_rem(self, v)
    }
}

macro_rules! define_from {
    () => {};
    ($path:path => $(#[$meta:meta])* $type:ty, $($rest:tt)*) => {
        $(#[$meta])*
        impl From<$type> for Number {
            #[inline]
            fn from(value: $type) -> Self {
                $path(value.into())
            }
        }
        $(#[$meta])*
        impl From<&$type> for Number {
            #[inline]
            fn from(value: &$type) -> Self {
                $path(value.clone().into())
            }
        }
        define_from! { $($rest)* }
    };
    ($path:path => {$($(#[$meta:meta])* $type:ty),*}, $($rest:tt)*) => {
        $(
            $(#[$meta])*
            impl From<$type> for Number {
                #[inline]
                fn from(value: $type) -> Self {
                    $path(value.into())
                }
            }
            $(#[$meta])*
            impl From<&$type> for Number {
                #[inline]
                fn from(value: &$type) -> Self {
                    $path(value.clone().into())
                }
            }
        )*
        define_from! { $($rest)* }
    };
    (BOX $path:path => $(#[$meta:meta])* $type:ty, $($rest:tt)*) => {
        $(#[$meta])*
        impl From<$type> for Number {
            #[inline]
            fn from(value: $type) -> Self {
                $path(Box::new(value.into()))
            }
        }
        $(#[$meta])*
        impl From<&$type> for Number {
            #[inline]
            fn from(value: &$type) -> Self {
                $path(Box::new(value.clone().into()))
            }
        }
        define_from! { $($rest)* }
    };
    (BOX $path:path => {$($(#[$meta:meta])* $type:ty),*}, $($rest:tt)*) => {
        $(
            $(#[$meta])*
            impl From<$type> for Number {
                #[inline]
                fn from(value: $type) -> Self {
                    $path(Box::new(value.into()))
                }
            }
            $(#[$meta])*
            impl From<&$type> for Number {
                #[inline]
                fn from(value: &$type) -> Self {
                    $path(Box::new(value.clone().into()))
                }
            }
        )*
        define_from! { $($rest)* }
    };
}
define_from! {
    Number::Int => {bool,i8,u8,i16,u16,i32,u32,i64},
    Number::Float => {f32, f64},
    Number::Ratio => Rational64,
    Number::Complex => Complex64,
    BOX Number::BigInt => {u64, #[cfg(has_i128)] i128, #[cfg(has_u128)] u128, BigInt},
    Number::BigInt => Box<BigInt>,
    BOX Number::BigFloat => BigDecimal,
    Number::BigFloat => Box<BigDecimal>,
    BOX Number::BigRatio => BigRational,
    Number::BigRatio => Box<BigRational>,
    BOX Number::BigComplex => BigComplex,
    Number::BigComplex => Box<BigComplex>,
}
pub trait Promote<T>: Sized {
    /// Performs the conversion.
    fn promote(self) -> T;
}
pub trait PromoteFrom<T>: Sized {
    /// Performs the conversion.
    fn promote_from(_: T) -> Self;
}

impl<T, U> Promote<U> for T
where
    U: PromoteFrom<T>,
{
    #[inline]
    fn promote(self) -> U {
        U::promote_from(self)
    }
}

macro_rules! define_promote {
    (for $for:ty where $var:ident: $type:ty => $expr:expr ) => {
        impl PromoteFrom<$type> for $for {
            #[inline]
            fn promote_from($var: $type) -> Self {
                $expr
            }
        }
        impl PromoteFrom<&$type> for $for {
            #[inline]
            fn promote_from($var: &$type) -> Self {
                $expr
            }
        }
        impl PromoteFrom<Box<$type>> for $for {
            #[inline]
            fn promote_from($var: Box<$type>) -> Self {
                let $var = $var.as_ref();
                $expr
            }
        }
        impl PromoteFrom<&Box<$type>> for $for {
            #[inline]
            fn promote_from($var: &Box<$type>) -> Self {
                let $var = $var.as_ref();
                $expr
            }
        }
    };
}

define_promote!(for Complex64 where value: Rational64  => Complex::new(*value.numer() as f64 / *value.denom() as f64,f64::zero()));
define_promote!(for BigRational where value: Rational64  => BigRational::new(value.numer().clone().into(), value.denom().clone().into()));
define_promote!(for BigRational where value: i64  => BigRational::from_integer(value.clone().into()));
define_promote!(for BigRational where value: BigInt  => Into::<BigRational>::into(value.clone()));
define_promote!(for Rational64 where value: i64  => Rational64::from_integer(value.clone().into()));
define_promote!(for BigInt where value: i64  => Into::<BigInt>::into(value.clone()));
define_promote!(for Complex64 where value: f64  => Into::<Complex64>::into(value.clone()));
define_promote!(for f64 where value: Rational64  => *value.numer() as f64 / *value.denom()  as f64);
define_promote!(for BigComplex where value: i64  => Complex::new(Into::<BigDecimal>::into(value.clone()),BigDecimal::zero()));
define_promote!(for BigComplex where value: BigInt  => Complex::new(Into::<BigDecimal>::into(value.clone()),BigDecimal::zero()));
define_promote!(for BigComplex where value: BigDecimal  => Complex::new(value.clone(),BigDecimal::zero()));
define_promote!(for BigComplex where value: BigRational  => Complex::new(Into::<BigDecimal>::into(value.numer().clone()) / Into::<BigDecimal>::into(value.denom().clone()),BigDecimal::zero()));
define_promote!(for BigComplex where value: Rational64  => Complex::new(Into::<BigDecimal>::into(*value.numer()) / Into::<BigDecimal>::into(*value.denom()),BigDecimal::zero()));
define_promote!(for BigDecimal where value: BigRational  => Into::<BigDecimal>::into(value.numer().clone()) / Into::<BigDecimal>::into(value.denom().clone()));
define_promote!(for BigDecimal where value: Rational64  => Into::<BigDecimal>::into(*value.numer()) / Into::<BigDecimal>::into(*value.denom()));
define_promote!(for BigDecimal where value: i64  => Into::<BigDecimal>::into(value.clone()));
define_promote!(for BigDecimal where value: BigInt  => Into::<BigDecimal>::into(value.clone()));

macro_rules! impl_to_primitive {
    () => {
        impl_to_primitive!(
            @build
            to_i64      i64,
            to_u64      u64,
            to_isize    isize,
            to_i8       i8,
            to_i16      i16,
            to_i32      i32,
            to_usize    usize,
            to_u8       u8,
            to_u16      u16,
            to_u32      u32,
            to_f32      f32,
            to_f64      f64
        );
    };
    (@build $($method:ident  $type:ty),*) => {
        impl ToPrimitive for Number {
            $(
                fn $method(&self) -> Option<$type> {
                    impl_to_primitive!(@match self $method)
                }
            )*
        }
    };
    (@match $self:ident $method:ident) => {
        match $self {
            Number::Int(v) => ToPrimitive::$method(v),
            Number::Float(v) => ToPrimitive::$method(v),
            Number::Ratio(v) => ToPrimitive::$method(v),
            Number::Complex(v) => ToPrimitive::$method(v),
            Number::BigInt(v) => ToPrimitive::$method(v.as_ref()),
            Number::BigFloat(v) => ToPrimitive::$method(v.as_ref()),
            Number::BigRatio(v) => ToPrimitive::$method(v.as_ref()),
            Number::BigComplex(v) => ToPrimitive::$method(v.as_ref()),
        }
    };
}
impl_to_primitive!();

#[cfg(test)]
mod test {
    use super::*;
    macro_rules! assert_matches {
        ($expression:expr, $($pattern:tt)+) => {
            match $expression {
                $($pattern)+ => (),
                ref e => panic!("assertion failed: `{:?}` does not match `{}`", e, stringify!($($pattern)+)),
            }
        }
    }

    #[test]
    fn number_into() {
        macro_rules! test_into {
            ($($($expr:expr),* => $path:path),*) => {
                $($(assert_matches!(Into::<Number>::into($expr), $path(_));)*)*
            };
        }
        test_into! (
            true as bool, 0 as i8, 0 as u8, 0 as i16, 0 as u16,
            0 as i32, 0 as u32, 0 as i64 => Number::Int,
            0.0 as f32, 0.0 as f64 => Number::Float,
            Rational64::zero() => Number::Ratio,
            Complex64::zero() => Number::Complex,
            0 as u64, BigInt::zero(), Box::new(BigInt::zero()) => Number::BigInt,
            BigDecimal::zero(), Box::new(BigDecimal::zero()) => Number::BigFloat,
            BigRational::zero(), Box::new(BigRational::zero()) => Number::BigRatio,
            BigComplex::zero(), Box::new(BigComplex::zero()) => Number::BigComplex
        );
        #[cfg(has_i128)]
        test_into! (0 as i128 => Number::BigInt);
        #[cfg(has_u128)]
        test_into! (0 as u128 => Number::BigInt);
    }

    #[test]
    fn number_promotions() {
        let si: &Number = &1.into();
        let bi: &Number = &(1 as u64).into();
        let sf: &Number = &1.0.into();
        let bf: &Number = &BigDecimal::one().into();
        let sr: &Number = &Rational64::new(1, 3).into();
        let br: &Number = &BigRational::new(1.into(), 3.into()).into();
        let sc: &Number = &Complex64::new(0.0, 1.0).into();
        let bc: &Number = &BigComplex::new(0.into(), 1.into()).into();
        macro_rules! test_type_combination {
            ($v1:ident {} $v2:ident => $type:path) => {};
            ($v1:ident {+ $($rest:tt)*} $v2:ident => $type:path) => {
                assert_matches!($v1 + $v2, $type(_));
                test_type_combination!($v1 {$($rest)*} $v2 => $type);
            };
            ($v1:ident {- $($rest:tt)*} $v2:ident => $type:path) => {
                assert_matches!($v1 - $v2, $type(_));
                test_type_combination!($v1 {$($rest)*} $v2 => $type);
            };
            ($v1:ident {/ $($rest:tt)*} $v2:ident => $type:path) => {
                assert_matches!($v1 / $v2, $type(_));
                test_type_combination!($v1 {$($rest)*} $v2 => $type);
            };
            ($v1:ident {* $($rest:tt)*} $v2:ident => $type:path) => {
                assert_matches!($v1 * $v2, $type(_));
                test_type_combination!($v1 {$($rest)*} $v2 => $type);
            };
            ($v1:ident {% $($rest:tt)*} $v2:ident => $type:path) => {
                assert_matches!($v1 % $v2, $type(_));
                test_type_combination!($v1 {$($rest)*} $v2 => $type);
            };
        }
        // Check types
        assert_matches!(si, Number::Int(_));
        assert_matches!(sf, Number::Float(_));
        assert_matches!(sr, Number::Ratio(_));
        assert_matches!(sc, Number::Complex(_));
        assert_matches!(bi, Number::BigInt(_));
        assert_matches!(br, Number::BigRatio(_));
        assert_matches!(bf, Number::BigFloat(_));
        assert_matches!(bc, Number::BigComplex(_));
        // No-promotion
        test_type_combination!(si {+ * / - %} si => Number::Int);
        test_type_combination!(sf {+ * / - %} sf => Number::Float);
        test_type_combination!(sr {+ *} sr => Number::Ratio);
        test_type_combination!(sc {+ * / - %} sc => Number::Complex);
        test_type_combination!(bi {+ * / - %} bi => Number::BigInt);
        test_type_combination!(br {+ *} br => Number::BigRatio);
        test_type_combination!(bf {+ * / - %} bf => Number::BigFloat);
        test_type_combination!(bc {+ * / - %} bc => Number::BigComplex);

        // Int promotions
        test_type_combination!(sr {/ - %} sr => Number::Int);
        assert_matches!(sr + sr + sr, Number::Int(_));
        assert_matches!((si + si + si) * sr, Number::Int(_));

        // Float promotions
        test_type_combination!(sr {+ * / - %} sf => Number::Float);
        test_type_combination!(si {+ * / - %} sf => Number::Float);

        // Ratio promotions

        // Complex promotions
        test_type_combination!(si {+ * / - %} sc => Number::Complex);
        test_type_combination!(sf {+ * / - %} sc => Number::Complex);
        test_type_combination!(sr {+ * / - %} sc => Number::Complex);

        // BigInt promotions
        let si_max: &Number = &i64::MAX.into();
        assert_matches!(si_max, Number::Int(_));
        assert_matches!(si + si_max, Number::BigInt(_));
        assert_matches!((si + si) * si_max, Number::BigInt(_));
        test_type_combination!(br {/ - %} br => Number::BigInt);
        assert_matches!(br + br + br, Number::BigInt(_));
        assert_matches!((si + si + si) * br, Number::BigInt(_));
        assert_matches!((bi + bi + bi) * sr, Number::BigInt(_));
        assert_matches!((bi + bi + bi) * br, Number::BigInt(_));
        assert_matches!((bi + bi + bi) * si, Number::BigInt(_));
        test_type_combination!(si {+ * / - %} bi => Number::BigInt);

        // BigRatio promotions
        test_type_combination!(si_max {+} sr => Number::BigRatio);
        test_type_combination!(si {+ * -} br => Number::BigRatio);
        test_type_combination!(sr {+ *} br => Number::BigRatio);

        // BigFloat promotions
        test_type_combination!(sf {+ * / - %} bf => Number::BigFloat);
        test_type_combination!(sr {+ * / - %} bf => Number::BigFloat);
        test_type_combination!(br {+ * / - %} bf => Number::BigFloat);
        test_type_combination!(sf {+ * / - %} br => Number::BigFloat);
        test_type_combination!(si {+ * / - %} bf => Number::BigFloat);
        test_type_combination!(bi {+ * / - %} bf => Number::BigFloat);

        // BigComplex promotions
        test_type_combination!(si {+ * / - %} bc => Number::BigComplex);
        test_type_combination!(sf {+ * / - %} bc => Number::BigComplex);
        test_type_combination!(sr {+ * / - %} bc => Number::BigComplex);
        test_type_combination!(sc {+ * / - %} bc => Number::BigComplex);
        test_type_combination!(bi {+ * / - %} bc => Number::BigComplex);
        test_type_combination!(br {+ * / - %} bc => Number::BigComplex);
        test_type_combination!(bf {+ * / - %} bc => Number::BigComplex);

        // Float Specials
        macro_rules! all_promote {
            (@parse {$e1:ident},{$($e2:ident),*} => $path:path) => {
                $(test_type_combination!($e1 {+ * / - %} $e2 => $path);)*
            };
            ({},{$($_:tt)*} => $__:path) => {};
            ({$ident:ident},{$($tree:tt)*} => $path:path) => {
                all_promote!(@parse {$ident},{$($tree)*} => $path);
            };
            ({$ident:ident,$($rest:tt)*},{$($tree:tt)*} => $path:path) => {
                all_promote!(@parse {$ident},{$($tree)*} => $path);
                all_promote!({$($rest)*},{$($tree)*} => $path);
            };
        }
        let sf_nan: &Number = &f64::NAN.into();
        let sf_neg_inf: &Number = &f64::NEG_INFINITY.into();
        let sf_pos_inf: &Number = &f64::INFINITY.into();
        let sc_nan: &Number = &Into::<Complex64>::into(f64::NAN).into();
        let sc_neg_inf: &Number = &Into::<Complex64>::into(f64::NEG_INFINITY).into();
        let sc_pos_inf: &Number = &Into::<Complex64>::into(f64::INFINITY).into();

        all_promote!({sf_nan,sf_neg_inf,sf_pos_inf},{si,sf,sr,bi,bf,br,bf} => Number::Float);
        all_promote!({sc_nan,sc_neg_inf,sc_pos_inf},{si,sf,sr,sc,bi,bf,br,bf} => Number::Complex);
    }

    #[test]
    fn number_float_specials() {
        macro_rules! test_cross_combination {
            (@parse {+ $e1:expr},{$($e2:expr),*}  @ $path:path{$var:ident} => $res:expr) => {
                $(if let $path($var) = $e1 + $e2 {
                    assert!($res, "Failed: {:?} + {:?} [{:?}] predicate false {:?}", $e1, $e2, $e1 + $e2, stringify!($res));
                } else {
                    panic!("Failed: {:?} + {:?} [{:?}] is not {:?}", $e1, $e2, $e1 + $e2, stringify!($path));
                })*
            };
            (@parse {- $e1:expr},{$($e2:expr),*}  @ $path:path{$var:ident} => $res:expr) => {
                $(if let $path($var) = $e1 - $e2 {
                    assert!($res, "Failed: {:?} - {:?} [{:?}] predicate false {:?}", $e1, $e2, $e1 - $e2, stringify!($res));
                } else {
                    panic!("Failed: {:?} - {:?} [{:?}] is not {:?}", $e1, $e2, $e1 - $e2, stringify!($path));
                })*
            };
            (@parse {* $e1:expr},{$($e2:expr),*}  @ $path:path{$var:ident} => $res:expr) => {
                $(if let $path($var) = $e1 * $e2 {
                    assert!($res, "Failed: {:?} * {:?} [{:?}] predicate false {:?}", $e1, $e2, $e1 * $e2, stringify!($res));
                } else {
                    panic!("Failed: {:?} * {:?} [{:?}] is not {:?}", $e1, $e2, $e1 * $e2, stringify!($path));
                })*
            };
            (@parse {/ $e1:expr},{$($e2:expr),*}  @ $path:path{$var:ident} => $res:expr) => {
                $(if let $path($var) = $e1 / $e2 {
                    assert!($res, "Failed: {:?} / {:?} [{:?}] predicate false {:?}", $e1, $e2, $e1 / $e2, stringify!($res));
                } else {
                    panic!("Failed: {:?} / {:?} [{:?}] is not {:?}", $e1, $e2, $e1 / $e2, stringify!($path));
                })*
            };
            (@parse {% $e1:expr},{$($e2:expr),*}  @ $path:path{$var:ident} => $res:expr) => {
                $(if let $path($var) = $e1 % $e2 {
                    assert!($res, "Failed: {:?} % {:?} [{:?}] predicate false {:?}", $e1, $e2, $e1 % $e2, stringify!($res));
                } else {
                    panic!("Failed: {:?} % {:?} [{:?}] is not {:?}", $e1, $e2, $e1 % $e2, stringify!($path));
                })*
            };
            ({}, {}, {$($_:tt)*} @ $__:path{$___:ident} => $____:expr) => {};
            ({$($_:tt)*}, {}, {$($__:tt)*} @ $___:path{$____:ident} => $_____:expr) => {};
            ({}, {$($_:tt)*}, {$($__:tt)*} @ $___:path{$____:ident} => $_____:expr) => {};
            ({$e1:ident}, {+ $($tree_op:tt)*}, {$($tree_e2:tt)*} @ $path:path{$var:ident} => $res:expr) => {
                test_cross_combination!(@parse {+ $e1},{$($tree_e2)*} @ $path{$var} => $res);
                test_cross_combination!({$e1},{$($tree_op)*},{$($tree_e2)*} @ $path{$var} => $res);

            };
            ({$e1:ident}, {- $($tree_op:tt)*}, {$($tree_e2:tt)*} @ $path:path{$var:ident} => $res:expr) => {
                test_cross_combination!(@parse {- $e1},{$($tree_e2)*} @ $path{$var} => $res);
                test_cross_combination!({$e1},{$($tree_op)*},{$($tree_e2)*} @ $path{$var} => $res);

            };
            ({$e1:ident}, {* $($tree_op:tt)*}, {$($tree_e2:tt)*} @ $path:path{$var:ident} => $res:expr) => {
                test_cross_combination!(@parse {* $e1},{$($tree_e2)*} @ $path{$var} => $res);
                test_cross_combination!({$e1},{$($tree_op)*},{$($tree_e2)*} @ $path{$var} => $res);

            };
            ({$e1:ident}, {/ $($tree_op:tt)*}, {$($tree_e2:tt)*} @ $path:path{$var:ident} => $res:expr) => {
                test_cross_combination!(@parse {/ $e1},{$($tree_e2)*} @ $path{$var} => $res);
                test_cross_combination!({$e1},{$($tree_op)*},{$($tree_e2)*} @ $path{$var} => $res);

            };
            ({$e1:ident}, {% $($tree_op:tt)*}, {$($tree_e2:tt)*} @ $path:path{$var:ident} => $res:expr) => {
                test_cross_combination!(@parse {% $e1},{$($tree_e2)*} @ $path{$var} => $res);
                test_cross_combination!({$e1},{$($tree_op)*},{$($tree_e2)*} @ $path{$var} => $res);

            };
            ({$e1:ident,$($tree_e1:tt)*}, {$($tree_op:tt)*}, {$($tree_e2:tt)*} @ $path:path{$var:ident} => $res:expr) => {
                test_cross_combination!({$e1},{$($tree_op)*},{$($tree_e2)*} @ $path{$var} => $res);
                test_cross_combination!({$($tree_e1)*},{$($tree_op)*},{$($tree_e2)*} @ $path{$var} => $res);

            };
        }
        macro_rules! test_cross_combination_sym {
            ({$($tree_e1:tt)*}, {$($tree_op:tt)*}, {$($tree_e2:tt)*} @ $path:path{$var:ident} => $res:expr) => {
                test_cross_combination!({$($tree_e1)*},{$($tree_op)*},{$($tree_e2)*} @ $path{$var} => $res);
                test_cross_combination!({$($tree_e2)*},{$($tree_op)*},{$($tree_e1)*} @ $path{$var} => $res);

            };
        }
        let si_pos: &Number = &1.into();
        let bi_pos: &Number = &BigInt::one().into();
        let sf_pos: &Number = &1.0.into();
        let bf_pos: &Number = &BigDecimal::one().into();
        let sr_pos: &Number = &Rational64::one().into();
        let br_pos: &Number = &BigRational::one().into();
        let sc_pos: &Number = &Complex64::one().into();
        let bc_pos: &Number = &BigComplex::one().into();

        let si_neg: &Number = &(-1).into();
        let bi_neg: &Number = &(-BigInt::one()).into();
        let sf_neg: &Number = &(-1.0).into();
        let bf_neg: &Number = &(-BigDecimal::one()).into();
        let sr_neg: &Number = &(-Rational64::one()).into();
        let br_neg: &Number = &(-BigRational::one()).into();
        let sc_neg: &Number = &(-Complex64::one()).into();
        let bc_neg: &Number = &(-BigComplex::one()).into();

        let sf_nan: &Number = &f64::NAN.into();
        let sf_neg_inf: &Number = &f64::NEG_INFINITY.into();
        let sf_pos_inf: &Number = &f64::INFINITY.into();
        let sc_0_nan: &Number = &Complex64::new(0.0, f64::NAN).into();
        let sc_nan_0: &Number = &Into::<Complex64>::into(f64::NAN).into();
        let sc_nan_nan: &Number = &Complex64::new(f64::NAN, f64::NAN).into();
        let sc_neg_inf: &Number = &Into::<Complex64>::into(f64::NEG_INFINITY).into();
        let sc_pos_inf: &Number = &Into::<Complex64>::into(f64::INFINITY).into();

        test_cross_combination_sym!(
            {sf_nan},{+ - * / %},
            {
                si_pos,bi_pos,sf_pos,bf_pos,sr_pos,br_pos,
                si_neg,bi_neg,sf_neg,bf_neg,sr_neg,br_neg,
                sf_nan,sf_neg_inf,sf_pos_inf
            }
            @ Number::Float{res} => res.is_nan()
        );

        test_cross_combination_sym!(
            {sf_nan},{+ - * / %},
            {
                sc_pos,bc_pos,sc_neg,bc_neg,
                sc_0_nan,sc_nan_0,sc_nan_nan,
                sc_neg_inf,sc_pos_inf
            }
            @ Number::Complex{res} => res.is_nan()
        );

        test_cross_combination_sym!(
            {sc_0_nan,sc_nan_0,sc_nan_nan},{+ - * / %},
            {
                si_pos,bi_pos,sf_pos,bf_pos,sr_pos,br_pos,sc_pos,bc_pos,
                si_neg,bi_neg,sf_neg,bf_neg,sr_neg,br_neg,sc_neg,bc_neg,
                sf_nan,sf_neg_inf,sf_pos_inf,
                sc_0_nan,sc_nan_0,sc_nan_nan,sc_neg_inf,sc_pos_inf
            }
            @ Number::Complex{res} => res.is_nan()
        );
        test_cross_combination!(
            {sf_pos_inf,sf_neg_inf},{%},
            {
                si_pos,bi_pos,sf_pos,bf_pos,sr_pos,br_pos,
                si_neg,bi_neg,sf_neg,bf_neg,sr_neg,br_neg,
                sf_nan,sf_neg_inf,sf_pos_inf
            }
            @ Number::Float{res} => res.is_nan()
        );
        test_cross_combination!(
            {sc_neg_inf,sc_neg_inf},{%},
            {
                si_pos,bi_pos,sf_pos,bf_pos,sr_pos,br_pos,
                si_neg,bi_neg,sf_neg,bf_neg,sr_neg,br_neg,
                sf_nan,sf_neg_inf,sf_pos_inf
            }
            @ Number::Complex{res} => res.is_nan()
        );
    }

    #[test]
    fn number_eq() {
        macro_rules! cross_assert_eq {
            (@parse {$e1:expr},{$($e2:expr),*}) => {
                $(assert_eq!($e1,$e2, "Failed: {:?} == {:?}", $e1, $e2);)*
            };
            ({},{$($_:tt)*}) => {};
            ({$expr:expr},{$($tree:tt)*}) => {
                cross_assert_eq!(@parse {$expr},{$($tree)*});
            };
            ({$expr:expr,$($rest:tt)*},{$($tree:tt)*}) => {
                cross_assert_eq!(@parse {$expr},{$($tree)*});
                cross_assert_eq!({$($rest)*},{$($tree)*});
            };
            ({$($tree:tt)*}) => {
                cross_assert_eq!({$($tree)*},{$($tree)*});
            };
        }
        macro_rules! cross_assert_ne {
            (@parse {$e1:expr},{$($e2:expr),*}) => {
                $(assert_ne!($e1,$e2, "Failed: {:?} != {:?}", $e1, $e2);)*
            };
            ({},{$($_:tt)*}) => {};
            ({$expr:expr},{$($tree:tt)*}) => {
                cross_assert_ne!(@parse {$expr},{$($tree)*});
            };
            ({$expr:expr,$($rest:tt)*},{$($tree:tt)*}) => {
                cross_assert_ne!(@parse {$expr},{$($tree)*});
                cross_assert_ne!({$($rest)*},{$($tree)*});
            };
        }
        let si0: &Number = &0.into();
        let si1: &Number = &1.into();
        let bi0: &Number = &(0 as u64).into();
        let bi1: &Number = &(1 as u64).into();
        let sf0: &Number = &0.0.into();
        let sf1: &Number = &1.0.into();
        let bf0: &Number = &BigDecimal::zero().into();
        let bf1: &Number = &BigDecimal::one().into();
        let sr0: &Number = &Rational64::zero().into();
        let sr1: &Number = &Rational64::one().into();
        let br0: &Number = &BigRational::zero().into();
        let br1: &Number = &BigRational::one().into();
        let sc0: &Number = &Complex64::zero().into();
        let sc1: &Number = &Complex64::one().into();
        let bc0: &Number = &BigComplex::zero().into();
        let bc1: &Number = &BigComplex::one().into();

        // Integer based
        cross_assert_eq! ({si0,bi0,bf0,sr0,br0,bc0});
        cross_assert_eq! ({si1,bi1,bf1,sr1,br1,bc1});
        cross_assert_ne! (
            {si0,bi0,bf0,sr0,br0,bc0,sf0,sc0},
            {si1,bi1,bf1,sr1,br1,bc1,sf1,sc1}
        );

        // Float based
        cross_assert_eq! ({sf0,sc0});
        cross_assert_eq! ({sf1,sc1});
        cross_assert_ne! (
            {sf0,sc0,si1,bi1,bf1,sr1,br1,bc1},
            {sf1,sc1,si0,bi0,bf0,sr0,br0,bc0}
        );

        // Float NAN
        let sf_nan: &Number = &f64::NAN.into();
        let sc_nan: &Number = &Into::<Complex64>::into(f64::NAN).into();
        cross_assert_ne! (
            {sf_nan,sc_nan},
            {sf_nan,sc_nan,si0,bi0,bf0,sr0,br0,bc0,sf0,sc0,si1,bi1,bf1,sr1,br1,bc1,sf1,sc1}
        );
    }
}
