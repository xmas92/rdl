use std::{any::Any, hash::Hash, mem, sync::Arc};

macro_rules! replace_ty {
    ($_t:tt $sub:ty) => {
        $sub
    };
}
macro_rules! define_tuple_types {
    () => {};
    ($first:ident, $($rest:ident,)*) => {
        type $first = (RustPrimitive, $(replace_ty!($rest RustPrimitive),)*);
        define_tuple_types! {$($rest,)*}
    };
}

macro_rules! define_primitive_enum_internal {
    (
        @parse
        {$($tree_output:tt)*};
    ) => {
        #[derive(Debug, Clone)]
        #[allow(non_camel_case_types)]
        pub enum RustPrimitive {
            $($tree_output)*
        }
    };
    (
        @parse {$($tree_output:tt)*};
        $(#[$meta:meta])*
        box $tt:tt,
        $($tail:tt)*
    ) => {
        define_primitive_enum_internal! {
            @parse
            { $($tree_output)* };
            $(#[$meta])*
            $tt => Box<$tt>,
            $($tail)*
        }
    };
    (
        @parse {$($tree_output:tt)*};
        $(#[$meta:meta])*
        $name:ident => $type:ty ,
        $($tail:tt)*
    ) => {
        define_primitive_enum_internal! {
            @parse
            {
                $($tree_output)*
                $(#[$meta])*
                $name($type),
            };
            $($tail)*
        }
    };
    (
        @parse {$($tree_output:tt)*};
        $tt:tt,
        $($tail:tt)*
    ) => {
        define_primitive_enum_internal! {
            @parse
            {$($tree_output)*};
            $tt => $tt,
            $($tail)*
        }
    };
}
macro_rules! define_primitive_enum {
    ($($cmd:tt)*) => {
        define_primitive_enum_internal! {
            @parse {};
            $($cmd)*
        }
    };
}

macro_rules! define_from_types {
    () => {};
    (
        $(#[$meta:meta])*
        box $tt:tt,
        $($tail:tt)*
    ) => {
        define_from_types! {
            $(#[$meta])*
            $tt => box $tt,
            $($tail)*
        }
    };
    (
        $tt:tt,
        $($tail:tt)*
    ) => {
        define_from_types! {
            $tt => $tt,
            $($tail)*
        }
    };
    (
        $(#[$meta:meta])*
        $name:ident => $type:ty,
        $($tail:tt)*
    ) => {
        $(#[$meta])*
        impl From<$type> for RustPrimitive {
            fn from(value: $type) -> RustPrimitive {
                RustPrimitive::$name(value)
            }
        }
        define_from_types! {$($tail)*}
    };
    (
        $(#[$meta:meta])*
        $name:ident => box $type:ty,
        $($tail:tt)*
    ) => {
        $(#[$meta])*
        impl From<$type> for RustPrimitive {
            fn from(value: $type) -> RustPrimitive {
                RustPrimitive::$name(Box::new(value))
            }
        }
        define_from_types! {$($tail)*}
    };
}

macro_rules! define_from_tuple_types {
    () => {};
    ($name:ident => $type:ty, $($names:ident,)*) => {
        #[allow(non_snake_case)]
        impl<$($names: Into<RustPrimitive>,)*> From<($($names,)*)> for RustPrimitive {
            fn from(value: ($($names,)*)) -> RustPrimitive {
                let ($($names,)*) = value;
                RustPrimitive::$name(Box::new(($($names.into(),)*)))
            }
        }
    };
    ($first:tt, $($rest:tt,)*) => {
        define_from_tuple_types! {$first => $first, $first, $($rest,)*}
        define_from_tuple_types! {$($rest,)*}
    };
}

define_tuple_types! { T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, }
define_from_tuple_types! { T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, }

// TODO? Maybe implement an unsafe primitive which can contain
// something which is not Send + Sync as well as contain a non
// static reference. However this will break everything.

define_primitive_enum! {
    bool,char,f32,f64,
    i8,i16,i32,i64,isize,
    u8,u16,u32,u64,usize,
    #[cfg(has_i128)]
    box i128,
    #[cfg(has_u128)]
    box u128,
    box T0, box T1, box T2, box T3, box T4, box T5,
    box T6, box T7, box T8, box T9, box T10, box T11,
    Any => Arc<dyn Any + Send + Sync>,
    AnyRef => Arc<&'static (dyn Any + Send + Sync)>,
}
define_from_types! {
    bool,char,f32,f64,
    i8,i16,i32,i64,isize,
    u8,u16,u32,u64,usize,
    #[cfg(has_i128)]
    box i128,
    #[cfg(has_u128)]
    box u128,
    Any => Arc<dyn Any + Send + Sync>,
    AnyRef => Arc<&'static (dyn Any + Send + Sync)>,
}

impl From<&'static (dyn Any + Send + Sync)> for RustPrimitive {
    fn from(value: &'static (dyn Any + Send + Sync)) -> Self {
        RustPrimitive::AnyRef(Arc::new(value))
    }
}
macro_rules! match_hash_custom {
    (
        @parse {$($tree_output:tt)*};
        $self:ident, $state:ident,
        custom: {}
    ) => {
        match $self {
            $($tree_output)*
        }
    };
    (
        @parse {$($tree_output:tt)*};
        $self:ident, $state:ident,
        custom: {
            $(#[$meta:meta])*
            $name:ident: $v:ident => $arm:expr,
            $($custom:tt)*
        }
        $($tail:tt)*
    ) => {
        match_hash_custom!(
            @parse {
                $(#[$meta])*
                RustPrimitive::$name($v) => $arm,
                $($tree_output)*
            };
            $self, $state,
            custom: {
                $($custom)*
            }
            $($tail)*
        )
    };
}

macro_rules! match_hash_by_ref {
    (
        @parse {$($tree_output:tt)*};
        $self:ident, $state:ident,
        by_ref: {}
        $($tail:tt)*
    ) => {
        match_hash_custom! {
            @parse {$($tree_output)*};
            $self, $state, $($tail)*
        }
    };
    (
        @parse {$($tree_output:tt)*};
        $self:ident, $state:ident,
        by_ref: {
            $(#[$meta:meta])*
            $name:ident,
            $($by_ref:tt)*
        }
        $($tail:tt)*
    ) => {
        match_hash_by_ref!(
            @parse {
                $(#[$meta])*
                RustPrimitive::$name(v) => Arc::as_ptr(v).hash($state),
                $($tree_output)*
            };
            $self, $state,
            by_ref: {
                $($by_ref)*
            }
            $($tail)*
        )
    };
}

macro_rules! match_hash_default {
    (
        @parse {$($tree_output:tt)*};
        $self:ident, $state:ident,
        default: {}
        $($tail:tt)*
    ) => {
        match_hash_by_ref! {
            @parse {$($tree_output)*};
            $self, $state, $($tail)*
        }
    };
    (
        @parse {$($tree_output:tt)*};
        $self:ident, $state:ident,
        default: {
            $(#[$meta:meta])*
            box $name:ident,
            $($default:tt)*
        }
        $($tail:tt)*
    ) => {
        match_hash_default!(
            @parse {$($tree_output)*};
            $self, $state,
            default: {
                $(#[$meta])*
                $name,
                $($default)*
            }
            $($tail)*
        )
    };
    (
        @parse {$($tree_output:tt)*};
        $self:ident, $state:ident,
        default: {
            $(#[$meta:meta])*
            $name:ident,
            $($default:tt)*
        }
        $($tail:tt)*
    ) => {
        match_hash_default!(
            @parse {
                $(#[$meta])*
                RustPrimitive::$name(v) => v.hash($state),
                $($tree_output)*
            };
            $self, $state,
            default: {
                $($default)*
            }
            $($tail)*
        )
    };
}

macro_rules! match_hash {
    (
        $self:ident, $state:ident, $($tail:tt)*
    ) => {
        match_hash_default! {
            @parse { };
            $self, $state, $($tail)*
        }
    };
}

impl Hash for RustPrimitive {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match_hash!(
            self, state,
            default: {
                bool,char,
                i8,i16,i32,i64,isize,
                u8,u16,u32,u64,usize,
                #[cfg(has_i128)]
                box i128,
                #[cfg(has_u128)]
                box u128,
                box T0, box T1, box T2, box T3, box T4, box T5,
                box T6, box T7, box T8, box T9, box T10, box T11,
            }
            by_ref: {
                Any,
                AnyRef,
            }
            custom: {
                f64: f => unsafe {
                    mem::transmute::<f64, u64>(*f).hash(state)
                },
                f32: f => unsafe {
                    mem::transmute::<f32, u32>(*f).hash(state)
                },
            }
        );
    }
}

macro_rules! match_partial_eq_by_ref {
    (
        @parse {$($tree_output:tt)*};
        $self:ident, $other:ident,
        by_ref: {}
    ) => {
        match ($self, $other) {
            $($tree_output)*
            (_, _) => false,
        }
    };
    (
        @parse {$($tree_output:tt)*};
        $self:ident, $other:ident,
        by_ref: {
            $(#[$meta:meta])*
            $name:ident,
            $($by_ref:tt)*
        }
        $($tail:tt)*
    ) => {
        match_partial_eq_by_ref!(
            @parse {
                $(#[$meta])*
                (RustPrimitive::$name(v1), RustPrimitive::$name(v2)) => Arc::as_ptr(v1) == Arc::as_ptr(v2),
                $($tree_output)*
            };
            $self, $other,
            by_ref: {
                $($by_ref)*
            }
            $($tail)*
        )
    };
}

macro_rules! match_partial_eq_default {
    (
        @parse {$($tree_output:tt)*};
        $self:ident, $other:ident,
        default: {}
        $($tail:tt)*
    ) => {
        match_partial_eq_by_ref! {
            @parse {$($tree_output)*};
            $self, $other, $($tail)*
        }
    };
    (
        @parse {$($tree_output:tt)*};
        $self:ident, $other:ident,
        default: {
            $(#[$meta:meta])*
            box $name:ident,
            $($default:tt)*
        }
        $($tail:tt)*
    ) => {
        match_partial_eq_default!(
            @parse {$($tree_output)*};
            $self, $other,
            default: {
                $(#[$meta])*
                $name,
                $($default)*
            }
            $($tail)*
        )
    };
    (
        @parse {$($tree_output:tt)*};
        $self:ident, $other:ident,
        default: {
            $(#[$meta:meta])*
            $name:ident,
            $($default:tt)*
        }
        $($tail:tt)*
    ) => {
        match_partial_eq_default!(
            @parse {
                $(#[$meta])*
                (RustPrimitive::$name(v1), RustPrimitive::$name(v2)) => v1 == v2,
                $($tree_output)*
            };
            $self, $other,
            default: {
                $($default)*
            }
            $($tail)*
        )
    };
}

macro_rules! match_partial_eq {
    (
        $self:ident, $other:ident, $($tail:tt)*
    ) => {
        match_partial_eq_default! {
            @parse { };
            $self, $other, $($tail)*
        }
    };
}
impl PartialEq for RustPrimitive {
    fn eq(&self, other: &Self) -> bool {
        match_partial_eq!(
            self, other,
            default: {
                bool,char,f32,f64,
                i8,i16,i32,i64,isize,
                u8,u16,u32,u64,usize,
                #[cfg(has_i128)]
                box i128,
                #[cfg(has_u128)]
                box u128,
                box T0, box T1, box T2, box T3, box T4, box T5,
                box T6, box T7, box T8, box T9, box T10, box T11,
            }
            by_ref: {
                Any,
                AnyRef,
            }
        )
    }
}

impl Eq for RustPrimitive {}
