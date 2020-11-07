use crate::{
    error::{ArityError, GeneralError, RuntimeError},
    grammar::{Form, Unparse},
    intrinsic,
    list::List,
};
use im::{hashmap, HashMap, HashSet, Vector};
use num::{BigInt, BigRational};
use std::cmp::{Eq, PartialEq};
use std::error::Error;
use std::fmt::{self, Alignment};
use std::hash::Hash;
use std::mem;
use std::result::Result;
use std::sync::{Arc, RwLock};
use std::{any::Any, fmt::Display};

#[derive(Debug, Clone)]
pub struct Context {
    context: HashMap<String, RuntimeValue>,
}

#[derive(Debug, Clone)]
pub struct ContextLookupError {
    symbol: String,
}

impl Error for ContextLookupError {}

impl Display for ContextLookupError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("Symbol: \'{}\' not found", self.symbol))
    }
}

pub type RuntimeResult = Result<RuntimeValue, RuntimeError>;

impl Context {
    pub fn new() -> Context {
        Context {
            context: HashMap::new(),
        }
    }
    pub fn new_macro() -> Context {
        Context {
            context: hashmap! {
                String::from("def") => RuntimeValue::None,
                String::from("if") => RuntimeValue::None,
                String::from("do") => RuntimeValue::None,
                String::from("let") => RuntimeValue::None,
                String::from("quote") => RuntimeValue::None,
                String::from("unquote") => RuntimeValue::None,
                String::from("var") => RuntimeValue::None,
                String::from("fn") => RuntimeValue::None,
                String::from("loop") => RuntimeValue::None,
                String::from("recur") => RuntimeValue::None,
                String::from("throw") => RuntimeValue::None,
                String::from("try") => RuntimeValue::None,
            },
        }
    }
    // (defn f [a & b] (do (println a b) (if b (recur (first b) (next b)) a))) (f 1 2 3 4)
    pub fn new_global_default() -> Context {
        Context {
            context: hashmap! {
                String::from("+") => intrinsic::plus::function(),
                String::from("-") => intrinsic::minus::function(),
                String::from("*") => intrinsic::star::function(),
                String::from("get") => intrinsic::get::function(),
                String::from("nth") => intrinsic::nth::function(),
                String::from("nthrest") => intrinsic::nthrest::function(),
                String::from("nthnext") => intrinsic::nthnext::function(),
                String::from("get_or_else") => intrinsic::get_or_else::function(),
                String::from("nth_or_else") => intrinsic::nth_or_else::function(),
                String::from("cons") => intrinsic::cons::function(),
                String::from("seq") => intrinsic::seq::function(),
                String::from("sequence") => intrinsic::sequence::function(),
                String::from("first") => intrinsic::first::function(),
                String::from("last") => intrinsic::last::function(),
                String::from("rest") => intrinsic::rest::function(),
                String::from("next") => intrinsic::next::function(),
                String::from("=") => intrinsic::eq::function(),
                String::from("list") => intrinsic::list::function(),
                String::from("println") => intrinsic::println::function(),
                String::from("seq?") => intrinsic::is_seq::function(),
                String::from("concat") => intrinsic::concat::function(),
                String::from("apply") => intrinsic::apply::function(),
                String::from("into") => intrinsic::into::function(),
                String::from("iter") => intrinsic::iter::function(),
                String::from("chain") => intrinsic::chain::function(),
                String::from("repeat") => intrinsic::repeat::function(),
                String::from("take") => intrinsic::take::function(),
                String::from("interleave") => intrinsic::interleave::function(),
                String::from("butlast") => intrinsic::butlast::function(),
                String::from("chunk") => intrinsic::chunk::function(),
                String::from("zip") => intrinsic::zip::function(),
            },
        }
    }
    pub fn get(&self, symbol: &String) -> RuntimeResult {
        if let Some(value) = self.context.get(symbol) {
            return Ok(value.clone());
        }
        Context::get_global(symbol)
    }
    pub fn get_global(symbol: &String) -> RuntimeResult {
        // TODO: Handle panic. Possible?
        let read_guard = CONTEXT.read().unwrap();
        if let Some(value) = read_guard.context.get(symbol) {
            return Ok(value.clone());
        }
        Err(RuntimeError::new(ContextLookupError {
            symbol: symbol.clone(),
        }))
    }
    pub fn set(&mut self, symbol: &String, value: RuntimeValue) {
        self.context.insert(symbol.clone(), value);
    }
    pub fn set_global(symbol: &String, value: RuntimeValue) {
        // TODO: Handle panic. Possible?
        let mut write_guard = CONTEXT.write().unwrap();
        write_guard.context.insert(symbol.clone(), value);
    }
    pub fn debug_print_global() {
        println!("Context: {:?}", CONTEXT.read().unwrap());
    }
    pub fn debug_print_global_complement(other: &Context) {
        println!(
            "Context: {:?}",
            CONTEXT
                .read()
                .unwrap()
                .context
                .clone()
                .relative_complement(other.context.clone())
        );
    }
    pub fn get_global_context() -> Context {
        CONTEXT.read().unwrap().as_ref().clone()
    }
}

lazy_static! {
    pub static ref CONTEXT: RwLock<Box<Context>> =
        RwLock::new(Box::new(Context::new_global_default()));
}

#[derive(Clone)]
pub enum RuntimeValue {
    None,
    Ratio(i64, i64),
    Integer(i64),
    BigInteger(Box<BigInt>),
    Float(f64),
    BigFloat(Box<BigRational>),
    String(Box<String>),
    Character(char),
    Boolean(bool),
    Keyword(Box<String>),
    Map(Box<HashMap<RuntimeValue, RuntimeValue>>),
    Vector(Box<Vector<RuntimeValue>>),
    Set(Box<HashSet<RuntimeValue>>),
    List(Box<List<RuntimeValue>>),
    Unquote(Box<List<RuntimeValue>>),
    Form(Box<Form>),
    Reference(Arc<dyn Any + Send + Sync>),
    Iterator(Arc<dyn Fn(Vector<RuntimeValue>) -> RuntimeResult + Send + Sync>),
    Function(Arc<dyn Fn(Context, Vector<RuntimeValue>) -> RuntimeResult + Send + Sync>),
    Evaluation(Arc<dyn Fn(Context) -> RuntimeResult + Send + Sync>),
    ContextLookup(Arc<dyn Fn(Context) -> RuntimeResult + Send + Sync>),
    Macro(Arc<dyn Fn(Context, Vector<RuntimeValue>) -> RuntimeValue + Send + Sync>),
}

impl RuntimeValue {
    #[inline]
    pub fn evaluate_global_context(&self) -> RuntimeResult {
        self.evaluate(Context::new())
    }
    pub fn evaluate(&self, context: Context) -> RuntimeResult {
        match self {
            RuntimeValue::Map(m) => {
                let m: Result<HashMap<_, _>, RuntimeError> = m
                    .clone()
                    .into_iter()
                    .map(|(v1, v2)| {
                        Ok((v1.evaluate(context.clone())?, v2.evaluate(context.clone())?))
                    })
                    .collect();
                Ok(RuntimeValue::Map(Box::new(m?)))
            }
            RuntimeValue::Vector(v) => {
                let v: Result<Vector<_>, RuntimeError> = v
                    .clone()
                    .into_iter()
                    .map(|v| Ok(v.evaluate(context.clone())?))
                    .collect();
                Ok(RuntimeValue::Vector(Box::new(v?)))
            }
            RuntimeValue::Set(s) => {
                let s: Result<HashSet<_>, RuntimeError> = s
                    .clone()
                    .into_iter()
                    .map(|v| Ok(v.evaluate(context.clone())?))
                    .collect();
                Ok(RuntimeValue::Set(Box::new(s?)))
            }
            RuntimeValue::List(l) => {
                let l = l.order_preserve_map(|v| Ok(v.evaluate(context.clone())?));
                Ok(RuntimeValue::List(Box::new(l?)))
            }
            RuntimeValue::Unquote(l) => {
                let l = l.order_preserve_map(|v| Ok(v.evaluate(context.clone())?));
                Ok(RuntimeValue::Unquote(Box::new(l?)))
            }
            RuntimeValue::Evaluation(f) => (f)(context),
            RuntimeValue::ContextLookup(f) => (f)(context),
            //RuntimeValue::Form(f) => { todo!() }
            _ => Ok(self.clone()),
        }
    }
    #[inline]
    pub fn evaluate_global_context_with_args(&self, args: Vector<RuntimeValue>) -> RuntimeResult {
        self.evaluate_with_args(Context::new(), args)
    }
    pub fn evaluate_with_args(
        &self,
        context: Context,
        args: Vector<RuntimeValue>,
    ) -> RuntimeResult {
        match self {
            RuntimeValue::Map(m) => match args.len() {
                2 => Ok(match m.get(&args[0]) {
                    Some(v) => v.clone(),
                    None => args[1].clone(),
                }),
                1 => Ok(match m.get(&args[0]) {
                    Some(v) => v.clone(),
                    None => RuntimeValue::None,
                }),
                n => Err(RuntimeError::new(ArityError::new(n, String::from("Map")))),
            },
            RuntimeValue::Vector(v) => match args.len() {
                1 => match &args[0] {
                    RuntimeValue::Integer(n) => {
                        if *n < 0 || *n as i128 >= v.len() as i128 {
                            Err(RuntimeError::new(GeneralError::new(String::from(format!(
                                "Index ({}) out of range",
                                n
                            )))))
                        } else {
                            let i: usize = *n as usize;
                            Ok(v[i].clone())
                        }
                    }
                    _ => Err(RuntimeError::new(GeneralError::new(String::from(
                        "Index must be an integer.",
                    )))),
                },
                n => Err(RuntimeError::new(ArityError::new(
                    n,
                    String::from("Vector"),
                ))),
            },
            RuntimeValue::Set(s) => match args.len() {
                1 => Ok(match s.contains(&args[0]) {
                    true => args[0].clone(),
                    false => RuntimeValue::None,
                }),
                n => Err(RuntimeError::new(ArityError::new(n, String::from("Map")))),
            },
            RuntimeValue::Function(f) => (f)(context, args),
            RuntimeValue::Iterator(f) => (f)(args),
            _ => Ok(self.clone()),
        }
    }

    pub fn decompile(&self) -> Result<String, RuntimeError> {
        // This is probably a bad way to do it. Can now not differentiate between a list and an evaluation.
        // To get a list you need to do (defmacro m ((quote list) a b c)) not (defmacro m '(a b c))
        match self {
            RuntimeValue::None => Ok(String::from("nil")),
            RuntimeValue::Ratio(_, _) => todo!(),
            RuntimeValue::Integer(n) => Ok(n.to_string()),
            RuntimeValue::BigInteger(_) => todo!(),
            RuntimeValue::Float(n) => Ok(n.to_string()),
            RuntimeValue::BigFloat(_) => todo!(),
            RuntimeValue::String(s) => Ok(*s.clone()),
            RuntimeValue::Character(c) => Ok(format!("\\u{:04X}", *c as u32)),
            RuntimeValue::Boolean(b) => {
                if *b {
                    Ok(String::from("true"))
                } else {
                    Ok(String::from("false"))
                }
            }
            RuntimeValue::Keyword(s) => Ok(format!(":{}", *s)),
            RuntimeValue::Map(m) => Ok(format!(
                "{{{}}}",
                m.iter()
                    .map(|(k, v)| Ok(format!("{} {}", k.decompile()?, v.decompile()?)))
                    .collect::<Result<Vec<_>, _>>()?
                    .join(" ")
            )),
            RuntimeValue::Vector(v) => Ok(format!(
                "[{}]",
                v.iter()
                    .map(|e| e.decompile())
                    .collect::<Result<Vec<_>, _>>()?
                    .join(" ")
            )),
            RuntimeValue::Set(s) => Ok(format!(
                "#{{{}}}",
                s.iter()
                    .map(|e| e.decompile())
                    .collect::<Result<Vec<_>, _>>()?
                    .join(" ")
            )),
            RuntimeValue::List(l) => Ok(format!(
                "({})",
                l.iter()
                    .map(|e| e.decompile())
                    .collect::<Result<Vec<_>, _>>()?
                    .join(" ")
            )),
            RuntimeValue::Unquote(l) => Ok(format!(
                "{}",
                l.iter()
                    .map(|e| e.decompile())
                    .collect::<Result<Vec<_>, _>>()?
                    .join(" ")
            )),
            RuntimeValue::Iterator(_) => todo!(),
            RuntimeValue::Form(f) => Ok(f.unparse()),
            RuntimeValue::Reference(_) => todo!(),
            RuntimeValue::Function(_) => todo!(),
            RuntimeValue::Evaluation(_) => todo!(),
            RuntimeValue::ContextLookup(_) => todo!(),
            RuntimeValue::Macro(_) => todo!(),
        }
    }
}

impl fmt::Debug for RuntimeValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut fmt_str = String::with_capacity(20);
        fmt_str.push(':');
        if let Some(s) = f.align() {
            fmt_str.push(f.fill());
            fmt_str.push(match s {
                Alignment::Left => '<',
                Alignment::Right => '>',
                Alignment::Center => '^',
            });
        }
        if f.sign_plus() {
            fmt_str.push('+');
        } else if f.sign_minus() {
            fmt_str.push('-');
        }
        if f.alternate() {
            fmt_str.push('#');
        }
        if f.sign_aware_zero_pad() {
            fmt_str.push('0');
        }
        if let Some(w) = f.width() {
            fmt_str.push_str(&w.to_string()[..]);
        }
        if let Some(p) = f.precision() {
            fmt_str.push('.');
            fmt_str.push_str(&p.to_string()[..]);
        }
        fmt_str.push('?');

        match &self {
            RuntimeValue::None => f.write_str("nil"),
            RuntimeValue::Ratio(n, d) => f.write_fmt(format_args!("{}/{}", n, d)),
            RuntimeValue::Integer(n) => f.write_fmt(format_args!("{}", n)),
            RuntimeValue::BigInteger(n) => f.write_fmt(format_args!("{}", n)),
            RuntimeValue::Float(n) => f.write_fmt(format_args!("{}", n)),
            RuntimeValue::BigFloat(n) => f.write_fmt(format_args!("{}", n)),
            RuntimeValue::String(s) => f.write_fmt(format_args!("\"{}\"", s)),
            RuntimeValue::Character(c) => f.write_fmt(format_args!("\'{}\'", c)),
            RuntimeValue::Boolean(b) => f.write_fmt(format_args!("\'{}\'", b)),
            RuntimeValue::Keyword(k) => f.write_fmt(format_args!(":{}", k)),
            RuntimeValue::Map(m) => f.write_fmt(format_args!("{:?}", m)),
            RuntimeValue::Vector(v) => f.write_fmt(format_args!("{:?}", v)),
            RuntimeValue::Set(s) => f.write_fmt(format_args!("{:?}", s)),
            RuntimeValue::List(l) => f.write_fmt(format_args!("({:?})", l)),
            RuntimeValue::Unquote(l) => f.write_fmt(format_args!("@({:?})", l)),
            RuntimeValue::Form(a) => f.write_fmt(format_args!("{:?}", a)),
            RuntimeValue::Reference(r) => f.write_fmt(format_args!("Ref{:?}", r.type_id())),
            RuntimeValue::Function(r) => f.write_fmt(format_args!("FN{:?}", Arc::as_ptr(r))),
            RuntimeValue::Evaluation(r) => f.write_fmt(format_args!("EV{:?}", Arc::as_ptr(r))),
            RuntimeValue::ContextLookup(r) => f.write_fmt(format_args!("LU{:?}", Arc::as_ptr(r))),
            RuntimeValue::Macro(r) => f.write_fmt(format_args!("MC{:?}", Arc::as_ptr(r))),
            RuntimeValue::Iterator(r) => f.write_fmt(format_args!("IT{:?}", Arc::as_ptr(r))),
        }
    }
}

impl PartialEq for RuntimeValue {
    fn eq(&self, other: &Self) -> bool {
        // hash check optimization if we ever cache hash values.
        match (&self, &other) {
            (RuntimeValue::None, RuntimeValue::None) => true,
            (RuntimeValue::Ratio(_, _), RuntimeValue::Ratio(_, _)) => todo!(),
            (RuntimeValue::Integer(i1), RuntimeValue::Integer(i2)) => i1 == i2,
            (RuntimeValue::BigInteger(i1), RuntimeValue::BigInteger(i2)) => i1 == i2,
            (RuntimeValue::Float(f1), RuntimeValue::Float(f2)) => f1 == f2,
            (RuntimeValue::BigFloat(f1), RuntimeValue::BigFloat(f2)) => f1 == f2,
            (RuntimeValue::String(s1), RuntimeValue::String(s2)) => s1 == s2,
            (RuntimeValue::Character(c1), RuntimeValue::Character(c2)) => c1 == c2,
            (RuntimeValue::Boolean(b1), RuntimeValue::Boolean(b2)) => b1 == b2,
            (RuntimeValue::Keyword(k1), RuntimeValue::Keyword(k2)) => k1 == k2,
            (RuntimeValue::Map(m1), RuntimeValue::Map(m2)) => m1 == m2,
            (RuntimeValue::Vector(v1), RuntimeValue::Vector(v2)) => v1 == v2,
            (RuntimeValue::Set(s1), RuntimeValue::Set(s2)) => s1 == s2,
            (RuntimeValue::List(l1), RuntimeValue::List(l2)) => l1 == l2,
            (RuntimeValue::Unquote(l1), RuntimeValue::Unquote(l2)) => l1 == l2,
            (RuntimeValue::Form(a1), RuntimeValue::Form(a2)) => a1 == a2,
            (RuntimeValue::Reference(r1), RuntimeValue::Reference(r2)) => {
                Arc::as_ptr(r1) == Arc::as_ptr(r2)
            }
            (RuntimeValue::Function(r1), RuntimeValue::Function(r2)) => {
                Arc::as_ptr(r1) == Arc::as_ptr(r2)
            }
            (RuntimeValue::Evaluation(r1), RuntimeValue::Evaluation(r2)) => {
                Arc::as_ptr(r1) == Arc::as_ptr(r2)
            }
            (RuntimeValue::ContextLookup(r1), RuntimeValue::ContextLookup(r2)) => {
                Arc::as_ptr(r1) == Arc::as_ptr(r2)
            }
            (RuntimeValue::Macro(r1), RuntimeValue::Macro(r2)) => {
                Arc::as_ptr(r1) == Arc::as_ptr(r2)
            }
            (RuntimeValue::Vector(v), RuntimeValue::List(l))
            | (RuntimeValue::List(l), RuntimeValue::Vector(v)) => {
                v.len() == l.len() && v.iter().zip(l.iter()).all(|(e1, e2)| *e1 == e2)
            }
            (RuntimeValue::Iterator(r1), RuntimeValue::Iterator(r2)) => {
                // TODO: Iterators should be sequenced and then checked for equality. Iters should rarely be used as keys in hashmaps
                Arc::as_ptr(r1) == Arc::as_ptr(r2)
            }

            (_, _) => false,
        }
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}
impl Eq for RuntimeValue {}

impl Hash for RuntimeValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match &self {
            RuntimeValue::None => {}
            RuntimeValue::Ratio(n, d) => {
                n.hash(state);
                d.hash(state);
            }
            RuntimeValue::Integer(n) => n.hash(state),
            RuntimeValue::BigInteger(n) => n.hash(state),
            RuntimeValue::Float(f) => unsafe { mem::transmute::<f64, u64>(*f).hash(state) },
            RuntimeValue::BigFloat(f) => f.hash(state),
            RuntimeValue::String(s) => s.hash(state),
            RuntimeValue::Character(c) => c.hash(state),
            RuntimeValue::Boolean(b) => b.hash(state),
            RuntimeValue::Keyword(k) => k.hash(state),
            RuntimeValue::Map(m) => m.hash(state),
            RuntimeValue::Vector(v) => v.hash(state),
            RuntimeValue::Set(s) => s.hash(state),
            RuntimeValue::List(l) | RuntimeValue::Unquote(l) => l.hash(state),
            RuntimeValue::Form(a) => a.hash(state),
            RuntimeValue::Reference(r) => Arc::as_ptr(r).hash(state),
            RuntimeValue::Function(r) => Arc::as_ptr(r).hash(state),
            RuntimeValue::Evaluation(r) => Arc::as_ptr(r).hash(state),
            RuntimeValue::ContextLookup(r) => Arc::as_ptr(r).hash(state),
            RuntimeValue::Macro(r) => Arc::as_ptr(r).hash(state),
            RuntimeValue::Iterator(r) => {
                // TODO: Iterators should be sequenced and then checked for equality. Iters should rarely be used as keys in hashmaps
                Arc::as_ptr(r).hash(state)
            }
        }
    }
}

impl From<i64> for RuntimeValue {
    fn from(n: i64) -> Self {
        RuntimeValue::Integer(n)
    }
}
impl From<BigInt> for RuntimeValue {
    fn from(n: BigInt) -> Self {
        RuntimeValue::BigInteger(Box::new(n))
    }
}
impl From<f64> for RuntimeValue {
    fn from(n: f64) -> Self {
        RuntimeValue::Float(n)
    }
}
impl From<BigRational> for RuntimeValue {
    fn from(n: BigRational) -> Self {
        RuntimeValue::BigFloat(Box::new(n))
    }
}
impl From<String> for RuntimeValue {
    fn from(s: String) -> Self {
        RuntimeValue::String(Box::new(s))
    }
}
impl From<char> for RuntimeValue {
    fn from(c: char) -> Self {
        RuntimeValue::Character(c)
    }
}
impl From<bool> for RuntimeValue {
    fn from(b: bool) -> Self {
        RuntimeValue::Boolean(b)
    }
}
impl From<Vector<RuntimeValue>> for RuntimeValue {
    fn from(v: Vector<RuntimeValue>) -> Self {
        RuntimeValue::Vector(Box::new(v))
    }
}
impl From<HashMap<RuntimeValue, RuntimeValue>> for RuntimeValue {
    fn from(m: HashMap<RuntimeValue, RuntimeValue>) -> Self {
        RuntimeValue::Map(Box::new(m))
    }
}
impl From<HashSet<RuntimeValue>> for RuntimeValue {
    fn from(s: HashSet<RuntimeValue>) -> Self {
        RuntimeValue::Set(Box::new(s))
    }
}
impl From<List<RuntimeValue>> for RuntimeValue {
    fn from(l: List<RuntimeValue>) -> Self {
        RuntimeValue::List(Box::new(l))
    }
}
impl Into<bool> for RuntimeValue {
    fn into(self) -> bool {
        match self {
            RuntimeValue::None | RuntimeValue::Boolean(false) => false,
            RuntimeValue::Evaluation(_) => todo!(),
            _ => true,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct RuntimeIter(pub RuntimeValue);
impl Iterator for RuntimeIter {
    type Item = RuntimeResult;

    fn next(&mut self) -> Option<Self::Item> {
        match &self.0 {
            RuntimeValue::Iterator(f) => {
                let (value, next) = match (f)(Vector::new().into()) {
                    e @ Err(_) => return Some(e),
                    Ok(RuntimeValue::Vector(mut v)) if v.len() == 2 => {
                        (v.pop_front().unwrap(), v.pop_front().unwrap())
                    }
                    _ => unreachable!(),
                };
                self.0 = next;
                match &self.0 {
                    RuntimeValue::None => None,
                    _ => Some(Ok(value)),
                }
            }
            _ => unreachable!(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        assert!(std::mem::size_of::<usize>() >= std::mem::size_of::<i64>());
        match self.0 {
            RuntimeValue::Iterator(_) => {
                match self
                    .0
                    .evaluate_global_context_with_args(vector![RuntimeValue::None].into())
                {
                    Ok(RuntimeValue::Integer(n)) if n < 0 => (usize::MAX, None),
                    Ok(RuntimeValue::Integer(n)) => (n as usize, Some(n as usize)),
                    Ok(RuntimeValue::None) => (0, None),
                    _ => unreachable!(),
                }
            }
            RuntimeValue::None => (0, Some(0)),
            _ => unreachable!(),
        }
    }

    #[allow(unused_must_use)]
    fn advance_by(&mut self, n: usize) -> Result<(), usize> {
        if n == 0 {
            Ok(())
        } else {
            match self.0 {
                RuntimeValue::Iterator(_) => {
                    let n = (n - 1) as i64;
                    let res = match self
                        .0
                        .evaluate_global_context_with_args(vector![n.into()].into())
                    {
                        Ok(RuntimeValue::Vector(mut v)) if v.len() == 3 => {
                            match v.pop_back().unwrap() {
                                RuntimeValue::Integer(taken) if taken >= 0 && taken <= n + 1 => {
                                    Ok((v.pop_back().unwrap(), taken))
                                }
                                RuntimeValue::Integer(_) => {
                                    Err(RuntimeError::new(GeneralError::new(String::from(
                                        "Iterator values taken must be in [0,n+1]",
                                    ))))
                                }
                                _ => Err(RuntimeError::new(GeneralError::new(String::from(
                                    "Iterator values taken must be an integer",
                                )))),
                            }
                        }
                        _ => unreachable!(),
                    };
                    match res {
                        Ok((RuntimeValue::None, taken)) => {
                            self.0 = RuntimeValue::None;
                            Err(taken as usize)
                        }
                        Ok((it, taken)) => {
                            assert!(taken == n + 1);
                            self.0 = it;
                            Ok(())
                        }
                        Err(_) => {
                            for i in 0..((n + 1) as usize) {
                                self.next().ok_or(i)?;
                            }
                            Ok(())
                        }
                    }
                }
                RuntimeValue::None => Err(0),
                _ => unreachable!(),
            }
        }
    }
}
/// Should only be used it RuntimeValue is known to be an Iterator.
impl IntoIterator for RuntimeValue {
    type Item = RuntimeResult;

    type IntoIter = RuntimeIter;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            RuntimeValue::Iterator(_) => RuntimeIter(self),
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec_rv_size() {
        use std::mem::size_of;
        let x = size_of::<RuntimeValue>();
        // let y = size_of::<HashMap<RuntimeValue,RuntimeValue>>();
        let z = size_of::<Vector<RuntimeValue>>();
        // let w = size_of::<HashSet<RuntimeValue>>();
        // let a = size_of::<BigInt>();
        // let b = size_of::<BigRational>();
        // let c = size_of::<String>();
        // let d = size_of::<Box<Vector<RuntimeValue>>>();
        // let e = size_of::<Box<dyn Any>>();
        // let f = size_of::<List<RuntimeValue>>();
        assert!(z - size_of::<usize>() >= 2 * x, "x: {}", x);
    }

    #[test]
    fn test_rv_fmt() {
        use crate::list;
        let value_vec = RuntimeValue::Vector(Box::new(im::vector![
            RuntimeValue::None,
            RuntimeValue::Integer(0),
            RuntimeValue::BigInteger(Box::new(BigInt::from(0))),
            RuntimeValue::Float(0.5),
            RuntimeValue::BigFloat(Box::new(BigRational::from_float(0.5).unwrap())),
            RuntimeValue::String(Box::new(String::from("string"))),
            RuntimeValue::Character('c'),
            RuntimeValue::Boolean(true),
            RuntimeValue::Keyword(Box::new(String::from("keyword"))),
        ]));
        let fnc = RuntimeValue::Function(Arc::new(|_, args| Ok(args.get(0).unwrap().clone())));
        let map = RuntimeValue::Map(Box::new(
            im::hashmap! {RuntimeValue::Character('a') => value_vec.clone(),
            fnc.clone() => fnc.clone(),
            RuntimeValue::None =>  RuntimeValue::List(Box::new(list!(
                                        RuntimeValue::Keyword(Box::new(String::from("a"))),
                                        RuntimeValue::Keyword(Box::new(String::from("b"))),
                                        RuntimeValue::Keyword(Box::new(String::from("c"))))))},
        ));
        let _str1 = format!("{:=<+#010.02?}", value_vec);
        let _str2 = format!("{:#?}", map);
        let _str3 = format!("{:?}", fnc);
    }
}
// (defmacro defn [name & body] '((quote def) name (cons (quote fn) body))) (defn a [a] (+ a 3)) (a 2)
// (defmacro apply [f & args] '((quote if) args (cons f args) '(f)))
// (defmacro defn [name & body] '((quote def) name (cons (quote fn) body))) (defn a [a] (+ a 3)) (a 2) (defmacro apply [f & args] (if args (cons f args) '(f))) (defn f ([a] (+ a 2))([] 1)([a & b] (if b (f (+ a (b 0)))))) (apply f) (apply f 1) (apply f 1 2 3)
