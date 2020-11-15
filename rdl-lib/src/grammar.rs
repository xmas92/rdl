use im::Vector;
use num::BigInt;
use peg::parser;
use std::{
    char::from_u32,
    cmp,
    collections::BTreeMap,
    sync::{Arc, Weak},
};

use crate::{
    error::{ArityError, GeneralError, RuntimeError},
    intrinsic, list,
    list::List,
    runtime::RuntimeResult,
    runtime::{Context, RuntimeValue},
};

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum Form {
    Value(Value),
    Special(Box<SpecialForm>),
    Macro(Box<MacroForm>),
    Symbol(String),
    Form(Vec<Form>),
    Unquote(Vec<Form>),
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum MacroForm {
    Value(String, Form),
    Function(String, Vec<BindingForm>, Option<BindingForm>, Form),
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum Value {
    Nil,
    Number(Number),
    QuoteString(String),
    Character(char),
    Boolean(bool),
    Keyword(String),
    List(Vec<Form>),
    Vector(Vec<Form>),
    Map(Vec<(Form, Form)>),
    Set(Vec<Form>),
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum Number {
    Integer(String, bool),
    Float(String, bool),
    Ratio(String, String),
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum BindingForm {
    Symbol(String),
    BindingVector(Vec<BindingForm>, Option<Box<BindingForm>>, Option<String>),
    BindingMap(
        Option<Vec<String>>,
        Vec<(BindingForm, Form)>,
        Option<String>,
        Option<Vec<(String, Form)>>,
    ),
}
impl BindingForm {
    fn compile(&self) -> CompiledBindingForm {
        match &self {
            BindingForm::Symbol(s) => CompiledBindingForm::Symbol(s.clone()),
            BindingForm::BindingVector(bf, rest, as_s) => CompiledBindingForm::BindingVector(
                bf.into_iter().map(|bf| bf.compile()).collect(),
                rest.clone().map(|bf| Box::new(bf.compile())),
                as_s.clone(),
            ),
            BindingForm::BindingMap(keys, bf, as_s, default) => CompiledBindingForm::BindingMap(
                keys.clone(),
                bf.into_iter()
                    .map(|(bf, f)| (bf.compile(), f.compile()))
                    .collect(),
                as_s.clone(),
                default
                    .clone()
                    .map(|v| v.into_iter().map(|(s, f)| (s, f.compile())).collect()),
            ),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum CompiledBindingForm {
    Symbol(String),
    BindingVector(
        Vec<CompiledBindingForm>,
        Option<Box<CompiledBindingForm>>,
        Option<String>,
    ),
    BindingMap(
        Option<Vec<String>>,
        Vec<(CompiledBindingForm, RuntimeValue)>,
        Option<String>,
        Option<BTreeMap<String, RuntimeValue>>,
    ),
}
// (def f (fn f ([] 1) ([a] a) ([a b] b) ([a b & c] (f c)))) ((f (f 8) 9 10 11 12) 3)
// (def f (fn f ([f] f) ([func b] (f (func b))))) (f f 1) (f {:a 1} :a)
// (def f1 (fn [[a b & c :as d]] a)) (def f2 (fn [[a b & c :as d]] b)) (def f3 (fn [[a b & c :as d]] c)) (def f4 (fn [[a b & c :as d]] d)) (def v "Axel") (f1 v) (f2 v) (f3 v) (f4 v)

// (defmacro a 3) (defmacro b (quote (fn [[a b :as c]] c))) a (b "test")
// (defmacro a [a b] '(a b)) (a + (+ 1 2))

impl CompiledBindingForm {
    fn bind_context(&self, context: &mut Context, value: RuntimeValue) -> Result<(), RuntimeError> {
        match &self {
            CompiledBindingForm::Symbol(s) => {
                context.set(s, value);
                Ok(())
            }
            CompiledBindingForm::BindingVector(bv, rest, as_s) => {
                bv.iter().enumerate().try_for_each(|(i, bf)| {
                    bf.bind_context(
                        context,
                        intrinsic::nth::internal3(
                            &value,
                            &RuntimeValue::Integer(i as i64),
                            &RuntimeValue::None,
                        )?,
                    )
                })?;
                if let Some(bf) = rest.as_ref() {
                    bf.bind_context(
                        context,
                        intrinsic::nthrest::internal2(
                            &value,
                            &RuntimeValue::Integer(bv.len() as i64),
                        )?,
                    )?
                }
                if let Some(s) = as_s {
                    context.set(s, value);
                }
                Ok(())
            }
            CompiledBindingForm::BindingMap(keys, bm, as_s, default) => {
                let empty = BTreeMap::<String, RuntimeValue>::new();
                let default = default.as_ref().unwrap_or(&empty);
                if let Some(keys) = keys {
                    keys.iter()
                        .map(|s| (s, RuntimeValue::Keyword(Box::new(s.clone()))))
                        .try_for_each(|(s, v)| -> Result<(), RuntimeError> {
                            context.set(
                                s,
                                intrinsic::get::internal3(
                                    &value,
                                    &v,
                                    &default.get(s).unwrap_or(&RuntimeValue::None),
                                )?,
                            );
                            Ok(())
                        })?;
                }
                bm.iter().try_for_each(|(bf, v)| {
                    let v = v.evaluate(context.clone())?;
                    if let CompiledBindingForm::Symbol(s) = bf {
                        context.set(
                            s,
                            intrinsic::get::internal3(
                                &value,
                                &v,
                                &default.get(s).unwrap_or(&RuntimeValue::None),
                            )?,
                        );
                        Ok(())
                    } else {
                        bf.bind_context(context, intrinsic::get::internal2(&value, &v)?)
                    }
                })?;
                if let Some(s) = as_s {
                    context.set(s, value);
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub enum SpecialForm {
    Def(String, Option<Form>),
    If(Form, Form, Option<Form>),
    Do(Vec<Form>),
    Let(Vec<(BindingForm, Form)>, Vec<Form>),
    Quote(Form),
    Var(String),
    Fn(
        Option<String>,
        Vec<(Vec<BindingForm>, Option<BindingForm>, Vec<Form>)>,
    ),
    Loop(Vec<(BindingForm, Form)>, Vec<Form>),
    Recur(Vec<Form>),
    Throw(Form),
    Try,
}

pub trait Unparse {
    fn unparse(&self) -> String;
}

impl Unparse for Form {
    fn unparse(&self) -> String {
        match self {
            Form::Value(v) => v.unparse(),
            Form::Special(s) => s.unparse(),
            Form::Macro(m) => m.unparse(),
            Form::Symbol(s) => s.clone(),
            Form::Form(f) => format!(
                "({})",
                f.iter()
                    .map(|f| f.unparse())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Form::Unquote(f) => match f.len() {
                1 => format!(
                    "(unquote {})",
                    f.iter()
                        .map(|f| f.unparse())
                        .collect::<Vec<String>>()
                        .join(" ")
                ),
                _ => format!(
                    "@({})",
                    f.iter()
                        .map(|f| f.unparse())
                        .collect::<Vec<String>>()
                        .join(" ")
                ),
            },
        }
    }
}
impl Unparse for Value {
    fn unparse(&self) -> String {
        match self {
            Value::Nil => String::from("nil"),
            Value::Number(n) => n.unparse(),
            Value::QuoteString(s) => format!("\"{}\"", s),
            Value::Character(c) => format!("\\u{:04X}", *c as u32),
            Value::Boolean(b) => format!("{}", *b),
            Value::Keyword(s) => format!(":{}", s),
            Value::List(l) => format!(
                "'({})",
                l.iter()
                    .map(|e| e.unparse())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Value::Vector(v) => format!(
                "[{}]",
                v.iter()
                    .map(|e| e.unparse())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Value::Map(m) => format!(
                "{{{}}}",
                m.iter()
                    .map(|(k, v)| format!("{} {}", k.unparse(), v.unparse()))
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            Value::Set(s) => format!(
                "#{{{}}}",
                s.iter()
                    .map(|e| e.unparse())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
        }
    }
}

impl Unparse for SpecialForm {
    fn unparse(&self) -> String {
        match self {
            SpecialForm::Def(s, f) => format!(
                "(def {}{})",
                s,
                f.as_ref()
                    .map_or(String::new(), |f| format!(" {}", f.unparse()))
            ),
            SpecialForm::If(c, t, f) => format!(
                "(if {} {} {})",
                c.unparse(),
                t.unparse(),
                f.as_ref().map_or(String::new(), |f| f.unparse())
            ),
            SpecialForm::Do(f) => format!(
                "(do {})",
                f.iter()
                    .map(|e| e.unparse())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            SpecialForm::Let(bf, f) => format!(
                "(let [{}] {})",
                bf.iter()
                    .map(|(bf, f)| format!("{} {}", bf.unparse(), f.unparse()))
                    .collect::<Vec<String>>()
                    .join(" "),
                f.iter()
                    .map(|e| e.unparse())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            SpecialForm::Quote(f) => format!("(quote {})", f.unparse()),
            SpecialForm::Var(s) => format!("(var {})", s),
            SpecialForm::Fn(s, f) => format!(
                "(fn {} {})",
                s.clone().unwrap_or_default(),
                f.iter()
                    .map(|(bf, bfm, f)| format!(
                        "([{}{}] {})",
                        bf.iter()
                            .map(|bf| bf.unparse())
                            .collect::<Vec<String>>()
                            .join(" "),
                        bfm.as_ref()
                            .map_or(String::new(), |bfm| format!(" & {}", bfm.unparse())),
                        f.iter()
                            .map(|f| f.unparse())
                            .collect::<Vec<String>>()
                            .join(" ")
                    ))
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            SpecialForm::Loop(bf, f) => format!(
                "(loop [{}] {})",
                bf.iter()
                    .map(|(bf, f)| format!("{} {}", bf.unparse(), f.unparse()))
                    .collect::<Vec<String>>()
                    .join(" "),
                f.iter()
                    .map(|e| e.unparse())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            SpecialForm::Recur(f) => format!(
                "(recur {})",
                f.iter()
                    .map(|e| e.unparse())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            SpecialForm::Throw(f) => format!("(throw {})", f.unparse()),
            SpecialForm::Try => todo!(),
        }
    }
}

impl Unparse for BindingForm {
    fn unparse(&self) -> String {
        match self {
            BindingForm::Symbol(s) => s.clone(),
            BindingForm::BindingVector(bf, bfm, as_s) => format!(
                "[{}{}{}]",
                bf.iter()
                    .map(|bf| bf.unparse())
                    .collect::<Vec<String>>()
                    .join(" "),
                bfm.as_ref()
                    .map_or(String::new(), |bfm| format!(" & {}", bfm.unparse())),
                as_s.as_ref()
                    .map_or(String::new(), |as_s| format!(" :as {}", as_s))
            ),
            BindingForm::BindingMap(keys, bf, as_s, default) => format!(
                "{{{}{}{}{}}}",
                keys.as_ref()
                    .map_or(String::new(), |keys| format!(":keys [{}]", keys.join(" "))),
                bf.iter()
                    .map(|(bf, f)| format!("{} {}", bf.unparse(), f.unparse()))
                    .collect::<Vec<String>>()
                    .join(" "),
                as_s.as_ref()
                    .map_or(String::new(), |as_s| format!(" :as {}", as_s)),
                default.as_ref().map_or(String::new(), |default| format!(
                    " :or {{{}}}",
                    default
                        .iter()
                        .map(|(s, f)| format!("{} {}", s, f.unparse()))
                        .collect::<Vec<String>>()
                        .join(" ")
                ))
            ),
        }
    }
}

impl Unparse for MacroForm {
    fn unparse(&self) -> String {
        match self {
            MacroForm::Value(s, f) => format!("(defmacro {} {})", s, f.unparse()),
            MacroForm::Function(s, bf, bfm, f) => format!(
                "(defmacro {} [{}{}] {})",
                s,
                bf.iter()
                    .map(|bf| bf.unparse())
                    .collect::<Vec<String>>()
                    .join(" "),
                bfm.as_ref()
                    .map_or(String::new(), |bfm| format!(" & {}", bfm.unparse())),
                f.unparse()
            ),
        }
    }
}

impl Unparse for Number {
    fn unparse(&self) -> String {
        match self {
            Number::Integer(s, b) => {
                if *b {
                    format!("{}M", s)
                } else {
                    format!("{}", s)
                }
            }
            Number::Float(s, b) => {
                if *b {
                    format!("{}M", s)
                } else {
                    format!("{}", s)
                }
            }
            Number::Ratio(_, _) => todo!(),
        }
    }
}

trait Compile {
    fn compile(&self) -> RuntimeValue;
    fn macro_compile(&self) -> RuntimeValue;
}

impl Compile for Form {
    fn compile(&self) -> RuntimeValue {
        match self {
            Form::Value(v) => v.compile(),
            Form::Special(s) => s.compile(),
            Form::Macro(m) => m.compile(),
            Form::Symbol(s) => {
                if let Ok(RuntimeValue::Macro(f)) = Context::get_global(s) {
                    (f)(Context::new(), Vector::new())
                } else {
                    let s = s.clone();
                    RuntimeValue::ContextLookup(Arc::new(move |context: Context| context.get(&s)))
                }
            }
            Form::Form(f) | Form::Unquote(f) => {
                if f.len() == 0 {
                    return RuntimeValue::None;
                }
                let first = if let Some(Form::Symbol(s)) = f.first() {
                    // Special case when symbol is a macro we do not want to let Form::Symbol::compile evaluate it as it is called a s a function
                    // This can probably be done better but requires extra runtime information to differentiate Macro::Value vs Macro::Function
                    let lookup = Context::get_global(s);
                    if let Ok(RuntimeValue::Macro(_)) = lookup {
                        lookup.unwrap()
                    } else {
                        f.first().unwrap().compile()
                    }
                } else {
                    f.first().unwrap().compile()
                };
                match first {
                    RuntimeValue::Macro(function) => {
                        let rest: Vector<RuntimeValue> =
                            f.into_iter().skip(1).map(|f| f.macro_compile()).collect();
                        if let Form::Form(_) = self {
                            (function)(Context::new(), rest)
                        } else {
                            match (function)(Context::new(), rest) {
                                v
                                @
                                (RuntimeValue::String(_)
                                | RuntimeValue::Map(_)
                                | RuntimeValue::Vector(_)
                                | RuntimeValue::Set(_)) => {
                                    if let Ok(RuntimeValue::List(l)) =
                                        intrinsic::sequence::internal1(&v)
                                    {
                                        RuntimeValue::Unquote(l.clone())
                                    } else {
                                        unreachable!()
                                    }
                                }
                                RuntimeValue::List(l) => RuntimeValue::Unquote(l.clone()),
                                v @ RuntimeValue::Unquote(_) => v.clone(),
                                v => RuntimeValue::Unquote(Box::new(List::unit(v.clone()))),
                            }
                        }
                    }
                    _ => {
                        let rest: Vector<RuntimeValue> =
                            f.into_iter().skip(1).map(|f| f.compile()).collect();
                        if let Form::Form(_) = self {
                            RuntimeValue::Evaluation(Arc::new(move |context: Context| {
                                let mut it: list::Iter<RuntimeValue>;
                                let first = match first.evaluate(context.clone())? {
                                    RuntimeValue::Unquote(l) => {
                                        it = l.into_iter();
                                        it.next().unwrap_or(RuntimeValue::None)
                                    }
                                    r => {
                                        it = List::empty().into_iter();
                                        r
                                    }
                                };
                                let args: Result<_, _> = it
                                    .chain(rest.clone().into_iter())
                                    .map(|value| value.evaluate(context.clone()))
                                    .flat_map(|value| match value {
                                        Ok(RuntimeValue::Unquote(l)) => {
                                            l.into_iter().map(|value| Ok(value)).collect()
                                        }
                                        r @ Ok(_) => vec![r],
                                        e @ Err(_) => vec![e],
                                    })
                                    .collect();
                                first.evaluate_with_args(context, args?)
                            }))
                        } else {
                            RuntimeValue::Evaluation(Arc::new(move |context: Context| {
                                let mut it: list::Iter<RuntimeValue>;
                                let first = match first.evaluate(context.clone())? {
                                    RuntimeValue::Unquote(l) => {
                                        it = l.into_iter();
                                        it.next().unwrap_or(RuntimeValue::None)
                                    }
                                    r => {
                                        it = List::empty().into_iter();
                                        r
                                    }
                                };
                                let args: Result<_, _> = it
                                    .chain(rest.clone().into_iter())
                                    .map(|value| value.evaluate(context.clone()))
                                    .flat_map(|value| match value {
                                        Ok(RuntimeValue::Unquote(l)) => {
                                            l.into_iter().map(|value| Ok(value)).collect()
                                        }
                                        r @ Ok(_) => vec![r],
                                        e @ Err(_) => vec![e],
                                    })
                                    .collect();
                                match first.evaluate_with_args(context, args?) {
                                    Ok(
                                        v
                                        @
                                        (RuntimeValue::String(_)
                                        | RuntimeValue::Map(_)
                                        | RuntimeValue::Vector(_)
                                        | RuntimeValue::Set(_)),
                                    ) => {
                                        if let Ok(RuntimeValue::List(l)) =
                                            intrinsic::sequence::internal1(&v)
                                        {
                                            Ok(RuntimeValue::Unquote(l))
                                        } else {
                                            unreachable!()
                                        }
                                    }
                                    Ok(RuntimeValue::List(l)) => Ok(RuntimeValue::Unquote(l)),
                                    v @ Ok(RuntimeValue::Unquote(_)) => v,
                                    Ok(v) => Ok(RuntimeValue::Unquote(Box::new(List::unit(v)))),
                                    e => e,
                                }
                            }))
                        }
                    }
                }
            }
        }
    }

    fn macro_compile(&self) -> RuntimeValue {
        match self {
            Form::Value(v) => v.macro_compile(),
            Form::Special(s) => s.macro_compile(),
            Form::Macro(m) => m.macro_compile(),
            Form::Symbol(s) => RuntimeValue::Form(Box::new(Form::Symbol(s.clone()))),
            Form::Form(f) => {
                // Reverse vector as list FromIterator builds the list last -> first
                RuntimeValue::List(Box::new(
                    f.into_iter().map(|f| f.macro_compile()).rev().collect(),
                ))
            }
            Form::Unquote(f) => {
                // Reverse vector as list FromIterator builds the list last -> first
                RuntimeValue::Unquote(Box::new(
                    f.into_iter().map(|f| f.macro_compile()).rev().collect(),
                ))
            }
        }
    }
}
// (defmacro defn [name & body] '((quote def) name '((quote fn) body))) (defn a [a] (+ a 3)) (a 2)
impl Compile for MacroForm {
    fn compile(&self) -> RuntimeValue {
        match self {
            MacroForm::Value(s, f) => {
                let f = f.compile();
                let ev =
                    RuntimeValue::Macro(Arc::new(move |context, _| match f.evaluate(context) {
                        Ok(f) => match f.decompile() {
                            Ok(s) => rdl::program(&s.as_str())
                                .unwrap()
                                .first()
                                .unwrap()
                                .compile(),
                            Err(e) => RuntimeValue::Evaluation(Arc::new(move |_| Err(e.clone()))),
                        },
                        Err(e) => RuntimeValue::Evaluation(Arc::new(move |_| Err(e.clone()))),
                    }));
                Context::set_global(s, ev);
                RuntimeValue::Form(Box::new(Form::Symbol(s.clone())))
            }
            MacroForm::Function(s, bf, bfm, f) => {
                let bf: Vec<_> = bf.into_iter().map(|bf| bf.compile()).collect();
                let bfm = bfm.as_ref().map(|bfm| bfm.compile());
                let f = f.compile();
                let s_debug = s.clone();
                let ev = RuntimeValue::Macro(Arc::new(move |context, args| {
                    let mut context = context.clone();
                    match (args.len(), bf.len()) {
                        (a, b) if a < b => {
                            let s_debug = s_debug.clone();
                            RuntimeValue::Evaluation(Arc::new(move |_| {
                                Err(RuntimeError::new(ArityError::new(
                                    args.len(),
                                    String::from(format!("Macro{:?}", s_debug)),
                                )))
                            }))
                        }
                        (a, b) => {
                            for (bf, arg) in bf.iter().zip(&args) {
                                if let Err(e) = bf.bind_context(&mut context, arg.clone()) {
                                    return RuntimeValue::Evaluation(Arc::new(move |_| {
                                        Err(e.clone())
                                    }));
                                }
                            }
                            if let Some(bfm) = bfm.as_ref() {
                                if let Err(e) = bfm.bind_context(
                                    &mut context,
                                    if b < a {
                                        RuntimeValue::Vector(Box::new(
                                            args.into_iter().skip(b).collect(),
                                        ))
                                    } else {
                                        RuntimeValue::None
                                    },
                                ) {
                                    return RuntimeValue::Evaluation(Arc::new(move |_| {
                                        Err(e.clone())
                                    }));
                                }
                            }
                            println!("{:?}", context);
                            //(defmacro -> [x & forms] (loop [x x forms forms] (if forms (let [form (first forms) threaded (if (seq? form) '((first form) x @(next form)) '(form x))] (recur threaded (next forms))) x))) (-> 1 (+ 2) (+ 3 (+ 2 4)))
                            match f.evaluate(context) {
                                Ok(f) => {
                                    println!("{:?}", f);
                                    match f.decompile() {
                                        Ok(s) => {
                                            println!("{:?}", s);
                                            rdl::program(&s.as_str())
                                                .unwrap()
                                                .first()
                                                .unwrap()
                                                .compile()
                                        }
                                        Err(e) => RuntimeValue::Evaluation(Arc::new(move |_| {
                                            Err(e.clone())
                                        })),
                                    }
                                }
                                Err(e) => {
                                    RuntimeValue::Evaluation(Arc::new(move |_| Err(e.clone())))
                                }
                            }
                        }
                    }
                }));
                Context::set_global(s, ev);
                RuntimeValue::Form(Box::new(Form::Symbol(s.clone())))
            }
        }
    }

    fn macro_compile(&self) -> RuntimeValue {
        todo!()
    }
}

impl Compile for Value {
    fn compile(&self) -> RuntimeValue {
        match self {
            Value::Nil => RuntimeValue::None,
            Value::Number(n) => n.compile(),
            Value::QuoteString(s) => RuntimeValue::String(Box::new(s.clone())),
            Value::Character(c) => RuntimeValue::Character(c.clone()),
            Value::Boolean(b) => RuntimeValue::Boolean(b.clone()),
            Value::Keyword(k) => RuntimeValue::Keyword(Box::new(k.clone())),
            Value::List(l) => {
                // Reverse vector as list FromIterator builds the list last -> first
                RuntimeValue::List(Box::new(l.into_iter().map(|f| f.compile()).rev().collect()))
            }
            Value::Vector(v) => {
                RuntimeValue::Vector(Box::new(v.into_iter().map(|f| f.compile()).collect()))
            }
            Value::Map(m) => RuntimeValue::Map(Box::new(
                m.into_iter()
                    .map(|(f1, f2)| (f1.compile(), f2.compile()))
                    .collect(),
            )),
            Value::Set(s) => {
                RuntimeValue::Set(Box::new(s.into_iter().map(|f| f.compile()).collect()))
            }
        }
    }

    fn macro_compile(&self) -> RuntimeValue {
        match self {
            Value::List(_) => RuntimeValue::Form(Box::new(Form::Value(self.clone()))),
            Value::Vector(v) => {
                RuntimeValue::Vector(Box::new(v.into_iter().map(|f| f.macro_compile()).collect()))
            }
            Value::Map(m) => RuntimeValue::Map(Box::new(
                m.into_iter()
                    .map(|(f1, f2)| (f1.compile(), f2.macro_compile()))
                    .collect(),
            )),
            Value::Set(s) => {
                RuntimeValue::Set(Box::new(s.into_iter().map(|f| f.macro_compile()).collect()))
            }
            _ => self.compile(),
        }
    }
}

impl Compile for Number {
    fn compile(&self) -> RuntimeValue {
        match self {
            Number::Integer(n, m) => {
                if *m {
                    RuntimeValue::BigInteger(Box::new(
                        BigInt::parse_bytes(n.as_bytes(), 10).unwrap(),
                    ))
                } else {
                    RuntimeValue::Integer(n.parse().unwrap())
                }
            }
            Number::Float(n, m) => {
                if *m {
                    todo!()
                } else {
                    RuntimeValue::Float(n.parse().unwrap())
                }
            }
            Number::Ratio(_, _) => todo!(),
        }
    }

    fn macro_compile(&self) -> RuntimeValue {
        self.compile()
    }
}

impl Compile for SpecialForm {
    fn compile(&self) -> RuntimeValue {
        match self {
            SpecialForm::Def(s, f) => {
                let v = match &f {
                    Some(f) => f.compile(),
                    None => RuntimeValue::None,
                };
                let s = s.clone();
                RuntimeValue::Evaluation(Arc::new(move |context: Context| {
                    let v = v.evaluate(context.clone())?;
                    Context::set_global(&s, v);
                    Ok(RuntimeValue::Form(Box::new(Form::Symbol(s.clone()))))
                }))
            }
            SpecialForm::If(b, t, f) => {
                let b = b.compile();
                let t = t.compile();
                let f = match f {
                    Some(f) => f.compile(),
                    None => RuntimeValue::None,
                };
                RuntimeValue::Evaluation(Arc::new(move |context: Context| {
                    let b = b.evaluate(context.clone())?;
                    match b {
                        RuntimeValue::None | RuntimeValue::Boolean(false) => {
                            f.evaluate(context.clone())
                        }
                        _ => t.evaluate(context.clone()),
                    }
                }))
            }
            SpecialForm::Do(f) => {
                let values: Vec<_> = f.into_iter().map(|f| f.compile()).collect();
                RuntimeValue::Evaluation(Arc::new(move |context: Context| match values.len() {
                    0 => Ok(RuntimeValue::None),
                    n => {
                        let res: Result<Vec<_>, _> = values
                            .iter()
                            .take(n - 1)
                            .map(|f| f.evaluate(context.clone()))
                            .collect();
                        res?;
                        values[n - 1].evaluate(context.clone())
                    }
                }))
            }
            SpecialForm::Let(b, f) => {
                let bindings: Vec<_> = b
                    .into_iter()
                    .map(|(bf, f)| (bf.compile(), f.compile()))
                    .collect();
                let values: Vec<_> = f.into_iter().map(|f| f.compile()).collect();
                RuntimeValue::Evaluation(Arc::new(move |context: Context| {
                    let mut context = context.clone();
                    for (bf, f) in bindings.clone() {
                        let f = f.evaluate(context.clone())?;
                        bf.bind_context(&mut context, f)?;
                    }
                    match values.len() {
                        0 => Ok(RuntimeValue::None),
                        n => {
                            let res: Result<Vec<_>, _> = values
                                .iter()
                                .take(n - 1)
                                .map(|f| f.evaluate(context.clone()))
                                .collect();
                            res?;
                            values[n - 1].evaluate(context.clone())
                        }
                    }
                }))
            }
            SpecialForm::Quote(f) => RuntimeValue::Form(Box::new(f.clone())),
            SpecialForm::Var(s) => {
                let s = s.clone();
                RuntimeValue::Evaluation(Arc::new(move |context: Context| {
                    let s = s.clone();
                    if let Ok(_) = context.get(&s) {
                        Ok(RuntimeValue::Form(Box::new(Form::Symbol(s))))
                    } else {
                        Err(RuntimeError::new(GeneralError::new(format!(
                            "Unable to resolve var: {:?}",
                            s
                        ))))
                    }
                }))
            }
            SpecialForm::Fn(s, f) => {
                let s = s.clone();
                let mut variadic: Option<(Vec<_>, _, Vec<_>)> = None;
                let mut variadic_arity: Option<usize> = None;
                let mut variadic_count: usize = 0;
                let (mut arity, mut f): (Vec<usize>, BTreeMap<_, (Vec<_>, _, Vec<_>)>) = f
                    .into_iter()
                    .inspect(|(bf, var, f)| {
                        if let (count, Some(_)) = (bf.len(), var) {
                            variadic_arity = variadic_arity
                                .map_or(Some(count + 1), |c| Some(cmp::min(c, count + 1)));
                            variadic_count += 1;
                            if Some(count + 1) == variadic_arity {
                                variadic = Some((
                                    bf.iter().map(|bf| bf.compile()).collect(),
                                    var.clone().map(|bf| bf.compile()),
                                    f.iter().map(|f| f.compile()).collect(),
                                ));
                            };
                        }
                    })
                    .filter_map(|(bf, var, f)| match var {
                        None => Some((
                            bf.len(),
                            (
                                bf.len(),
                                (
                                    bf.iter().map(|bf| bf.compile()).collect(),
                                    Option::<CompiledBindingForm>::None,
                                    f.iter().map(|f| f.compile()).collect(),
                                ),
                            ),
                        )),
                        _ => None,
                    })
                    .unzip();
                let report_arity = |(x, c)|
                    // TODO Add Logging
                    if variadic_arity.is_some() && variadic_arity.le(&x) {
                        println!(
                            "Warning multiple({}) definitions with arity {} overwritten by variadic function with arity {}",
                            c,
                            x.unwrap(),
                            variadic_arity.unwrap())
                    } else if c > 1 {
                        println!(
                            "Warning multiple({}) definitions with arity {}  found in function. Choice undefined",
                            c,
                            x.unwrap())
                    };
                if variadic_count > 1 {
                    println!(
                        "Warning multiple({}) variadic definitions found in function. One with smallest({}) arity chosen.",
                        variadic_count,
                        variadic_arity.unwrap());
                }
                arity.sort();
                (report_arity)(arity.into_iter().fold((None, 0), |acc, x| match acc {
                    (None, _) => (Some(x), 1),
                    (Some(y), c) if x == y => (Some(y), c + 1),
                    acc => {
                        (report_arity)(acc);
                        (Some(x), 1)
                    }
                }));
                if let (Some(value), Some(arity)) = (variadic, variadic_arity) {
                    f = f.into_iter().filter(|(k, _)| *k < arity).collect();
                    if !f.contains_key(&(arity - 1)) {
                        f.insert(arity - 1, value.clone());
                    }
                    f.insert(arity, value);
                }
                RuntimeValue::Function(Arc::new_cyclic(
                    |self_: &Weak<
                        Box<dyn Fn(Context, Vector<RuntimeValue>) -> RuntimeResult + Send + Sync>,
                    >| {
                        let self_ = self_.clone();
                        Box::new(move |context: Context, args: Vector<RuntimeValue>| {
                            let self_ = self_.clone();
                            let f = f.clone();
                            let mut context = context.clone();
                            if let Some(s) = s.clone() {
                                context.set(
                                    &s,
                                    RuntimeValue::Function(Arc::new(Box::new(
                                        move |context, args| {
                                            let self_ = self_.clone();
                                            (self_.upgrade().unwrap())(context, args)
                                        },
                                    ))),
                                )
                            }
                            let mut args = args;
                            let mut recursion = false;
                            loop {
                                let ret = {
                                    let mut context = context.clone();
                                    if let Some((bf, var, values)) = f.get(&args.len()) {
                                        // args.len()-1 <= bf.len() <= args.len()
                                        for (bf, arg) in bf.iter().zip(&args) {
                                            bf.bind_context(&mut context, arg.clone())?;
                                        }
                                        if let Some(var) = var {
                                            var.bind_context(
                                                &mut context,
                                                if bf.len() < args.len() {
                                                    match args.last().unwrap() {
                                                        // if we recur and the last argument is a vector or nil we assume it is updating the input argument
                                                        // FIXME need to define precis mechanism for recur and variadic function.
                                                        r @ RuntimeValue::Vector(_)
                                                        | r @ RuntimeValue::None
                                                            if recursion =>
                                                        {
                                                            r.clone()
                                                        }
                                                        r => RuntimeValue::Vector(Box::new(
                                                            Vector::unit(r.clone()),
                                                        )),
                                                    }
                                                } else {
                                                    RuntimeValue::None
                                                },
                                            )?;
                                        }
                                        match values.len() {
                                            0 => Ok(RuntimeValue::None),
                                            n => {
                                                let res: Result<Vec<_>, _> = values
                                                    .iter()
                                                    .take(n - 1)
                                                    .map(|f| f.evaluate(context.clone()))
                                                    .collect();
                                                res?;
                                                values[n - 1].evaluate(context.clone())
                                            }
                                        }
                                    } else {
                                        match f.last_key_value() {
                                            Some((_, (bf, Some(var), values)))
                                                if args.len() > bf.len() =>
                                            {
                                                // bf.len() < args.len()
                                                for (bf, arg) in bf.iter().zip(&args) {
                                                    bf.bind_context(&mut context, arg.clone())?;
                                                }
                                                var.bind_context(
                                                    &mut context,
                                                    RuntimeValue::Vector(Box::new(
                                                        args.into_iter().skip(bf.len()).collect(),
                                                    )),
                                                )?;
                                                match values.len() {
                                                    0 => Ok(RuntimeValue::None),
                                                    n => {
                                                        let res: Result<Vec<_>, _> = values
                                                            .iter()
                                                            .take(n - 1)
                                                            .map(|f| f.evaluate(context.clone()))
                                                            .collect();
                                                        res?;
                                                        values[n - 1].evaluate(context.clone())
                                                    }
                                                }
                                            }
                                            _ => Err(RuntimeError::new(ArityError::new(
                                                args.len(),
                                                String::from(format!("Fn{:?}", s)),
                                            ))),
                                        }
                                    }
                                };
                                match ret {
                                    Err(RuntimeError::Recur(recur_args)) => {
                                        args = recur_args;
                                        recursion = true;
                                    }
                                    ret => return ret,
                                };
                            }
                        })
                    },
                ))
            }
            SpecialForm::Loop(b, f) => {
                let bindings: Vec<_> = b
                    .into_iter()
                    .map(|(bf, f)| (bf.compile(), f.compile()))
                    .collect();
                let values: Vec<_> = f.into_iter().map(|f| f.compile()).collect();
                RuntimeValue::Evaluation(Arc::new(move |context: Context| {
                    let mut context = context.clone();
                    let mut bindings = bindings.clone();
                    loop {
                        let ret = {
                            for (bf, f) in &bindings {
                                let f = f.evaluate(context.clone())?;
                                bf.bind_context(&mut context, f)?;
                            }
                            match values.len() {
                                0 => Ok(RuntimeValue::None),
                                n => {
                                    let res: Result<Vec<_>, _> = values
                                        .iter()
                                        .take(n - 1)
                                        .map(|f| f.evaluate(context.clone()))
                                        .collect();
                                    res?;
                                    values[n - 1].evaluate(context.clone())
                                }
                            }
                        };
                        match ret {
                            Err(RuntimeError::Recur(mut recur_args)) => {
                                bindings.iter_mut().for_each(|(_, v)| {
                                    *v = recur_args.pop_front().unwrap_or(RuntimeValue::None)
                                })
                            }
                            ret => return ret,
                        };
                    }
                }))
            }
            SpecialForm::Recur(f) => {
                let values: Vec<_> = f.into_iter().map(|f| f.compile()).collect();
                RuntimeValue::Evaluation(Arc::new(move |context: Context| {
                    let res: Result<Vector<_>, _> =
                        values.iter().map(|f| f.evaluate(context.clone())).collect();
                    Err(RuntimeError::Recur(res?))
                }))
            }
            SpecialForm::Throw(_) => todo!(),
            SpecialForm::Try => todo!(),
        }
    }

    fn macro_compile(&self) -> RuntimeValue {
        RuntimeValue::Form(Box::new(Form::Special(Box::new(self.clone()))))
    }
}
parser! {
/// Doc comment
pub grammar rdl() for str {
    /// Top level parser rule
    /// This doc comment has multiple lines to test support for that as well
    pub rule program() -> Vec<Form>
        = f:form() ** __ { f }

    rule whitespace() = [' ' | '\n' | '\t' | ',']

    rule end_text() = !['a'..='z' | 'A'..='Z'| '0'..='9' | '*' | '+' | '!' | '-' | '_' | '\'' | '?' | '<' | '>' | '=']

    rule _() = quiet!{whitespace()*}
    rule __() = quiet!{end_text() _} / expected!("Delimiter")

    rule form() -> Form
        = v:value() { Form::Value(v) }
        / sf:special_form() { Form::Special(Box::new(sf)) }
        / mf:macro_form() { Form::Macro(Box::new(mf)) }
        / s:symbol()  { Form::Symbol(s) }
        / f:unquote_form() { Form::Unquote(f) }
        / l:normal_form() { Form::Form(l) }

    rule reserved_symbol()
        = "def" end_text()
        / "if" end_text()
        / "do" end_text()
        / "let" end_text()
        / "quote" end_text()
        / "unquote" end_text()
        / "var" end_text()
        / "fn" end_text()
        / "loop" end_text()
        / "recur" end_text()
        / "throw" end_text()
        / "try" end_text()
        / "true" end_text()
        / "false" end_text()
        / "nil" end_text()

    rule symbol() -> String
        // Fix '&'. Required for macros with binding forms. Not sure what to do about it.
        = quiet!{!number() s:$(['a'..='z' | 'A'..='Z' | '*' | '+' | '!' | '-' | '_' | '\'' | '?' | '<' | '>' | '=' | '&']
            ['a'..='z' | 'A'..='Z'| '0'..='9' | '*' | '+' | '!' | '-' | '_' | '\'' | '?' | '<' | '>' | '=']*)
            { String::from(s) }}
        / expected!("Symbol")

    rule binding_symbol() -> String
        // Fix '&'. Required for macros with binding forms. Not sure what to do about it.
        = quiet!{!number() s:$(['a'..='z' | 'A'..='Z' | '*' | '+' | '!' | '-' | '_' | '\'' | '?' | '<' | '>' | '=']
            ['a'..='z' | 'A'..='Z'| '0'..='9' | '*' | '+' | '!' | '-' | '_' | '\'' | '?' | '<' | '>' | '=']*)
            { String::from(s) }}
        / expected!("Binding Symbol")

    rule macro_symbol() -> String
        = !number() s:$(['a'..='z' | 'A'..='Z' | '*' | '+' | '!' | '-' | '_' | '\'' | '?' | '<' | '>' | '=']
            ['a'..='z' | 'A'..='Z'| '0'..='9' | '*' | '+' | '!' | '-' | '_' | '\'' | '?' | '<' | '>' | '=']*)
            { String::from(s) }

    rule value() -> Value
        = m:map() { Value::Map(m) }
        / l:list() { Value::List(l) }
        / v:vector() { Value::Vector(v) }
        / s:set() { Value::Set(s) }
        / quiet!{
            n:number() {Value::Number(n) }
            / qs:quote_string() {Value::QuoteString(qs) }
            / "nil" end_text() { Value::Nil }
            / k:keyword() {Value::Keyword(k) }
            / c:character() {Value::Character(c) }
            / b:boolean() {Value::Boolean(b) }}
            / expected!("Value")

    rule character() -> char
        = "\\newline" { '\n' }
        / "\\space" { ' ' }
        / "\\tab" { '\t' }
        / "\\formfeed"  { '\x0C' }
        / "\\backspace" { '\\' }
        / "\\return" { '\r' }
        / "\\u" s:$(['0'..='9'|'A'..='F'|'a'..='f']*<4,6>){ from_u32(u32::from_str_radix(s, 16).unwrap()).unwrap() }
        / "\\o" s:$(['0'..='3']['0'..='7']*<2,2>){ from_u32(u32::from_str_radix(s, 8).unwrap()).unwrap() }
        / "\\" s:$([_]) { s.chars().next().unwrap() }

    rule boolean() -> bool
        = "true" end_text() { true }
        / "false" end_text() { false }

    rule keyword() -> String
        = ":" s:symbol() { s }

    rule quote_string() -> String
        = "\"" s:$(((['\\'] / !['\"'])[_])*) "\"" { s.to_string() }

    rule number() -> Number
        = f:float() end_text() { f }
        / r:ratio() end_text() { r }
        / i:integer() end_text() { i }

    rule sign() -> String
        = s:$(['+' | '-']) { s.to_string() }

    rule exponent() -> String
        = "e" s:sign()? n:$(['0'..='9']+) { format!("e{}{}", s.unwrap_or(String::new()), n) }
        / "E" s:sign()? n:$(['0'..='9']+) { format!("e{}{}", s.unwrap_or(String::new()), n) }

    rule float_inner() -> String
        = "inf" { "inf".to_string() }
        / "NaN" { "NaN".to_string() }
        / n:$(['0'..='9']+['.']['0'..='9']*) e:exponent()?  { format!("{}{}", n, e.unwrap_or(String::new())) }
        / n:$(['.']['0'..='9']+) e:exponent()?  { format!("{}{}", n, e.unwrap_or(String::new())) }
        / n:$(['0'..='9']+) e:exponent()  { format!("{}{}", n, e) }

    rule float() -> Number
        = s:sign()? n:float_inner() "M" { Number::Float(format!("{}{}", s.unwrap_or(String::new()), n), true)}
        / s:sign()? n:float_inner() { Number::Float(format!("{}{}", s.unwrap_or(String::new()), n), false)}

    rule integer_inner() -> String
        = n:$(['0'..='9']+) { n.to_string() }

    rule integer() -> Number
        = s:sign()? n:integer_inner() "M" { Number::Integer(format!("{}{}", s.unwrap_or(String::new()), n), true)}
        / s:sign()? n:integer_inner() { Number::Integer(format!("{}{}", s.unwrap_or(String::new()), n), false)}


    rule ratio() -> Number
        = s:sign()? n:integer_inner() "/" d:integer_inner() { Number::Ratio(format!("{}{}", s.unwrap_or(String::new()), n), d)}


    rule form_end() = quiet!{")"} / expected!("Form End")
    rule normal_form() -> Vec<Form>
        = quiet!{"(" _ f:form() ** __ _ form_end() { f }} / expected!("Form")

    rule unquote_form() -> Vec<Form>
        = quiet!{"@(" _ f:form() ** __ _ form_end() { f }
        / "(" _ "unquote" _ f:form() ** __ _ form_end() { f }} / expected!("Form")

    rule map_pair() -> (Form,Form)
        = f1:form() _ f2:form() { (f1, f2) }

    rule map_end() = quiet!{"}"} / expected!("Map End")
    rule map() -> Vec<(Form,Form)>
        = quiet!{"{" _ mp:map_pair() ** __ _ map_end() { mp }} / expected!("Map")

    rule list_end() = quiet!{")"} / expected!("List End")
    rule list() -> Vec<Form>
        = quiet!{"'(" _ f:form() ** __ _ list_end() { f }} / expected!("List")


    rule set_end() = quiet!{"}"} / expected!("Set End")
    rule set() -> Vec<Form>
        = quiet!{"#{" _ f:form() ** __ _ set_end() { f }} / expected!("Set")

    rule vector_end() = quiet!{"]"} / expected!("Vector End")
    rule vector() -> Vec<Form>
        = quiet!{"[" _ f:form() ** __ _ vector_end() { f }} / expected!("Vector")

    rule macro_form_inner() -> MacroForm
    = s:symbol() _ "[" _ bf:binding_form() ** __ _ bfm:binding_form_more()? _ "]" _ f:form() { MacroForm::Function(s,bf,bfm,f) }
    / s:symbol() _ f:form() { MacroForm::Value(s,f) }
    rule macro_form() -> MacroForm
    = "(" _ "defmacro" _ mf:macro_form_inner() _ ")" { mf }

    rule special_form() -> SpecialForm
        = "(" _ sf:special_form_inner() _ ")" { sf }

    rule special_form_inner() -> SpecialForm
        = f:def_form() { f }
        / f:if_form() { f }
        / f:do_form() { f }
        / f:let_form() { f }
        / f:quote_form() { f }
        / f:var_form() { f }
        / f:fn_form() { f }
        / f:loop_form() { f }
        / f:recur_form() { f }
        / f:throw_form() { f }
        / f:try_form() { f }

    rule def_form() -> SpecialForm
        = "def" __ s:symbol() _ f:form()? { SpecialForm::Def(s,f) }

    rule if_form() -> SpecialForm
        = "if" __ f1:form() __ f2:form() __? f3:form()? { SpecialForm::If(f1,f2,f3) }

    rule do_form() -> SpecialForm
        = "do" __ f:form() ** __  { SpecialForm::Do(f) }

    rule let_form() -> SpecialForm
        = "let" __ "[" _ bff:binding_form_form() ** __ _  "]" _ f:form() ** __ { SpecialForm::Let(bff,f) }

    rule quote_form() -> SpecialForm
        = "quote" __ f:form() { SpecialForm::Quote(f) }

    rule var_form() -> SpecialForm
        = "var" __ s:symbol() { SpecialForm::Var(s) }

    rule fn_form() -> SpecialForm
        = "fn" __ s:symbol()? _ fnb:fn_body() { SpecialForm::Fn(s, fnb) }

    rule fn_body() -> Vec<(Vec<BindingForm>, Option<BindingForm>, Vec<Form>)>
        = sfb:single_fn_body() { vec![sfb]}
        / mfb:multi_fn_body()+ { mfb }

    rule single_fn_body() -> (Vec<BindingForm>, Option<BindingForm>, Vec<Form>)
        = "[" _ bf:binding_form() ** __ _ bfm:binding_form_more()? _ "]" _ f:form() ** __ { (bf , bfm, f) }

    rule multi_fn_body() -> (Vec<BindingForm>, Option<BindingForm>, Vec<Form>)
        = _ "(" _ sfb:single_fn_body() _ ")" _ { sfb }

    rule loop_form() -> SpecialForm
        = "loop" __ "[" _ bff:binding_form_form() ** __ _ "]" _ f:form() ** __ { SpecialForm::Loop(bff,f) }

    rule recur_form() -> SpecialForm
        = "recur" __ f:form() ** __ _ { SpecialForm::Recur(f) }

    rule throw_form() -> SpecialForm
        = "throw" __ f:form() { SpecialForm::Throw(f) }

    rule try_form() -> SpecialForm
        = "try" __ { SpecialForm::Try }

    rule binding_form_form() -> (BindingForm,Form)
        = b:binding_form() _ f:form() { (b,f) }

    rule binding_form_more() -> BindingForm
        = "&" __ bf:binding_form() __ { bf }

    rule as_symbol() -> String
    = ":as" __ s:symbol() __ { s }

    rule binding_form() -> BindingForm
        = s:binding_symbol()  { BindingForm::Symbol(s) }
        / bv:binding_vector() { bv }
        / bm:binding_map()    { bm }

    rule binding_vector() -> BindingForm
        = "[" bf:binding_form() ** __ _ bfm:binding_form_more()? _ s:as_symbol()? _ "]" { BindingForm::BindingVector(bf, bfm.map(|bfm| Box::new(bfm)), s) }

    rule or_map_pair() -> (String,Form)
    = s:symbol() _ f:form() { (s, f) }

    rule or_map() -> Vec<(String,Form)>
        = "{" _ mp:or_map_pair() ** __ _ "}" { mp }

    rule binding_map_or() -> Vec<(String,Form)>
        = ":or" __ m:or_map() { m }

    rule binding_map_keys() -> Vec<String>
        = ":keys" __ "[" _ s:symbol() ** __ _ "]" { s }

    rule binding_map_pair() -> (BindingForm, Form)
    = bf:binding_form() _ f:form() { (bf, f) }

    rule binding_map() -> BindingForm
        = "{" _ bmk:binding_map_keys()? _ bmp:binding_map_pair() ** __ _ s:as_symbol()? _ bmo:binding_map_or()? _ "}"
            { BindingForm::BindingMap(bmk,bmp,s,bmo) }

    }
}

#[derive(Debug)]
pub struct ParseError {
    string: String,
}

pub fn compile(program_string: &str) -> Result<Vec<RuntimeValue>, ParseError> {
    let res = rdl::program(program_string);
    match res {
        Ok(ast) => {
            let mut ret = Vec::<RuntimeValue>::new();

            for f in ast {
                ret.push(f.compile());
            }

            Ok(ret)
        }
        Err(err) => Err(ParseError {
            string: String::from(format!("{:?}", err)),
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn grammar_list() {
        assert!(rdl::program("(a)").is_ok());
        assert!(rdl::program("(a (a))").is_ok());
        assert!(rdl::program("((a (a)))").is_ok());
    }
    #[test]
    fn grammar_symbol() {
        assert!(rdl::program("a").is_ok());
        assert!(rdl::program("a1").is_ok());
        assert!(rdl::program("?a").is_ok());
        assert!(rdl::program("1a").is_err());
        assert!(rdl::program("defn").is_ok());
    }

    #[test]
    fn grammar_def() {
        assert!(rdl::program("(def a)").is_ok());
        assert!(rdl::program("(def a a)").is_ok());
        assert!(rdl::program("(def a (a b))").is_ok());
        // assert!(rdl::program("(def)").is_err());
        // assert!(rdl::program("(def (a b))").is_err());
    }
}
