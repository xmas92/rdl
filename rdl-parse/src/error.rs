use crate::runtime::RuntimeValue;
use im::Vector;
use std::{
    error::Error,
    fmt::{self, Display},
    sync::Arc,
};

#[derive(Debug, Clone)]
pub struct ArityError(usize, String);
impl ArityError {
    pub fn new(num_args: usize, function: String) -> ArityError {
        ArityError(num_args, function)
    }
}
impl Display for ArityError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "ArityError Wrong number of args ({}) passed to: {}",
            self.0, self.1
        ))
    }
}
impl Error for ArityError {}

#[derive(Debug, Clone)]
pub struct GeneralError(String);
impl GeneralError {
    pub fn new(text: String) -> GeneralError {
        GeneralError(text)
    }
}

impl Display for GeneralError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}
impl Error for GeneralError {}

#[derive(Debug, Clone)]
pub enum RuntimeError {
    Error(Option<Arc<dyn Error + Send + Sync>>),
    Recur(Vector<RuntimeValue>),
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", self))
    }
}
impl Error for RuntimeError {}

impl RuntimeError {
    pub fn new<E: Error + Send + Sync + 'static>(error: E) -> RuntimeError {
        RuntimeError::Error(Some(Arc::new(error)))
    }
}
