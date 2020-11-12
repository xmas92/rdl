use super::{collection::Collection, number::Number};

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum Value {
    None,
    Boolean(bool),
    Character(char),
    String(String),
    Keyword(String),
    Number(Number),
    Collection(Collection),
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn assure_size() {
        use std::mem::size_of;

        assert!(
            size_of::<Value>() <= 32,
            format!("size_of::<Value>()[{}] <= 32 FAILED", size_of::<Value>())
        );
    }
}
