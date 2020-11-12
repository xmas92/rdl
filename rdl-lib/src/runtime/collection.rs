use im::{HashMap, HashSet, OrdMap, OrdSet, Vector};

use super::value::Value;

#[derive(Debug, Clone, Eq, PartialEq, Hash, Ord, PartialOrd)]
pub enum Collection {
    Vector(Box<Vector<Value>>),
    Map(Box<HashMap<Value, Value>>),
    OrdMap(Box<OrdMap<Value, Value>>),
    Set(Box<HashSet<Value>>),
    OrdSet(Box<OrdSet<Value>>),
}
