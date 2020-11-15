use std::borrow::Borrow;
use std::fmt;
use std::hash::Hash;
use std::iter::{FromIterator, IntoIterator, Iterator};
use std::ops::Deref;
use std::sync::Arc;

#[macro_export]
macro_rules! list {
    [] => {List::empty()};
    [$ele:expr] => {List::cons($ele, List::empty())};
    [$ele:expr, $($tail:expr),*] => {List::cons($ele, list![$($tail),*])};
}

#[derive(Clone, PartialEq, Eq)]
pub struct List<E> {
    size: usize,
    node: Arc<Node<E>>,
}

#[derive(Clone, PartialEq, Eq)]
enum Node<E> {
    Empty,
    Node(E, Arc<Node<E>>),
}

pub struct Iter<E: Clone> {
    node: Arc<Node<E>>,
}

impl<E: Clone> List<E> {
    #[inline]
    pub fn empty() -> Self {
        Self {
            size: 0,
            node: Arc::new(Node::Empty),
        }
    }

    #[inline]
    pub fn unit(first: E) -> Self {
        Self::cons(first, Self::empty())
    }

    #[inline]
    pub fn cons<T: Borrow<Self>>(first: E, rest: T) -> Self {
        let list: Self = rest.borrow().clone();
        Self {
            size: list.size + 1,
            node: Arc::new(Node::Node(first, list.node)),
        }
    }

    pub fn is_empty(&self) -> bool {
        match &self.node.deref() {
            Node::Empty => true,
            Node::Node(_, _) => false,
        }
    }

    pub fn first(&self) -> Option<&E> {
        match &self.node.deref() {
            Node::Empty => None,
            Node::Node(ref first, _) => Some(first),
        }
    }

    pub fn rest(&self) -> Self {
        match &self.node.deref() {
            Node::Empty => Self::empty(),
            Node::Node(_, ref rest) => Self {
                size: self.size - 1,
                node: rest.clone(),
            },
        }
    }

    pub fn skip(&self, count: usize) -> Self {
        if count > self.size {
            Self::empty()
        } else {
            let mut rest = &self.node;
            for _ in 0..count {
                rest = match rest.deref() {
                    Node::Empty => unreachable!(),
                    Node::Node(_, ref rest) => rest,
                };
            }
            Self {
                size: self.size - count,
                node: rest.clone(),
            }
        }
    }

    pub fn len(&self) -> usize {
        self.size
    }

    pub fn iter(&self) -> Iter<E> {
        Iter {
            node: self.node.clone(),
        }
    }
    pub fn order_preserve_map<A, B, F: FnMut(E) -> Result<A, B>>(
        &self,
        mut f: F,
    ) -> Result<List<A>, B> {
        let mut v: Vec<Arc<Node<A>>> = Vec::with_capacity(self.size);
        let empty = Arc::new(Node::Empty);
        for e in self {
            v.push(Arc::new(Node::Node((f)(e)?, empty.clone())));
        }
        let mut rest = v.pop().unwrap_or(empty);
        while let Some(mut node) = v.pop() {
            match Arc::get_mut(&mut node).unwrap() {
                Node::Empty => unreachable!(),
                Node::Node(_, n) => *n = rest,
            };
            rest = node;
        }
        Ok(List {
            size: self.size,
            node: rest,
        })
    }
}

impl<E: fmt::Debug + Clone> fmt::Debug for List<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(
            self.into_iter()
                .map(|e| format!("{:?}", e))
                .collect::<Vec<String>>()
                .join(" ")
                .as_str(),
        )
        // write!(f, "{:?}", self.node.deref())
    }
}
impl<E: fmt::Debug + Clone> fmt::Debug for Node<E> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Node::Empty => write!(f, ""),
            Node::Node(ref first, ref rest) => write!(f, "{:?} {:?}", first.clone(), rest.clone()),
        }
    }
}

impl<E: Clone> FromIterator<E> for List<E> {
    #[inline]
    fn from_iter<I: IntoIterator<Item = E>>(iterator: I) -> Self {
        iterator
            .into_iter()
            .fold(Self::empty(), |list, element| Self::cons(element, list))
    }
}

impl<'a, E: Clone> IntoIterator for &'a List<E> {
    type Item = E;
    type IntoIter = Iter<E>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        Iter {
            node: self.node.clone(),
        }
    }
}

impl<E: Clone> Iterator for Iter<E> {
    type Item = E;

    fn next(&mut self) -> Option<Self::Item> {
        let first;
        let rest;
        match &self.node.deref() {
            Node::Empty => return None,
            Node::Node(ref current_first, ref current_rest) => {
                first = current_first.clone();
                rest = current_rest.clone();
            }
        }
        self.node = rest;
        Some(first)
    }
}

impl<E: Hash + Clone> Hash for List<E> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for i in self {
            i.hash(state);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn list_macro() {
        let list = list![1, 2, 3];
        assert_eq!(
            list,
            List::cons(1, List::cons(2, List::cons(3, List::empty())))
        );
    }

    #[test]
    fn list_iterator() {
        let list1 = List::cons(
            1,
            List::cons(2, List::cons(3, List::cons(4, List::empty()))),
        );
        let list2: List<String> = list1
            .into_iter()
            .filter(|&x| x > 3)
            .map(|x| x.to_string())
            .collect();

        assert_eq!(list2, list![String::from("4")]);
    }

    #[test]
    fn list_first() {
        let list1 = list![1];
        let list2 = list![1, 2];
        assert_eq!(list1.first(), list2.first());

        let list1 = list![1];
        let list2 = list![2, 1];
        assert!(list1.first() != list2.first());

        let list: List<i32> = list![];
        assert_eq!(list.first(), None);
    }

    #[test]
    fn list_rest() {
        let list1 = list![1, 2, 3];
        assert_eq!(list1.rest(), list![2, 3]);

        let list1: List<i32> = list![];
        assert_eq!(list1.rest(), list![]);

        let list1 = list![1, 2];
        let list2 = list![1, 2, 3];
        assert!(list1.rest() != list2.rest());

        let list1: List<i32> = list![];
        let list2 = list![1];
        assert!(list1.rest() == list2.rest());
    }

    #[test]
    fn list_length() {
        let list1 = list![1, 2, 3];
        assert_eq!(list1.len(), 3);

        let list1: List<i32> = list![];
        assert_eq!(list1.len(), 0);

        let list1 = list![1, 2];
        let list2 = list![1, 2, 3];
        assert!(list1.len() != list2.len());
    }

    #[test]
    fn list_skip() {
        let list1 = list![1, 2, 3];
        assert_eq!(list1, list1.skip(0));

        let list2 = list![2, 3];
        assert_eq!(list2, list1.skip(1));

        let list2 = list![3];
        assert_eq!(list2, list1.skip(2));

        let list2: List<i32> = list![];
        assert_eq!(list2, list1.skip(3));
    }
}
