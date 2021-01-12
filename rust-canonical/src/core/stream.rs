//! Lazy stream implementation.

use crate::core::goal::Goal;

/// Possibly infinite sequence of values.
pub enum Stream<T> {
    Empty,
    Pair(T, Box<Stream<T>>),
    Suspension(Box<dyn FnOnce() -> Stream<T>>),
}

impl<T> Stream<T> {
    /// Initialize an empty stream.
    pub fn empty() -> Self {
        Stream::Empty
    }

    /// Create a stream with one element.
    pub fn singleton(x: T) -> Self {
        Stream::cons(x, Stream::Empty)
    }

    /// Prepend an element to a stream.
    pub fn cons(a: T, d: Self) -> Self {
        Stream::Pair(a, Box::new(d))
    }

    /// Create a suspended (lazily evaluated) stream.
    pub fn suspension(sup: impl 'static + FnOnce() -> Stream<T>) -> Self {
        Stream::Suspension(Box::new(sup))
    }

    /// Create a stream with elements from an iterator.
    pub fn from_iter(mut iter: impl Iterator<Item = T>) -> Self {
        match iter.next() {
            None => Stream::Empty,
            Some(item) => Stream::cons(item, Stream::from_iter(iter)),
        }
    }

    /// Return `true` if the stream is empty.
    pub fn is_empty(&self) -> bool {
        match self {
            Stream::Empty => true,
            _ => false,
        }
    }

    /// Return the number of elements in an unsuspended stream, or `None` if
    /// the stream contains any suspensions.
    pub fn len(&self) -> Option<usize> {
        match self {
            Stream::Empty => Some(0),
            Stream::Pair(_, d) => d.len().map(|l| l + 1),
            Stream::Suspension(_) => None,
        }
    }

    /// Truncate a stream to at most `n` elements, resolving any suspensions.
    pub fn take_inf(self, n: usize) -> Stream<T> {
        if n == 0 {
            return Stream::empty();
        }
        match self {
            Stream::Empty => Stream::empty(),
            Stream::Pair(a, d) => Stream::cons(a, d.take_inf(n - 1)),
            Stream::Suspension(sup) => sup().take_inf(n),
        }
    }

    /// Resolve all suspensions in the stream.
    /// If the stream is infinite this function will not return and may crash.
    pub fn take_inf_all(self) -> Stream<T> {
        match self {
            Stream::Empty => Stream::empty(),
            Stream::Pair(a, d) => Stream::cons(a, d.take_inf_all()),
            Stream::Suspension(sup) => sup().take_inf_all(),
        }
    }

    /// Convert `Stream` to `Vec`.
    pub fn into_vec(self) -> Vec<T> {
        self.into_iter().collect()
    }
}

impl<T> std::iter::IntoIterator for Stream<T> {
    type Item = T;
    type IntoIter = StreamIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        StreamIter(self)
    }
}

impl<T: 'static + Default> Stream<T> {
    pub fn append_inf(s: Stream<T>, t: Stream<T>) -> Self {
        match s {
            Stream::Empty => t,
            Stream::Pair(a, d) => Stream::cons(a, Stream::append_inf(*d, t)),
            Stream::Suspension(sup) => {
                Stream::Suspension(Box::new(|| Stream::append_inf(t, sup())))
            }
        }
    }

    pub fn append_map_inf(self, g: impl 'static + Clone + Goal<T>) -> Self {
        match self {
            Stream::Empty => Stream::Empty,
            Stream::Pair(a, d) => Stream::append_inf(g.apply(a), d.append_map_inf(g)),
            Stream::Suspension(sup) => Stream::suspension(|| sup().append_map_inf(g)),
        }
    }

    pub fn map<U: 'static>(self, f: impl 'static + Fn(T) -> U) -> Stream<U> {
        match self {
            Stream::Empty => Stream::empty(),
            Stream::Pair(a, d) => Stream::cons(f(a), d.map(f)),
            Stream::Suspension(sup) => Stream::suspension(|| sup().map(f)),
        }
    }
}

impl<T: PartialEq> PartialEq for Stream<T> {
    fn eq(&self, other: &Self) -> bool {
        use Stream::*;
        match (self, other) {
            (Empty, Empty) => true,
            (Pair(a, x), Pair(b, y)) => a == b && x == y,
            _ => false,
        }
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for Stream<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Stream::Empty => write!(f, "()"),
            Stream::Suspension(_) => write!(f, "(...)"),
            Stream::Pair(x, next) => {
                let mut next = next;
                write!(f, "({:?}", x)?;
                loop {
                    match &**next {
                        Stream::Empty => break,
                        Stream::Pair(x, n) => {
                            write!(f, " {:?}", x)?;
                            next = n;
                        }
                        Stream::Suspension(_) => {
                            write!(f, "...")?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
        }
    }
}

/// Iterator over a `Stream`.
pub struct StreamIter<T>(Stream<T>);

impl<T> Iterator for StreamIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        match std::mem::replace(&mut self.0, Stream::Empty) {
            Stream::Empty => None,
            Stream::Pair(a, d) => {
                self.0 = *d;
                Some(a)
            }
            Stream::Suspension(sup) => {
                self.0 = sup();
                self.next()
            }
        }
    }
}
