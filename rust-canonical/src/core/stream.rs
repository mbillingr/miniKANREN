use std::sync::Arc;

pub enum Stream<T> {
    Empty,
    Pair(T, Box<Stream<T>>),
    Suspension(Box<dyn FnOnce() -> Stream<T>>),
}

impl<T> Stream<T> {
    pub fn empty() -> Self {
        Stream::Empty
    }

    pub fn singleton(x: T) -> Self {
        Stream::cons(x, Stream::Empty)
    }

    pub fn cons(a: T, d: Self) -> Self {
        Stream::Pair(a, Box::new(d))
    }

    pub fn suspension(sup: impl 'static + FnOnce() -> Stream<T>) -> Self {
        Stream::Suspension(Box::new(sup))
    }

    pub fn from_iter(mut iter: impl Iterator<Item = T>) -> Self {
        match iter.next() {
            None => Stream::Empty,
            Some(item) => Stream::cons(item, Stream::from_iter(iter)),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Stream::Empty => true,
            _ => false,
        }
    }

    pub fn len(&self) -> Option<usize> {
        match self {
            Stream::Empty => Some(0),
            Stream::Pair(_, d) => d.len().map(|l| l + 1),
            Stream::Suspension(_) => None,
        }
    }

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

    pub fn take_inf_all(self) -> Stream<T> {
        match self {
            Stream::Empty => Stream::empty(),
            Stream::Pair(a, d) => Stream::cons(a, d.take_inf_all()),
            Stream::Suspension(sup) => sup().take_inf_all(),
        }
    }
}

impl<T> std::iter::IntoIterator for Stream<T> {
    type Item = T;
    type IntoIter = StreamIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        StreamIter(self)
    }
}

impl<T: 'static> Stream<T> {
    pub fn append_inf(s: Stream<T>, t: Stream<T>) -> Self {
        match s {
            Stream::Empty => t,
            Stream::Pair(a, d) => Stream::cons(a, Stream::append_inf(*d, t)),
            Stream::Suspension(sup) => {
                Stream::Suspension(Box::new(|| Stream::append_inf(t, sup())))
            }
        }
    }

    pub fn append_map_inf(self, g: Arc<dyn Fn(T) -> Self>) -> Self {
        match self {
            Stream::Empty => Stream::Empty,
            Stream::Pair(a, d) => Stream::append_inf(g(a), d.append_map_inf(g)),
            Stream::Suspension(sup) => Stream::Suspension(Box::new(|| sup().append_map_inf(g))),
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
