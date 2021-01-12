use super::stream::Stream;
use crate::core::stream::StreamIter;

pub trait Goal<T: Default>: Clone {
    fn apply(&self, s: T) -> Stream<T>;

    fn run(&self, n: usize) -> Stream<T> {
        self.apply(T::default()).take_inf(n)
    }

    fn run_inf(&self) -> Stream<T> {
        self.apply(T::default()).take_inf_all()
    }

    fn iter(&self) -> StreamIter<T> {
        self.apply(T::default()).into_iter()
    }
}

impl<T: Default, G: Clone + Fn(T) -> Stream<T>> Goal<T> for G {
    fn apply(&self, s: T) -> Stream<T> {
        self(s)
    }
}