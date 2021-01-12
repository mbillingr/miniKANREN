use super::stream::Stream;

pub trait Goal<T> {
    fn apply(&self, s: T) -> Stream<T>;
}

impl<T, G: Fn(T) -> Stream<T>> Goal<T> for G {
    fn apply(&self, s: T) -> Stream<T> {
        self(s)
    }
}
