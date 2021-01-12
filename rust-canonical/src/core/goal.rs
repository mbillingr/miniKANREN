//! Goals are the building blocks of miniKANREN.
//!
//! A goal applied to a `Substitution` returns a `Stream` of `Substitution`s.
//! If the goal fails the `Stream` is empty.

use super::stream::Stream;
use crate::core::stream::StreamIter;

/// The `Goal` trait. See module-level documentation for more details.
pub trait Goal<T: Default>: Clone {
    /// Apply the goal to a `Substitution`.
    fn apply(&self, s: T) -> Stream<T>;

    /// Run the goal by applying it to an `Substitution`. Returns at most `n` values.
    fn run(&self, n: usize) -> Stream<T> {
        self.apply(T::default()).take_inf(n)
    }

    /// Run the goal by applying it to an `Substitution`. Returns all values but may crash.
    fn run_inf(&self) -> Stream<T> {
        self.apply(T::default()).take_inf_all()
    }

    /// Convert the goal into an iterator of values.
    fn iter(&self) -> StreamIter<T> {
        self.apply(T::default()).into_iter()
    }
}

impl<T: Default, G: Clone + Fn(T) -> Stream<T>> Goal<T> for G {
    fn apply(&self, s: T) -> Stream<T> {
        self(s)
    }
}
