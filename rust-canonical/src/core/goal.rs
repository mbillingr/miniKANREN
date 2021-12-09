//! Goals are the building blocks of miniKANREN.
//!
//! A goal applied to a `Substitution` returns a `Stream` of `Substitution`s.
//! If the goal fails the `Stream` is empty.

use super::stream::Stream;
use crate::core::stream::StreamIter;
use std::rc::Rc;
use std::sync::Arc;

/// The `Goal` trait. See module-level documentation for more details.
pub trait Goal<T: Default>: Clone + RawGoal<T> {}

impl<T: Default, G: Clone + RawGoal<T>> Goal<T> for G {}

/// The `Goal` trait. See module-level documentation for more details.
pub trait RawGoal<T: Default> {
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

impl<T: Default, G: Fn(T) -> Stream<T>> RawGoal<T> for G {
    fn apply(&self, s: T) -> Stream<T> {
        self(s)
    }
}

impl<T: Default, G: Goal<T>> RawGoal<T> for Arc<G> {
    fn apply(&self, s: T) -> Stream<T> {
        (**self).apply(s)
    }
}

impl<T: Default> RawGoal<T> for Arc<dyn RawGoal<T>> {
    fn apply(&self, s: T) -> Stream<T> {
        (**self).apply(s)
    }
}

impl<T: Default, G: RawGoal<T>> RawGoal<T> for Rc<G> {
    fn apply(&self, s: T) -> Stream<T> {
        (**self).apply(s)
    }
}

impl<T: Default> RawGoal<T> for Rc<dyn RawGoal<T>> {
    fn apply(&self, s: T) -> Stream<T> {
        (**self).apply(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dummy_goal(_: ()) -> Stream<()> {
        Stream::empty()
    }

    fn run_goal<T: Default>(g: impl Goal<T>) -> Stream<T> {
        g.apply(T::default())
    }

    #[test]
    fn an_arcd_dyn_rawgoal_is_a_goal() {
        // should compile
        let adg: Arc<dyn RawGoal<()>> = Arc::new(dummy_goal);
        run_goal(adg);
    }

    #[test]
    fn an_rcd_dyn_rawgoal_is_a_goal() {
        // should compile
        let adg: Rc<dyn RawGoal<()>> = Rc::new(dummy_goal);
        run_goal(adg);
    }
}
