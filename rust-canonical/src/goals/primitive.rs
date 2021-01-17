/// Primitive relations.
use crate::core::goal::{Goal, RawGoal};
use crate::core::stream::Stream;
use crate::core::substitution::Substitution;
use crate::core::value::Value;
use crate::goals::{combinators, StatSubs};

/// Creates a goal that succeeds if u and v are equivalent.
pub fn eq(u: impl Into<Value>, v: impl Into<Value>) -> impl Goal<StatSubs> {
    let u = u.into();
    let v = v.into();
    move |s: Substitution<'static>| match s.unify(&u, &v) {
        Some(s) => Stream::singleton(s),
        None => Stream::empty(),
    }
}

/// Creates a goal that always succeeds.
pub fn succeed() -> impl Goal<StatSubs> {
    |s| Stream::singleton(s)
}

/// Creates a goal that never succeeds.
pub fn fail() -> impl Goal<StatSubs> {
    |_| Stream::empty()
}

/// Creates a goal that never produces a substitution. (Use with care! Can lead to infinite recursion.)
pub fn nevero() -> impl Goal<StatSubs> {
    |s| Stream::Suspension(Box::new(|| nevero().apply(s)))
}

/// Creates a goal that always produces a substitution.
pub fn alwayso() -> impl Goal<StatSubs> {
    |s| {
        Stream::Suspension(Box::new(|| {
            combinators::disj2(succeed(), alwayso()).apply(s)
        }))
    }
}
