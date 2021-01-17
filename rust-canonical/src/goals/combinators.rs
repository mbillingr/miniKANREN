//! Compose goals using combinators to build more complex goals
//!

use crate::core::goal::Goal;
use crate::core::stream::Stream;
use crate::core::substitution::Substitution;
use crate::goals::StatSubs;
use crate::{succeed, RawGoal, Value};
use std::sync::Arc;

/// Creates a goal that succeeds if either of its subgoals succeeds.
pub fn disj2(g1: impl Goal<StatSubs>, g2: impl Goal<StatSubs>) -> impl Goal<StatSubs> {
    move |s: Substitution<'static>| Stream::append_inf(g1.apply(s.clone()), g2.apply(s))
}

/// Creates a goal that succeeds if both of its subgoals succeed.
pub fn conj2(g1: impl Goal<StatSubs>, g2: impl 'static + Goal<StatSubs>) -> impl Goal<StatSubs> {
    move |s| g1.apply(s).append_map_inf(g2.clone())
}

/// Creates a goal that succeeds if g_cond and g_then succeed or g_cond fails and g_else succeeds.
pub fn ifte(
    g_cond: impl Goal<StatSubs>,
    g_then: impl 'static + Goal<StatSubs>,
    g_else: impl Goal<StatSubs>,
) -> impl Goal<StatSubs> {
    move |s: StatSubs| {
        let mut s_inf = g_cond.apply(s.clone());
        loop {
            match s_inf {
                Stream::Empty => return g_else.apply(s),
                Stream::Pair(_, _) => return s_inf.append_map_inf(g_then.clone()),
                Stream::Suspension(sup) => s_inf = sup(),
            }
        }
    }
}

/// Creates a goal that succeeds at most once.
pub fn once(g: impl Goal<StatSubs>) -> impl Goal<StatSubs> {
    move |s| {
        let mut s_inf = g.apply(s);
        loop {
            match s_inf {
                Stream::Empty => return Stream::Empty,
                Stream::Pair(a, _) => return Stream::singleton(a),
                Stream::Suspension(sup) => s_inf = sup(),
            }
        }
    }
}

pub fn everyg<'a, G: 'static + Goal<Substitution<'static>>>(
    goalfn: impl Fn(&'a Value) -> G,
    mut values: impl Iterator<Item = &'a Value>,
) -> Arc<dyn RawGoal<Substitution<'static>>> {
    let mut final_goal: Arc<dyn RawGoal<_>>;

    if let Some(v) = values.next() {
        final_goal = Arc::new(goalfn(v));
    } else {
        return Arc::new(succeed());
    }

    for v in values {
        final_goal = Arc::new(conj2(goalfn(v), final_goal));
    }

    final_goal
}
