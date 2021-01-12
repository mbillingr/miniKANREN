use crate::core::goal::Goal;
use crate::core::stream::Stream;
use crate::core::substitution::Substitution;
use crate::core::value::Value;
use crate::goals::StatSubs;

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

/// Creates a goal that succeeds if either of its subgoals succeeds.
pub fn disj2(g1: impl Goal<StatSubs>, g2: impl Goal<StatSubs>) -> impl Goal<StatSubs> {
    move |s: Substitution<'static>| Stream::append_inf(g1.apply(s.clone()), g2.apply(s))
}

/// Creates a goal that never produces a substitution. (Use with care! Can lead to infinite recursion.)
pub fn nevero() -> impl Goal<StatSubs> {
    |s| Stream::Suspension(Box::new(|| nevero().apply(s)))
}

/// Creates a goal that always produces a substitution.
pub fn alwayso() -> impl Goal<StatSubs> {
    |s| Stream::Suspension(Box::new(|| disj2(succeed(), alwayso()).apply(s)))
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
