use crate::core::goal::Goal;
use crate::core::stream::Stream;
use crate::core::substitution::Substitution;
use crate::core::value::Value;
use crate::goals::StatSubs;

pub fn eq(u: impl Into<Value>, v: impl Into<Value>) -> impl Goal<StatSubs> {
    let u = u.into();
    let v = v.into();
    move |s: Substitution<'static>| match s.unify(&u, &v) {
        Some(s) => Stream::singleton(s),
        None => Stream::empty(),
    }
}

pub fn succeed() -> impl Goal<StatSubs> {
    |s| Stream::singleton(s)
}

pub fn fail() -> impl Goal<StatSubs> {
    |_| Stream::empty()
}

pub fn disj2(g1: impl Goal<StatSubs>, g2: impl Goal<StatSubs>) -> impl Goal<StatSubs> {
    move |s: Substitution<'static>| Stream::append_inf(g1.apply(s.clone()), g2.apply(s))
}

pub fn nevero() -> impl Goal<StatSubs> {
    |s| Stream::Suspension(Box::new(|| nevero().apply(s)))
}

pub fn alwayso() -> impl Goal<StatSubs> {
    |s| Stream::Suspension(Box::new(|| disj2(succeed(), alwayso()).apply(s)))
}

pub fn conj2(g1: impl Goal<StatSubs>, g2: impl 'static + Goal<StatSubs>) -> impl Goal<StatSubs> {
    move |s| g1.apply(s).append_map_inf(g2.clone())
}

pub fn ifte(
    g1: impl Goal<StatSubs>,
    g2: impl 'static + Goal<StatSubs>,
    g3: impl Goal<StatSubs>,
) -> impl Goal<StatSubs> {
    move |s: StatSubs| {
        let mut s_inf = g1.apply(s.clone());
        loop {
            match s_inf {
                Stream::Empty => return g3.apply(s),
                Stream::Pair(_, _) => return s_inf.append_map_inf(g2.clone()),
                Stream::Suspension(sup) => s_inf = sup(),
            }
        }
    }
}

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
