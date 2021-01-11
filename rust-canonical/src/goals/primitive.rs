use crate::core::stream::Stream;
use crate::core::value::Value;
use crate::goals::StatSubs;
use std::sync::Arc;

pub fn eq(u: impl Into<Value>, v: impl Into<Value>) -> impl Fn(StatSubs) -> Stream<StatSubs> {
    let u = u.into();
    let v = v.into();
    move |s| match s.unify(&u, &v) {
        Some(s) => Stream::singleton(s),
        None => Stream::empty(),
    }
}

pub fn succeed() -> impl Fn(StatSubs) -> Stream<StatSubs> {
    |s| Stream::singleton(s)
}

pub fn fail() -> impl Fn(StatSubs) -> Stream<StatSubs> {
    |_| Stream::empty()
}

pub fn disj2(
    g1: impl Fn(StatSubs) -> Stream<StatSubs>,
    g2: impl Fn(StatSubs) -> Stream<StatSubs>,
) -> impl Fn(StatSubs) -> Stream<StatSubs> {
    move |s| Stream::append_inf(g1(s.clone()), g2(s))
}

pub fn nevero() -> impl Fn(StatSubs) -> Stream<StatSubs> {
    |s| Stream::Suspension(Box::new(|| nevero()(s)))
}

pub fn alwayso() -> impl Fn(StatSubs) -> Stream<StatSubs> {
    |s| Stream::Suspension(Box::new(|| disj2(succeed(), alwayso())(s)))
}

pub fn conj2(
    g1: impl Fn(StatSubs) -> Stream<StatSubs>,
    g2: impl 'static + Fn(StatSubs) -> Stream<StatSubs>,
) -> impl Fn(StatSubs) -> Stream<StatSubs> {
    let g2 = Arc::new(g2);
    move |s| g1(s).append_map_inf(g2.clone())
}

pub fn ifte(
    g1: impl Fn(StatSubs) -> Stream<StatSubs>,
    g2: impl 'static + Fn(StatSubs) -> Stream<StatSubs>,
    g3: impl Fn(StatSubs) -> Stream<StatSubs>,
) -> impl Fn(StatSubs) -> Stream<StatSubs> {
    let g2 = Arc::new(g2);
    move |s| {
        let mut s_inf = g1(s.clone());
        loop {
            match s_inf {
                Stream::Empty => return g3(s),
                Stream::Pair(_, _) => return s_inf.append_map_inf(g2.clone()),
                Stream::Suspension(sup) => s_inf = sup(),
            }
        }
    }
}

pub fn once(g: impl Fn(StatSubs) -> Stream<StatSubs>) -> impl Fn(StatSubs) -> Stream<StatSubs> {
    move |s| {
        let mut s_inf = g(s);
        loop {
            match s_inf {
                Stream::Empty => return Stream::Empty,
                Stream::Pair(a, _) => return Stream::singleton(a),
                Stream::Suspension(sup) => s_inf = sup(),
            }
        }
    }
}
