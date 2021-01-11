use crate::core::logic_variable::Var;
use crate::core::stream::Stream;
use crate::core::substitution::Substitution;
use crate::core::value::Value;
use crate::goals::StatSubs;

pub mod core;
pub mod goals;
pub mod prelude;

pub fn call_with_fresh_var<T: Fn(StatSubs) -> Stream<StatSubs>>(
    name: &'static str,
    f: impl Fn(Var) -> T,
) -> T {
    f(Var::new(name))
}

pub fn reify(v: Value) -> impl Fn(StatSubs) -> Value {
    move |s| {
        let v = s.walk_star(&v);
        let r = Substitution::empty().reify_s(&v);
        r.walk_star(&v)
    }
}

pub fn run_goal(n: Option<usize>, g: impl Fn(StatSubs) -> Stream<StatSubs>) -> Stream<StatSubs> {
    match n {
        Some(n) => g(Substitution::empty()).take_inf(n),
        None => g(Substitution::empty()).take_inf_all(),
    }
}
