pub mod substitution;
pub mod value;

use crate::logic_variable::{ReifiedVar, Var};
use std::any::Any;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Formatter;
use std::sync::Arc;
use substitution::Substitution;
use value::Value;

pub trait Structure: std::any::Any + std::fmt::Debug {
    fn occurs<'s>(&self, x: &Var, s: &Substitution<'s>) -> bool;
    fn unify<'s>(&self, v: &Value, s: Substitution<'s>) -> Option<Substitution<'s>>;
    fn walk_star(self: Arc<Self>, s: &Substitution<'_>) -> Value;
    fn reify_s<'s>(&self, s: Substitution<'s>) -> Substitution<'s>;

    fn as_any(&self) -> &dyn Any;

    fn eqv(&self, other: &Value) -> bool;
}

pub trait Atomic: std::fmt::Debug {}
impl Atomic for () {}
impl Atomic for bool {}
impl Atomic for u8 {}
impl Atomic for u16 {}
impl Atomic for u32 {}
impl Atomic for u64 {}
impl Atomic for u128 {}
impl Atomic for i8 {}
impl Atomic for i16 {}
impl Atomic for i32 {}
impl Atomic for i64 {}
impl Atomic for i128 {}
impl Atomic for char {}
impl Atomic for f64 {}
impl Atomic for f32 {}
impl Atomic for String {}
impl Atomic for &str {}
impl<T: Atomic> Atomic for Box<T> {}

impl<T: 'static + Atomic + PartialEq> Structure for T {
    fn occurs<'s>(&self, _x: &Var, _s: &Substitution<'s>) -> bool {
        false
    }

    fn unify<'s>(&self, v: &Value, s: Substitution<'s>) -> Option<Substitution<'s>> {
        if self.eqv(v) {
            Some(s)
        } else {
            None
        }
    }

    fn walk_star(self: Arc<Self>, _: &Substitution<'_>) -> Value {
        Value::from_arc(self)
    }

    fn reify_s<'s>(&self, s: Substitution<'s>) -> Substitution<'s> {
        s
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eqv(&self, other: &Value) -> bool {
        other
            .downcast_ref::<T>()
            .map(|o| o == self)
            .unwrap_or(false)
    }
}

impl Structure for Var {
    fn occurs<'s>(&self, x: &Var, _s: &Substitution<'s>) -> bool {
        self == x
    }

    fn unify<'s>(&self, v: &Value, s: Substitution<'s>) -> Option<Substitution<'s>> {
        if let Some(var) = v.try_as_var() {
            if self == &var {
                return Some(s);
            }
        }

        let v = v.clone();
        s.extended(*self, v)
    }

    fn walk_star(self: Arc<Self>, _: &Substitution<'_>) -> Value {
        Value::var(*self)
    }

    fn reify_s<'s>(&self, s: Substitution<'s>) -> Substitution<'s> {
        let reified = Value::new(ReifiedVar(s.n_subs()));
        s.extended(*self, reified).unwrap()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eqv(&self, other: &Value) -> bool {
        other
            .downcast_ref::<Var>()
            .map(|v| v == self)
            .unwrap_or(false)
    }
}

impl Structure for ReifiedVar {
    fn occurs<'s>(&self, _x: &Var, _s: &Substitution<'s>) -> bool {
        false
    }

    fn unify<'s>(&self, _v: &Value, _s: Substitution<'s>) -> Option<Substitution<'s>> {
        unimplemented!()
    }

    fn walk_star(self: Arc<Self>, _: &Substitution<'_>) -> Value {
        Value::from_arc(self)
    }

    fn reify_s<'s>(&self, s: Substitution<'s>) -> Substitution<'s> {
        s
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eqv(&self, other: &Value) -> bool {
        other
            .downcast_ref::<ReifiedVar>()
            .map(|v| v == self)
            .unwrap_or(false)
    }
}

impl Structure for Option<Value> {
    fn occurs<'s>(&self, x: &Var, s: &Substitution<'s>) -> bool {
        match self {
            Some(v) => s.occurs(x, v),
            None => false,
        }
    }

    fn unify<'s>(&self, v: &Value, s: Substitution<'s>) -> Option<Substitution<'s>> {
        if let Some(other) = v.downcast_ref::<Self>() {
            match (self, other) {
                (Some(su), Some(sv)) => s.unify(su, sv),
                (None, None) => Some(s),
                _ => None,
            }
        } else {
            None
        }
    }

    fn walk_star(self: Arc<Self>, s: &Substitution<'_>) -> Value {
        match &*self {
            None => Value::from_arc(self),
            Some(v) => s.walk_star(v),
        }
    }

    fn reify_s<'s>(&self, s: Substitution<'s>) -> Substitution<'s> {
        match &*self {
            None => s,
            Some(v) => s.reify_s(v),
        }
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eqv(&self, other: &Value) -> bool {
        if let Some(o) = other.downcast_ref::<Self>() {
            self == o
        } else {
            false
        }
    }
}

impl Structure for (Value, Value) {
    fn occurs<'s>(&self, x: &Var, s: &Substitution<'s>) -> bool {
        s.occurs(x, &self.0) || s.occurs(x, &self.1)
    }

    fn unify<'s>(&self, v: &Value, s: Substitution<'s>) -> Option<Substitution<'s>> {
        if let Some(other) = v.downcast_ref::<Self>() {
            s.unify(&self.0, &other.0)
                .and_then(|s| s.unify(&self.1, &other.1))
        } else {
            None
        }
    }

    fn walk_star(self: Arc<Self>, s: &Substitution<'_>) -> Value {
        (s.walk_star(&self.0), s.walk_star(&self.1)).into()
    }

    fn reify_s<'s>(&self, s: Substitution<'s>) -> Substitution<'s> {
        s.reify_s(&self.0).reify_s(&self.1)
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eqv(&self, other: &Value) -> bool {
        if let Some(o) = other.downcast_ref::<Self>() {
            self == o
        } else {
            false
        }
    }
}

pub enum Stream<T> {
    Empty,
    Pair(T, Box<Stream<T>>),
    Suspension(Box<dyn FnOnce() -> Stream<T>>),
}

impl<T> Stream<T> {
    pub fn empty() -> Self {
        Stream::Empty
    }

    pub fn singleton(x: T) -> Self {
        Stream::cons(x, Stream::Empty)
    }

    pub fn cons(a: T, d: Self) -> Self {
        Stream::Pair(a, Box::new(d))
    }

    pub fn suspension(sup: impl 'static + FnOnce() -> Stream<T>) -> Self {
        Stream::Suspension(Box::new(sup))
    }

    pub fn from_iter(mut iter: impl Iterator<Item = T>) -> Self {
        match iter.next() {
            None => Stream::Empty,
            Some(item) => Stream::cons(item, Stream::from_iter(iter)),
        }
    }

    pub fn is_empty(&self) -> bool {
        match self {
            Stream::Empty => true,
            _ => false,
        }
    }

    pub fn len(&self) -> Option<usize> {
        match self {
            Stream::Empty => Some(0),
            Stream::Pair(_, d) => d.len().map(|l| l + 1),
            Stream::Suspension(_) => None,
        }
    }

    fn take_inf(self, n: usize) -> Stream<T> {
        if n == 0 {
            return Stream::empty();
        }
        match self {
            Stream::Empty => Stream::empty(),
            Stream::Pair(a, d) => Stream::cons(a, d.take_inf(n - 1)),
            Stream::Suspension(sup) => sup().take_inf(n),
        }
    }

    fn take_inf_all(self) -> Stream<T> {
        match self {
            Stream::Empty => Stream::empty(),
            Stream::Pair(a, d) => Stream::cons(a, d.take_inf_all()),
            Stream::Suspension(sup) => sup().take_inf_all(),
        }
    }
}

impl<T> std::iter::IntoIterator for Stream<T> {
    type Item = T;
    type IntoIter = StreamIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        StreamIter(self)
    }
}

pub struct StreamIter<T>(Stream<T>);

impl<T> Iterator for StreamIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        match std::mem::replace(&mut self.0, Stream::Empty) {
            Stream::Empty => None,
            Stream::Pair(a, d) => {
                self.0 = *d;
                Some(a)
            }
            Stream::Suspension(sup) => {
                self.0 = sup();
                self.next()
            }
        }
    }
}

impl<T: 'static> Stream<T> {
    fn append_inf(s: Stream<T>, t: Stream<T>) -> Self {
        match s {
            Stream::Empty => t,
            Stream::Pair(a, d) => Stream::cons(a, Stream::append_inf(*d, t)),
            Stream::Suspension(sup) => {
                Stream::Suspension(Box::new(|| Stream::append_inf(t, sup())))
            }
        }
    }

    fn append_map_inf(self, g: Arc<dyn Fn(T) -> Self>) -> Self {
        match self {
            Stream::Empty => Stream::Empty,
            Stream::Pair(a, d) => Stream::append_inf(g(a), d.append_map_inf(g)),
            Stream::Suspension(sup) => Stream::Suspension(Box::new(|| sup().append_map_inf(g))),
        }
    }

    pub fn map<U: 'static>(self, f: impl 'static + Fn(T) -> U) -> Stream<U> {
        match self {
            Stream::Empty => Stream::empty(),
            Stream::Pair(a, d) => Stream::cons(f(a), d.map(f)),
            Stream::Suspension(sup) => Stream::suspension(|| sup().map(f)),
        }
    }
}

impl<T: PartialEq> PartialEq for Stream<T> {
    fn eq(&self, other: &Self) -> bool {
        use Stream::*;
        match (self, other) {
            (Empty, Empty) => true,
            (Pair(a, x), Pair(b, y)) => a == b && x == y,
            _ => false,
        }
    }
}

pub type StatSubs = Substitution<'static>;

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

impl<T: std::fmt::Debug> std::fmt::Debug for Stream<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Stream::Empty => write!(f, "()"),
            Stream::Suspension(_) => write!(f, "(...)"),
            Stream::Pair(x, next) => {
                let mut next = next;
                write!(f, "({:?}", x)?;
                loop {
                    match &**next {
                        Stream::Empty => break,
                        Stream::Pair(x, n) => {
                            write!(f, " {:?}", x)?;
                            next = n;
                        }
                        Stream::Suspension(_) => {
                            write!(f, "...")?;
                            break;
                        }
                    }
                }
                write!(f, ")")
            }
        }
    }
}

#[macro_export]
macro_rules! disj {
    () => { fail() };
    ($g:expr) => { $g };
    ($g0:expr; $($g:expr);*) => { disj2($g0, disj!($($g);*))}
}

#[macro_export]
macro_rules! conj {
    () => { succeed() };
    ($g:expr) => { $g };
    ($g0:expr, $($g:expr),*) => { conj2($g0, conj!($($g),*))}
}

#[macro_export]
macro_rules! defrel {
    ($name:ident($($args:ident),*) { $($g:expr),* $(,)? }) => {
        fn $name($($args: impl 'static + Into<Value>),*) -> impl Fn(StatSubs) -> Stream<StatSubs> {
            $(
                let $args = $args.into();
            )*
            move |s| {
                $(
                    let $args = $args.clone();
                )*
                Stream::suspension(move || conj!($($g),*)(s))
            }
        }
    };

    // alternate syntax: separate goals with ;
    ($name:ident($($args:ident),*) { $($g:expr);* $(;)? }) => {
        defrel!{$name($($args),*) { $($g),* }}
    };
}

#[macro_export]
macro_rules! run {
    (*, ($($x:ident),*), $($body:tt)*) => {
        run!(@ None, ($($x),*), $($body)*)
    };

    (*, $q:ident, $($g:expr),* $(,)?) => {
        run!(@ None, $q, $($g),*)
    };

    ($n:expr, ($($x:ident),*), $($body:tt)*) => {
        run!(@ Some($n), ($($x),*), $($body)*)
    };

    ($n:expr, $q:ident, $($g:expr),* $(,)?) => {
        run!(@ Some($n), $q, $($g),*)
    };

    (@ $n:expr, ($($x:ident),*), $($g:expr),* $(,)?) => {
        run!(@ $n, q, {
            fresh!(
                ($($x),*),
                eq(vec![$(Value::var($x.clone())),*], q),
                $($g),*
            )
        })
    };

    (@ $n:expr, $q:ident, $($g:expr),* $(,)?) => {{
        let $q = Var::new(stringify!($q));
        let var = Value::var($q.clone());
        run_goal($n, conj!($($g),*)).map(reify(var))
    }};
}

#[macro_export]
macro_rules! fresh {
    (($($x:ident),*), $($g:expr),* $(,)?) => {{
        $( let $x = Var::new(stringify!($x)); )*
        conj!($($g),*)
    }}
}

#[macro_export]
macro_rules! conde {
    ( $($($g:expr),*);*) => {
        disj!($(conj!( $($g),*));*)
    }
}

#[macro_export]
macro_rules! conda {
    ($($g:expr),*) => { conj!($($g),*) };

    ($g0:expr, $($g:expr),*; $($rest:tt)*) => {
        ifte(g0, conj!($($g),*), conda!($($rest)*))
    };
}

#[macro_export]
macro_rules! condu {
    ( $($g0:expr, $($g:expr),*);*) => {
        conda!($(once($gO), $($g),*);*)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn walk<'s>(v: &Var, s: &'s Substitution) -> Value {
        s.walk(&Value::var(v.clone())).clone()
    }

    macro_rules! substitution {
        () => {Substitution{ subs: Cow::Owned(HashMap::new())}};

        ($($var:ident : $val:expr),*) => {{
            let mut subs = HashMap::new();
            $(
                subs.insert($var.clone(), Value::from($val.clone()));
            )*
            Substitution {
                subs: Cow::Owned(subs)
            }
        }}
    }

    #[test]
    fn it_works() {
        let u = Var::new("u");
        let v = Var::new("v");
        let w = Var::new("w");
        let x = Var::new("x");
        let y = Var::new("y");
        let z = Var::new("z");

        assert_eq!(walk(&z, &substitution! {z: "a", x: w, y: z}), "a");
        assert_eq!(walk(&y, &substitution! {z: "a", x: w, y: z}), "a");
        assert_eq!(walk(&x, &substitution! {z: "a", x: w, y: z}), w);
        assert_eq!(walk(&x, &substitution! {x: y, v: x, w: x}), y);
        assert_eq!(walk(&v, &substitution! {x: y, v: x, w: x}), y);
        assert_eq!(walk(&w, &substitution! {x: y, v: x, w: x}), y);

        assert!(Substitution::empty().occurs(&x, &Value::var(x.clone())));
        assert!(substitution! {y: x}.occurs(&x, &Value::cons(Value::var(y), ())));
        assert!(substitution! {y: x}.occurs(&x, &vec![Value::var(y)].into()));
        assert!(!Substitution::empty().extend(x.clone(), vec![Value::var(x.clone())].into()));
        assert!(!substitution! {y: x}.extend(x.clone(), vec![Value::var(y.clone())].into()));

        assert_eq!(
            Substitution::empty().unify(
                &Value::new(Some(Value::var(x.clone()))),
                &Value::new(Some(Value::var(y.clone())))
            ),
            Some(substitution!(x: y)),
        );

        assert_eq!(
            Substitution::empty()
                .unify(
                    &Value::new(Some(Value::var(x.clone()))),
                    &Value::new(Some(Value::var(y.clone())))
                )
                .unwrap()
                .unify(
                    &Value::new(Some(Value::var(x.clone()))),
                    &Value::new(Some(Value::new(42)))
                ),
            Some(substitution!(x: y, y: 42)),
        );

        assert_eq!(
            eq(x, Value::var(u.clone()))(Substitution::empty()),
            Stream::singleton(substitution!(x: u))
        );
        assert_eq!(
            eq(x, 42)(Substitution::empty()),
            Stream::singleton(substitution!(x: 42))
        );
        assert_eq!(
            eq(42, 42)(Substitution::empty()),
            Stream::singleton(substitution!())
        );
        assert_eq!(eq(42, 123)(Substitution::empty()), Stream::empty());

        assert_eq!(fail()(Substitution::empty()), Stream::Empty);
        assert_eq!(eq(true, false)(Substitution::empty()), Stream::Empty);
        assert_eq!(
            eq(x, y)(Substitution::empty()),
            Stream::singleton(substitution! {x: y})
        );

        assert_eq!(
            disj2(eq("olive", x), eq("oil", x))(Substitution::empty()),
            Stream::cons(
                substitution! {x: "olive"},
                Stream::cons(substitution! {x: "oil"}, Stream::empty())
            )
        );

        // no value - stack overflow
        //assert_eq!(nevero()(Substitution::empty()).take_inf(1), Stream::Empty);

        assert_eq!(
            alwayso()(Substitution::empty()).take_inf(3),
            Stream::from_iter(
                vec![
                    Substitution::empty(),
                    Substitution::empty(),
                    Substitution::empty()
                ]
                .into_iter()
            )
        );

        assert_eq!(
            disj2(eq("olive", x), eq("oil", x))(Substitution::empty())
                .take_inf(5)
                .len(),
            Some(2)
        );

        assert_eq!(
            conj2(eq("olive", x), eq("oil", x))(Substitution::empty()),
            Stream::empty()
        );

        assert_eq!(
            conj2(eq("olive", x), eq(y.clone(), x.clone()))(Substitution::empty()),
            Stream::singleton(substitution! {y: "olive", x: "olive"})
        );

        assert_eq!(
            substitution! {x: "b", z: y, w: vec![Value::from(x), "e".into(), (z).into()]}
                .walk_star(&w.clone().into()),
            Value::from(vec![Value::from("b"), "e".into(), y.clone().into()])
        );

        let a1 = Value::from(vec![
            Value::from(u),
            Value::from(w),
            Value::from(y),
            Value::from(z),
            Value::from(Some(Value::from(vec![Value::from("ice"), Value::from(z)]))),
        ]);
        let a2 = Value::from("corn");
        let a3 = Value::from(vec![Value::from(v), Value::from(u)]);
        let s = substitution! {x: a1, y: a2, w: a3};
        //println!("{:?}", reify((&x).into())(s));
        assert_eq!(
            reify((x).into())(s),
            Value::from(vec![
                Value::from(ReifiedVar(0)),
                Value::from(vec![Value::from(ReifiedVar(1)), Value::from(ReifiedVar(0))]),
                Value::from("corn"),
                Value::from(ReifiedVar(2)),
                Value::from(vec![Value::from("ice"), Value::from(ReifiedVar(2))])
            ])
        );

        assert_eq!(
            run_goal(Some(5), disj2(eq("olive", x), eq("oil", x)))
                .into_iter()
                .map(|s| reify((x).into())(s))
                .collect::<Vec<_>>(),
            vec![Value::from("olive"), Value::from("oil")],
        );

        assert_eq!(
            ifte(succeed(), eq(false, y.clone()), eq(true, y.clone()))(Substitution::empty()),
            Stream::singleton(substitution!(y: false))
        );

        assert_eq!(
            ifte(fail(), eq(false, y.clone()), eq(true, y.clone()))(Substitution::empty()),
            Stream::singleton(substitution!(y: true))
        );

        assert_eq!(
            disj!(eq("virgin", x); eq("olive", x); eq("oil", x))(Substitution::empty()),
            Stream::from_iter(
                vec![
                    substitution! {x: "virgin"},
                    substitution! {x: "olive"},
                    substitution! {x: "oil"},
                ]
                .into_iter()
            )
        );

        defrel! {
            teacup(t) {
                disj!(eq("tea", t.clone()); eq("cup", t))
            }
        }

        assert_eq!(
            teacup(x.clone())(Substitution::empty())
                .into_iter()
                .collect::<Vec<_>>(),
            vec![substitution!(x: "tea"), substitution!(x: "cup")]
        );

        assert_eq!(
            format!("{:?}", fresh!((x, y), eq(x, y))(Substitution::empty())),
            "({x: y})"
        );

        assert_eq!(run!(1, x,), Stream::singleton(Value::from(ReifiedVar(0))));
        assert_eq!(run!(1, x, eq(x, 42)), Stream::singleton(Value::new(42)));
        assert_eq!(
            run!(1, (x, y),),
            Stream::singleton(Value::new(vec![
                Value::from(ReifiedVar(0)),
                Value::from(ReifiedVar(1))
            ]))
        );
        assert_eq!(
            run!(1, (x, y), eq(x, 42)),
            Stream::singleton(Value::new(vec![Value::new(42), Value::from(ReifiedVar(0))]))
        );

        defrel! {
            conso(a, d, p) {
                eq(vec![a, d], p)
            }
        }

        assert_eq!(
            run!(*, x, conso(1, 2, x)),
            Stream::singleton(Value::new(vec![Value::new(1), Value::new(2)]))
        );
        assert_eq!(
            run!(*, x, conso(1, x, vec![Value::new(1), Value::new(2)])),
            Stream::singleton(Value::new(2))
        );
        assert_eq!(
            run!(*, x, conso(x, 2, vec![Value::new(1), Value::new(2)])),
            Stream::singleton(Value::new(1))
        );
        assert_eq!(
            run!(*, x, conso(x.clone(), x, vec![Value::new(1), Value::new(2)])),
            Stream::empty()
        );
        assert_eq!(
            run!(*, x, conso(x.clone(), x, vec![Value::new(3), Value::new(3)])),
            Stream::singleton(Value::new(3))
        );

        assert_eq!(
            run!(5, q, eq(q, "onion")),
            Stream::singleton(Value::new("onion"))
        );

        assert_eq!(
            run!(5, q, eq(q, "onion"), alwayso(),),
            Stream::from_iter(std::iter::repeat(Value::new("onion")).take(5))
        );
    }
}
