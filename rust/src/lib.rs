use std::any::Any;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Formatter;
use std::hash::{Hash, Hasher};
use std::iter;
use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
    Var(Var),
    Val(Rc<dyn Structure>),
    RV(usize),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Var(a), Value::Var(b)) => a == b,
            (Value::Val(a), Value::Val(b)) => Rc::ptr_eq(a, b) || a.eqv(&**b),
            (Value::RV(a), Value::RV(b)) => a == b,
            _ => false,
        }
    }
}

impl Value {
    pub fn new(val: impl Into<Value>) -> Self {
        val.into()
    }

    fn ptr_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Var(a), Value::Var(b)) => a == b,
            (Value::Val(a), Value::Val(b)) => Rc::ptr_eq(a, b),
            (Value::RV(a), Value::RV(b)) => a == b,
            _ => false,
        }
    }
}

#[derive(Clone, Eq)]
pub struct Var(Rc<String>);

#[derive(Clone, PartialEq)]
pub struct Substitution<'s> {
    subs: Cow<'s, HashMap<Var, Value>>,
}

impl Var {
    pub fn new(name: impl Into<String>) -> Self {
        Var(Rc::new(name.into()))
    }
}

impl PartialEq for Var {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl Hash for Var {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        (&*self.0 as *const String).hash(hasher)
    }
}

impl From<Var> for Value {
    fn from(v: Var) -> Value {
        Value::Var(v)
    }
}

impl From<&Var> for Value {
    fn from(v: &Var) -> Value {
        Value::Var(v.clone())
    }
}

impl PartialEq<Var> for Value {
    fn eq(&self, v: &Var) -> bool {
        match self {
            Value::Var(sv) => v == sv,
            _ => false,
        }
    }
}

impl<'s> Substitution<'s> {
    pub fn empty() -> Self {
        Substitution {
            subs: Cow::Owned(HashMap::new()),
        }
    }

    fn walk<'a>(&'a self, mut v: &'a Value) -> &'a Value {
        loop {
            if let Value::Var(var) = v {
                if let Some(next) = self.subs.get(var) {
                    v = next;
                    continue;
                }
            }
            return v;
        }
    }

    fn walk_star(&self, v: &Value) -> Value {
        match self.walk(v).clone() {
            Value::Var(var) => Value::Var(var),
            Value::Val(val) => val.walk_star(self),
            v => v,
        }
    }

    fn extend(&mut self, x: Var, v: Value) -> bool {
        if self.occurs(&x, &v) {
            false
        } else {
            self.subs.to_mut().insert(x, v);
            true
        }
    }

    fn extended(mut self, x: Var, v: Value) -> Option<Self> {
        if self.extend(x, v) {
            Some(self)
        } else {
            None
        }
    }

    fn occurs(&self, x: &Var, v: &Value) -> bool {
        let v = self.walk(v);
        match v {
            Value::Var(var) => var == x,
            Value::Val(val) => val.occurs(x, self),
            Value::RV(_) => false,
        }
    }

    fn unify(self, u: &Value, v: &Value) -> Option<Self> {
        let u = self.walk(u);
        let v = self.walk(v);
        match (u, v) {
            (_, _) if u.ptr_eq(v) => Some(self),
            (Value::Var(uvar), _) => {
                let uvar = uvar.clone();
                let v = v.clone();
                self.extended(uvar, v)
            }
            (_, Value::Var(vvar)) => {
                let u = u.clone();
                let vvar = vvar.clone();
                self.extended(vvar, u)
            }
            (Value::Val(uval), Value::Val(vval)) => {
                let uval = uval.clone();
                let vval = vval.clone();
                uval.unify(&*vval, self)
            }
            _ => panic!("Cannot unify if there are reified variables"),
        }
    }

    fn reify_s(self, v: &Value) -> Self {
        let v = self.walk(v);
        match v {
            Value::Var(var) => {
                let var = var.clone();
                let reified = Value::RV(self.subs.len());
                self.extended(var, reified).unwrap()
            }
            Value::Val(val) => val.clone().reify_s(self),
            _ => self,
        }
    }
}

pub trait Structure: std::any::Any + std::fmt::Debug {
    fn occurs<'s>(&self, x: &Var, s: &Substitution<'s>) -> bool;
    fn unify<'s>(&self, v: &dyn Structure, s: Substitution<'s>) -> Option<Substitution<'s>>;
    fn walk_star(self: Rc<Self>, s: &Substitution<'_>) -> Value;
    fn reify_s<'s>(&self, s: Substitution<'s>) -> Substitution<'s>;

    fn as_any(&self) -> &dyn Any;

    fn eqv(&self, other: &dyn Structure) -> bool;
}

impl<T: Structure> From<T> for Value {
    fn from(v: T) -> Self {
        Value::Val(Rc::new(v))
    }
}

pub trait Atomic: std::fmt::Debug {}
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
impl<T: Atomic> Atomic for Rc<T> {}

impl<T: 'static + Atomic + PartialEq> PartialEq<T> for Value {
    fn eq(&self, other: &T) -> bool {
        match self {
            Value::Val(val) => val
                .as_any()
                .downcast_ref::<T>()
                .map(|x| x == other)
                .unwrap_or(false),
            _ => false,
        }
    }
}

impl<T: 'static + Atomic + PartialEq> Structure for T {
    fn occurs<'s>(&self, _x: &Var, _s: &Substitution<'s>) -> bool {
        false
    }

    fn unify<'s>(&self, v: &dyn Structure, s: Substitution<'s>) -> Option<Substitution<'s>> {
        if self.eqv(v) {
            Some(s)
        } else {
            None
        }
    }

    fn walk_star(self: Rc<Self>, _: &Substitution<'_>) -> Value {
        Value::Val(self)
    }

    fn reify_s<'s>(&self, s: Substitution<'s>) -> Substitution<'s> {
        s
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eqv(&self, other: &dyn Structure) -> bool {
        other
            .as_any()
            .downcast_ref::<T>()
            .map(|o| o == self)
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

    fn unify<'s>(&self, v: &dyn Structure, s: Substitution<'s>) -> Option<Substitution<'s>> {
        if let Some(other) = v.as_any().downcast_ref::<Self>() {
            match (self, other) {
                (Some(su), Some(sv)) => s.unify(su, sv),
                (None, None) => Some(s),
                _ => None,
            }
        } else {
            None
        }
    }

    fn walk_star(self: Rc<Self>, s: &Substitution<'_>) -> Value {
        match &*self {
            None => Value::Val(self),
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

    fn eqv(&self, other: &dyn Structure) -> bool {
        if let Some(o) = other.as_any().downcast_ref::<Self>() {
            self == o
        } else {
            false
        }
    }
}

impl Structure for Vec<Value> {
    fn occurs<'s>(&self, x: &Var, s: &Substitution<'s>) -> bool {
        self.iter().any(|v| s.occurs(x, v))
    }

    fn unify<'s>(&self, v: &dyn Structure, mut s: Substitution<'s>) -> Option<Substitution<'s>> {
        if let Some(other) = v.as_any().downcast_ref::<Self>() {
            for (a, b) in self.iter().zip(other) {
                s = s.unify(a, b)?;
            }
            Some(s)
        } else {
            None
        }
    }

    fn walk_star(self: Rc<Self>, s: &Substitution<'_>) -> Value {
        self.iter()
            .map(|v| s.walk_star(v))
            .collect::<Vec<_>>()
            .into()
    }

    fn reify_s<'s>(&self, mut s: Substitution<'s>) -> Substitution<'s> {
        for v in self.iter() {
            s = s.reify_s(v);
        }
        s
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eqv(&self, other: &dyn Structure) -> bool {
        if let Some(o) = other.as_any().downcast_ref::<Self>() {
            self == o
        } else {
            false
        }
    }
}

pub type StatSubs = Substitution<'static>;

pub struct Interleave<I, J>
where
    I: Iterator,
    J: Iterator<Item = I::Item>,
{
    iter1: I,
    iter2: J,
    next_iter: u8,
}

impl<I, J> Iterator for Interleave<I, J>
where
    I: Iterator,
    J: Iterator<Item = I::Item>,
{
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        if self.next_iter == 1 {
            if let Some(x) = self.iter1.next() {
                self.next_iter = 2;
                Some(x)
            } else {
                self.iter2.next()
            }
        } else {
            if let Some(x) = self.iter1.next() {
                self.next_iter = 1;
                Some(x)
            } else {
                self.iter2.next()
            }
        }
    }
}

fn interleave<I, J>(s: I, t: J) -> Interleave<I, J>
where
    I: Iterator,
    J: Iterator<Item = I::Item>,
{
    Interleave {
        iter1: s,
        iter2: t,
        next_iter: 1,
    }
}

pub struct InterleaveFlatten<I>
where
    I: Iterator,
    I::Item: Iterator,
{
    input_iterator: I,
    output_iterators: Vec<I::Item>,
    cursor: usize,
}

impl<I> Iterator for InterleaveFlatten<I>
where
    I: Iterator,
    I::Item: Iterator,
{
    type Item = <I::Item as Iterator>::Item;
    fn next(&mut self) -> Option<Self::Item> {
        if self.cursor >= self.output_iterators.len() {
            self.cursor -= self.output_iterators.len();
        }

        if self.cursor == 0 {
            match self.input_iterator.next() {
                None => {
                    if self.output_iterators.is_empty() {
                        return None;
                    }
                }
                Some(it) => self.output_iterators.push(it),
            }
        }

        match self.output_iterators[self.cursor].next() {
            Some(x) => {
                self.cursor += 1;
                Some(x)
            }
            None => {
                self.output_iterators.swap_remove(self.cursor);
                self.next()
            }
        }
    }
}

fn interleave_flatten<I>(iter: I) -> InterleaveFlatten<I>
where
    I: Iterator,
    I::Item: Iterator,
{
    InterleaveFlatten {
        input_iterator: iter,
        output_iterators: vec![],
        cursor: 0,
    }
}

pub fn eq<'a>(
    u: impl Into<Value>,
    v: impl Into<Value>,
) -> impl Fn(Substitution<'a>) -> iter::Take<iter::Once<Substitution<'a>>> {
    let u = u.into();
    let v = v.into();
    move |s| match s.unify(&u, &v) {
        Some(s) => iter::once(s).take(1),
        None => iter::once(Substitution::empty()).take(0),
    }
}

pub fn succeed<'a>() -> impl Fn(Substitution<'a>) -> iter::Once<Substitution<'a>> {
    |s| iter::once(s)
}

pub fn fail() -> impl Fn(StatSubs) -> iter::Empty<Substitution<'static>> {
    |_| iter::empty()
}

pub fn disj2<I, J>(
    g1: impl Fn(StatSubs) -> I,
    g2: impl Fn(StatSubs) -> J,
) -> impl Fn(StatSubs) -> Interleave<I, J>
where
    I: Iterator,
    J: Iterator<Item = I::Item>,
{
    move |s| interleave(g1(s.clone()), g2(s))
}

pub fn nevero() -> impl Fn(StatSubs) -> InterleaveFlatten<iter::Repeat<iter::Empty<StatSubs>>> {
    |_| interleave_flatten(iter::repeat(iter::empty()))
}

pub fn alwayso() -> impl Fn(StatSubs) -> iter::Repeat<StatSubs> {
    |s| iter::repeat(s)
}

pub fn conj2<I, J>(
    g1: impl Fn(StatSubs) -> I,
    g2: impl 'static + Fn(StatSubs) -> J,
) -> impl Fn(StatSubs) -> Box<dyn Iterator<Item = StatSubs>>
where
    I: 'static + Iterator<Item = StatSubs>,
    J: 'static + Iterator<Item = StatSubs>,
{
    let g2 = Rc::new(g2);
    move |s| {
        Box::new(interleave_flatten(g1(s).map({
            let g2 = g2.clone();
            move |sg| g2(sg)
        })))
    }
}

pub fn reify(v: Value) -> impl Fn(StatSubs) -> Value {
    move |s| {
        let v = s.walk_star(&v);
        let r = Substitution::empty().reify_s(&v);
        r.walk_star(&v)
    }
}

pub fn run_goal<I: Iterator<Item = StatSubs>>(
    n: Option<usize>,
    g: impl Fn(StatSubs) -> I,
) -> iter::Take<I> {
    match n {
        Some(n) => g(Substitution::empty()).take(n),
        None => g(Substitution::empty()).take(usize::max_value()), // todo: usize::max is not really an infinite iterator...
    }
}

pub fn ifte<I, J, K>(
    g1: impl Fn(StatSubs) -> I,
    g2: impl 'static + Fn(StatSubs) -> J,
    g3: impl Fn(StatSubs) -> K,
) -> impl Fn(StatSubs) -> Box<dyn Iterator<Item = StatSubs>>
where
    I: 'static + Iterator<Item = StatSubs>,
    J: 'static + Iterator<Item = StatSubs>,
    K: 'static + Iterator<Item = StatSubs>,
{
    let g2 = Rc::new(g2);
    move |s| {
        let mut s_inf = g1(s.clone()).peekable();
        if s_inf.peek().is_none() {
            Box::new(g3(s))
        } else {
            Box::new(interleave_flatten(s_inf.map({
                let g2 = g2.clone();
                move |sg| g2(sg)
            })))
        }
    }
}

pub fn once<I: Iterator<Item = StatSubs>>(
    g: impl Fn(StatSubs) -> I,
) -> impl Fn(StatSubs) -> iter::Take<I> {
    move |s| g(s).take(1)
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Value::Var(var) => write!(f, "{:?}", var),
            Value::Val(val) => write!(f, "{:?}", val),
            Value::RV(n) => write!(f, "_{}", n),
        }
    }
}

impl std::fmt::Debug for Var {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::fmt::Debug for Substitution<'_> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{{")?;
        let mut iter = self.subs.iter();
        if let Some((var, val)) = iter.next() {
            write!(f, "{:?}: {:?}", var, val)?;
        }
        for (var, val) in iter {
            write!(f, ", {:?}: {:?}", var, val)?;
        }
        write!(f, "}}")
    }
}

#[macro_export]
macro_rules! disj {
    () => { fail() };
    ($g:expr) => { $g };
    ($g0:expr, $($g:expr),*) => { disj2($g0, disj!($($g),*))}
}

#[macro_export]
macro_rules! conj {
    () => { succeed() };
    ($g:expr) => { $g };
    ($g0:expr, $($g:expr),*) => { conj2($g0, conj!($($g),*))}
}

#[macro_export]
macro_rules! defrel {
    ($name:ident($($args:ident),*) { $($g:expr);* }) => {
        fn $name($($args: impl 'static + Into<Value>),*) -> impl Fn(StatSubs) -> Box<dyn Iterator<Item = StatSubs>> {
            $(
                let $args = $args.into();
            )*
            move |s| {
                $(
                    let $args = $args.clone();
                )*
                Box::from(conj!($($g),*)(s))
            }
        }
    }
}

#[macro_export]
macro_rules! run {
    (*, ($($x:ident),*) $body:tt) => {
        run!(@ None, ($($x),*) $body)
    };

    (*, $q:ident { $($g:expr;)* }) => {
        run!(@ None, $q { $($g;)* })
    };

    ($n:expr, ($($x:ident),*) $body:tt) => {
        run!(@ Some($n), ($($x),*) $body)
    };

    ($n:expr, $q:ident { $($g:expr;)* }) => {
        run!(@ Some($n), $q { $($g;)* })
    };

    (@ $n:expr, ($($x:ident),*) { $($g:expr;)* }) => {
        run!(@ $n, q {
            fresh!($($x),* {
                eq(vec![$(Value::Var($x.clone())),*], q);
                $($g;)*
            });
        })
    };

    (@ $n:expr, $q:ident { $($g:expr;)* }) => {{
        let $q = Var::new(stringify!($q));
        let var = Value::Var($q.clone());
        run_goal($n, conj!($($g),*)).map(reify(var))
    }};
}

#[macro_export]
macro_rules! fresh {
    ($($x:ident),* {$($g:expr;)*}) => {{
        $( let $x = Var::new(stringify!($x)); )*
        conj!($($g),*)
    }}
}

#[macro_export]
macro_rules! conde {
    ( $($($g:expr),*);*) => {
        disj!($(conj!( $($g),*)),*)
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
        s.walk(&Value::Var(v.clone())).clone()
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
    fn test_interleave_flatten() {
        struct InfiniteIterators {
            n: usize,
        }

        impl Iterator for InfiniteIterators {
            type Item = iter::StepBy<std::ops::RangeFrom<usize>>;
            fn next(&mut self) -> Option<Self::Item> {
                self.n += 1;
                let inc = usize::pow(10, self.n as u32);
                Some((inc..).step_by(inc))
            }
        }

        let iter = interleave_flatten(InfiniteIterators { n: 0 });
        assert_eq!(
            iter.take(10).collect::<Vec<_>>(),
            vec![10, 20, 100, 30, 200, 1_000, 40, 300, 2_000, 10_000]
        )
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

        assert!(Substitution::empty().occurs(&x, &Value::Var(x.clone())));
        assert!(substitution! {y: x}.occurs(&x, &Value::Val(Rc::new(vec![Value::Var(y.clone())]))));
        assert!(!Substitution::empty()
            .extend(x.clone(), Value::Val(Rc::new(vec![Value::Var(x.clone())]))));
        assert!(!substitution! {y: x}
            .extend(x.clone(), Value::Val(Rc::new(vec![Value::Var(y.clone())]))));

        assert_eq!(
            Substitution::empty().unify(
                &Value::Val(Rc::new(Some(Value::Var(x.clone())))),
                &Value::Val(Rc::new(Some(Value::Var(y.clone()))))
            ),
            Some(substitution!(x: y)),
        );

        assert_eq!(
            Substitution::empty()
                .unify(
                    &Value::Val(Rc::new(Some(Value::Var(x.clone())))),
                    &Value::Val(Rc::new(Some(Value::Var(y.clone()))))
                )
                .unwrap()
                .unify(
                    &Value::Val(Rc::new(Some(Value::Var(x.clone())))),
                    &Value::Val(Rc::new(Some(Value::Val(Rc::new(42)))))
                ),
            Some(substitution!(x: y, y: 42)),
        );

        assert_eq!(
            eq(&x, Value::Var(u.clone()))(Substitution::empty()).collect::<Vec<_>>(),
            vec![substitution!(x: u)]
        );
        assert_eq!(
            eq(&x, 42)(Substitution::empty()).collect::<Vec<_>>(),
            vec![substitution!(x: 42)]
        );
        assert_eq!(
            eq(42, 42)(Substitution::empty()).collect::<Vec<_>>(),
            vec![substitution!()]
        );
        assert_eq!(
            eq(42, 123)(Substitution::empty()).collect::<Vec<_>>(),
            vec![]
        );

        assert_eq!(fail()(Substitution::empty()).collect::<Vec<_>>(), vec![]);
        assert_eq!(
            eq(true, false)(Substitution::empty()).collect::<Vec<_>>(),
            vec![]
        );
        assert_eq!(
            eq(&x, &y)(Substitution::empty()).collect::<Vec<_>>(),
            vec![substitution! {x: y}]
        );

        assert_eq!(
            disj2(eq("olive", &x), eq("oil", &x))(Substitution::empty()).collect::<Vec<_>>(),
            vec![substitution! {x: "olive"}, substitution! {x: "oil"}]
        );

        // no value - stack overflow
        //assert_eq!(nevero()(Substitution::empty()).take(1).collect::<Vec<_>>(), vec![]);

        assert_eq!(
            alwayso()(Substitution::empty()).take(3).collect::<Vec<_>>(),
            vec![Substitution::empty(); 3]
        );

        assert_eq!(
            conj2(eq("olive", &x), eq("oil", x.clone()))(Substitution::empty()).collect::<Vec<_>>(),
            vec![]
        );

        assert_eq!(
            conj2(eq("olive", &x), eq(y.clone(), x.clone()))(Substitution::empty())
                .collect::<Vec<_>>(),
            vec![substitution! {y: "olive", x: "olive"}]
        );

        assert_eq!(
            substitution! {x: "b", z: y, w: vec![Value::from(&x), "e".into(), (&z).into()]}
                .walk_star(&w.clone().into()),
            Value::from(vec![Value::from("b"), "e".into(), y.clone().into()])
        );

        let a1 = Value::from(vec![
            Value::from(&u),
            Value::from(&w),
            Value::from(&y),
            Value::from(&z),
            Value::from(Some(Value::from(vec![Value::from("ice"), Value::from(&z)]))),
        ]);
        let a2 = Value::from("corn");
        let a3 = Value::from(vec![Value::from(&v), Value::from(&u)]);
        let s = substitution! {x: a1, y: a2, w: a3};
        //println!("{:?}", reify((&x).into())(s));
        assert_eq!(
            reify((&x).into())(s),
            Value::from(vec![
                Value::RV(0),
                Value::from(vec![Value::RV(1), Value::RV(0)]),
                Value::from("corn"),
                Value::RV(2),
                Value::from(vec![Value::from("ice"), Value::RV(2)])
            ])
        );

        assert_eq!(
            run_goal(Some(5), disj2(eq("olive", &x), eq("oil", &x)))
                .map(|s| reify((&x).into())(s))
                .collect::<Vec<_>>(),
            vec![Value::from("olive"), Value::from("oil")],
        );

        assert_eq!(
            ifte(succeed(), eq(false, y.clone()), eq(true, y.clone()))(Substitution::empty())
                .collect::<Vec<_>>(),
            vec![substitution!(y: false)]
        );

        assert_eq!(
            ifte(fail(), eq(false, y.clone()), eq(true, y.clone()))(Substitution::empty())
                .collect::<Vec<_>>(),
            vec![substitution!(y: true)]
        );

        assert_eq!(
            disj!(eq("virgin", &x), eq("olive", &x), eq("oil", &x))(Substitution::empty())
                .collect::<Vec<_>>(),
            vec![
                substitution! {x: "virgin"},
                substitution! {x: "olive"},
                substitution! {x: "oil"},
            ]
        );

        defrel! {
            teacup(t) {
                disj!(eq("tea", t.clone()), eq("cup", t))
            }
        }

        assert_eq!(
            teacup(x.clone())(Substitution::empty()).collect::<Vec<_>>(),
            vec![substitution!(x: "tea"), substitution!(x: "cup")]
        );

        assert_eq!(
            format!(
                "{:?}",
                fresh!(x, y { eq(x, y); })(Substitution::empty()).collect::<Vec<_>>()
            ),
            "[{x: y}]"
        );

        assert_eq!(run!(1, x {}).collect::<Vec<_>>(), vec![Value::RV(0)]);
        assert_eq!(
            run!(1, x { eq(x, 42); }).collect::<Vec<_>>(),
            vec![Value::new(42)]
        );
        assert_eq!(
            run!(1, (x, y) { }).collect::<Vec<_>>(),
            vec![Value::new(vec![Value::RV(0), Value::RV(1)])]
        );
        assert_eq!(
            run!(1, (x, y) { eq(x, 42); }).collect::<Vec<_>>(),
            vec![Value::new(vec![Value::new(42), Value::RV(0)])]
        );

        defrel! {
            conso(a, d, p) {
                eq(vec![a, d], p)
            }
        }

        assert_eq!(
            run!(*, x { conso(1, 2, x); }).collect::<Vec<_>>(),
            vec![Value::new(vec![Value::new(1), Value::new(2)])]
        );
        assert_eq!(
            run!(*, x { conso(1, x, vec![Value::new(1), Value::new(2)]); }).collect::<Vec<_>>(),
            vec![Value::new(2)]
        );
        assert_eq!(
            run!(*, x { conso(x, 2, vec![Value::new(1), Value::new(2)]); }).collect::<Vec<_>>(),
            vec![Value::new(1)]
        );
        assert_eq!(
            run!(*, x { conso(x.clone(), x, vec![Value::new(1), Value::new(2)]); })
                .collect::<Vec<_>>(),
            Vec::<Value>::new()
        );
        assert_eq!(
            run!(*, x { conso(x.clone(), x, vec![Value::new(3), Value::new(3)]); })
                .collect::<Vec<_>>(),
            vec![Value::new(3)]
        );

        assert_eq!(
            run!(5, q {
                eq(q, "onion");
            })
            .collect::<Vec<_>>(),
            vec![Value::new("onion")]
        );

        assert_eq!(
            run!(5, q {
                eq(q, "onion");
                alwayso();
            })
            .collect::<Vec<_>>(),
            vec![Value::new("onion"); 5]
        );
    }
}
