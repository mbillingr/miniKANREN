use crate::core::substitution::Substitution;
use crate::core::value::Value;
use crate::logic_variable::{ReifiedVar, Var};
use std::any::Any;
use std::sync::Arc;

pub trait Structure: std::any::Any + std::fmt::Debug {
    fn occurs<'s>(&self, x: &Var, s: &Substitution<'s>) -> bool;
    fn unify<'s>(&self, v: &Value, s: Substitution<'s>) -> Option<Substitution<'s>>;
    fn walk_star(self: Arc<Self>, s: &Substitution<'_>) -> Value;
    fn reify_s<'s>(&self, s: Substitution<'s>) -> Substitution<'s>;

    fn as_any(&self) -> &dyn Any;

    fn eqv(&self, other: &Value) -> bool;
}

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
