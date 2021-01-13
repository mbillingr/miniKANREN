//! Logic-aware data structures

use crate::core::logic_variable::{ReifiedVar, Var};
use crate::core::substitution::Substitution;
use crate::core::value::Value;
use std::any::Any;
use std::rc::Rc;
use std::sync::Arc;

/// Trait implemented by all logic-compatible types
pub trait Structure: std::any::Any + std::fmt::Debug {
    /// Returns `true` if `self` contains a variable that is equivalent
    /// to `x` when considering substitution `s`.
    fn occurs<'s>(&self, x: &Var, s: &Substitution<'s>) -> bool;

    /// Attempt to unify `self` with `Value` `v` under substitution `s`.
    fn unify<'s>(&self, v: &Value, s: Substitution<'s>) -> Option<Substitution<'s>>;

    /// Recursively replace any variables contained in `self` with
    /// their substituted values.
    fn walk_star(self: Arc<Self>, s: &Substitution<'_>) -> Value;

    /// Substitute all variables that remain fresh in `self` with reified variables.
    fn reify_s<'s>(&self, s: Substitution<'s>) -> Substitution<'s>;

    /// Cast to `Any` reference.
    fn as_any(&self) -> &dyn Any;

    /// Return `true` if `self` is equivalent to `other`.
    fn eqv(&self, other: &Value) -> bool;
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
        s.extend(*self, v)
    }

    fn walk_star(self: Arc<Self>, _: &Substitution<'_>) -> Value {
        Value::var(*self)
    }

    fn reify_s<'s>(&self, s: Substitution<'s>) -> Substitution<'s> {
        let reified = Value::new(ReifiedVar(s.n_subs()));
        s.extend(*self, reified).unwrap()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eqv(&self, other: &Value) -> bool {
        other
            .downcast_ref::<Self>()
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
            .downcast_ref::<Self>()
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
        let other = v.downcast_ref::<Self>()?;
        match (self, other) {
            (Some(su), Some(sv)) => s.unify(su, sv),
            (None, None) => Some(s),
            _ => None,
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
        other
            .downcast_ref::<Self>()
            .map(|v| v == self)
            .unwrap_or(false)
    }
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
            .downcast_ref::<Self>()
            .map(|o| o == self)
            .unwrap_or(false)
    }
}

/// Marker trait for types that contain no further values
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

impl<T: Atomic> Atomic for Option<T> {}
impl<T: Atomic> Atomic for Box<T> {}
impl<T: Atomic> Atomic for Arc<T> {}
impl<T: Atomic> Atomic for Rc<T> {}
