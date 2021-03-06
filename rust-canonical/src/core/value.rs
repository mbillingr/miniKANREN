//! Dynamic values
use crate::core::logic_variable::{ReifiedVar, Var};
use crate::core::pair::Pair;
use crate::core::structure::{Atomic, Structure};
use crate::core::substitution::Substitution;
use std::fmt::Formatter;
use std::sync::Arc;

/// Dynamic value
#[derive(Clone)]
pub struct Value(Arc<dyn Structure>);

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0) || self.0.eqv(&other)
    }
}

impl Value {
    /// Construct a new value
    pub fn new(val: impl Into<Value>) -> Self {
        val.into()
    }

    /// Construct pre-Arc'd value
    pub fn from_arc<T: Structure>(x: Arc<T>) -> Self {
        Value(x)
    }

    /// Construct a value representing a variable
    pub fn var(v: Var) -> Self {
        Value::new(v)
    }

    /// Construct a value representing a reified variable
    pub fn rv(i: usize) -> Self {
        Value::new(ReifiedVar(i))
    }

    /// Construct a pair from two values
    pub fn cons(car: impl Into<Value>, cdr: impl Into<Value>) -> Self {
        Value::new((car.into(), cdr.into()))
    }

    /// Cast to `Var` if the value represents a variable
    pub fn try_as_var(&self) -> Option<Var> {
        self.downcast_ref().copied()
    }

    /// Try to cast to concrete type
    pub fn downcast_ref<T: 'static>(&self) -> Option<&T> {
        self.0.as_any().downcast_ref()
    }

    pub(super) fn occurs<'s>(&self, x: &Var, s: &Substitution<'s>) -> bool {
        self.0.occurs(x, s)
    }

    pub(super) fn unify<'s>(&self, v: &Value, s: Substitution<'s>) -> Option<Substitution<'s>> {
        self.0.unify(v, s)
    }

    pub(super) fn walk_star(&self, s: &Substitution<'_>) -> Value {
        self.0.clone().walk_star(s)
    }

    pub(super) fn reify_s<'s>(&self, s: Substitution<'s>) -> Substitution<'s> {
        self.0.reify_s(s)
    }
}

impl PartialEq<Var> for Value {
    fn eq(&self, v: &Var) -> bool {
        self.0
            .as_any()
            .downcast_ref::<Var>()
            .map(|sv| sv == v)
            .unwrap_or(false)
    }
}

impl<T: Structure> From<T> for Value {
    fn from(v: T) -> Self {
        Value(Arc::new(v))
    }
}

impl<T: 'static + Atomic + PartialEq> PartialEq<T> for Value {
    fn eq(&self, other: &T) -> bool {
        self.0
            .as_any()
            .downcast_ref::<T>()
            .map(|x| x == other)
            .unwrap_or(false)
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl From<Vec<Value>> for Value {
    /// Convert `Vec<Value>` into a linked list of values.
    fn from(items: Vec<Value>) -> Self {
        let mut list = Value::from(());
        for v in items.into_iter().rev() {
            list = Value::cons(v, list);
        }
        list
    }
}

impl<A: Into<Value>, D: Into<Value>> From<(A, D)> for Value {
    fn from(pair: (A, D)) -> Self {
        let pair = (pair.0.into(), pair.1.into());
        Value::new(Pair::from(pair))
    }
}
