use super::logic_variables::{MaybeVar, Var};
use downcast_rs::{impl_downcast, Downcast};
use std::fmt::Debug;
use std::ops::Deref;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct Object(Arc<dyn Value>);

impl Object {
    pub fn new(val: impl Value) -> Self {
        Object(Arc::new(val))
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.0.is_equal(&*other.0)
    }
}

impl Deref for Object {
    type Target = dyn Value;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl<T: Value> From<T> for Object {
    fn from(val: T) -> Self {
        Object::new(val)
    }
}

pub trait Value: Debug + Downcast + MaybeVar + MaybePair {
    fn is_equal(&self, other: &dyn Value) -> bool;
}
impl_downcast!(Value);

impl Value for Var {
    fn is_equal(&self, _: &dyn Value) -> bool {
        unimplemented!()
    }
}

pub trait MaybePair {
    fn try_as_pair(&self) -> Option<(&Object, &Object)>;

    fn is_pair(&self) -> bool {
        self.try_as_pair().is_some()
    }
    fn car(&self) -> Option<&Object> {
        self.try_as_pair().map(|(a, _)| a)
    }
    fn cdr(&self) -> Option<&Object> {
        self.try_as_pair().map(|(_, d)| d)
    }
}

impl MaybePair for Var {
    fn try_as_pair(&self) -> Option<(&Object, &Object)> {
        None
    }
}

type Pair = (Object, Object);

impl Value for Pair {
    fn is_equal(&self, _other: &dyn Value) -> bool {
        unimplemented!()
    }
}

impl MaybePair for Pair {
    fn try_as_pair(&self) -> Option<(&Object, &Object)> {
        Some((&self.0, &self.1))
    }
}

impl MaybeVar for Pair {
    fn try_as_var(&self) -> Option<Var> {
        None
    }
}
