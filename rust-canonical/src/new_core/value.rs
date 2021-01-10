use super::logic_variables::{MaybeVar, Var};
use downcast_rs::{impl_downcast, Downcast};
use std::fmt::Debug;

pub trait Value: Debug + Downcast + MaybeVar + MaybePair {
    fn is_equal(&self, other: &dyn Value) -> bool;
}
impl_downcast!(Value);

impl PartialEq for dyn Value {
    fn eq(&self, other: &Self) -> bool {
        self.is_equal(other)
    }
}

impl Value for Var {
    fn is_equal(&self, _: &dyn Value) -> bool {
        unimplemented!()
    }
}

pub trait MaybePair {
    fn try_as_pair(&self) -> Option<(&dyn Value, &dyn Value)>;

    fn is_pair(&self) -> bool {
        self.try_as_pair().is_some()
    }
    fn car(&self) -> Option<&dyn Value> {
        self.try_as_pair().map(|(a, _)| a)
    }
    fn cdr(&self) -> Option<&dyn Value> {
        self.try_as_pair().map(|(_, d)| d)
    }
}

impl MaybePair for Var {
    fn try_as_pair(&self) -> Option<(&dyn Value, &dyn Value)> {
        None
    }
}

type Pair<T, U> = (T, U);

impl<T: Value, U: Value> Value for Pair<T, U> {
    fn is_equal(&self, other: &dyn Value) -> bool {
        unimplemented!()
    }
}

impl<T: Value, U: Value> MaybePair for Pair<T, U> {
    fn try_as_pair(&self) -> Option<(&dyn Value, &dyn Value)> {
        Some((&self.0, &self.1))
    }
}

impl<T: Value, U: Value> MaybeVar for Pair<T, U> {
    fn try_as_var(&self) -> Option<Var> {
        None
    }
}
