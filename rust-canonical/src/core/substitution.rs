use super::value::Value;
use crate::core::logic_variable::Var;
use crate::core::structure::Structure;
use std::borrow::Cow;
use std::collections::HashMap;
use std::fmt::Formatter;

#[derive(Clone, PartialEq)]
pub struct Substitution<'s> {
    pub(crate) subs: Cow<'s, HashMap<Var, Value>>,
}

impl Default for Substitution<'_> {
    fn default() -> Self {
        Self::empty()
    }
}

impl<'s> Substitution<'s> {
    pub fn empty() -> Self {
        Substitution {
            subs: Cow::Owned(HashMap::new()),
        }
    }

    pub fn n_subs(&self) -> usize {
        self.subs.len()
    }

    pub(super) fn walk<'a>(&'a self, v: &'a Value) -> &'a Value {
        if let Some(var) = v.try_as_var() {
            if let Some(next) = self.subs.get(&var) {
                return self.walk(next);
            }
        }
        v
    }

    pub(super) fn walk_star(&self, v: &Value) -> Value {
        self.walk(v).walk_star(self)
    }

    pub fn extend(mut self, x: Var, v: Value) -> Option<Self> {
        if self.occurs(&x, &v) {
            None
        } else {
            self.subs.to_mut().insert(x, v);
            Some(self)
        }
    }

    pub fn occurs(&self, x: &Var, v: &Value) -> bool {
        let v = self.walk(v);
        v.occurs(x, self)
    }

    pub fn unify(self, u: &Value, v: &Value) -> Option<Self> {
        let u = self.walk(u);
        let v = self.walk(v);

        if let Some(u) = u.try_as_var() {
            return u.unify(&v.clone(), self);
        }

        if let Some(v) = v.try_as_var() {
            return v.unify(&u.clone(), self);
        }

        let u = u.clone();
        let v = v.clone();
        u.unify(&v, self)
    }

    pub(super) fn reify_s(self, v: &Value) -> Self {
        self.walk(v).clone().reify_s(self)
    }

    pub fn reify(&self, v: &Value) -> Value {
        let v = self.walk_star(v);
        let r = Substitution::empty().reify_s(&v);
        r.walk_star(&v)
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

#[cfg(test)]
mod tests {
    use super::*;

    fn walk(v: Var, s: &Substitution) -> Value {
        s.walk(&Value::var(v)).clone()
    }

    #[test]
    fn it_works() {
        let v = Var::new("v");
        let w = Var::new("w");
        let x = Var::new("x");
        let y = Var::new("y");
        let z = Var::new("z");

        assert_eq!(walk(z, &substitution! {z: "a", x: w, y: z}), "a");
        assert_eq!(walk(y, &substitution! {z: "a", x: w, y: z}), "a");
        assert_eq!(walk(x, &substitution! {z: "a", x: w, y: z}), w);
        assert_eq!(walk(x, &substitution! {x: y, v: x, w: x}), y);
        assert_eq!(walk(v, &substitution! {x: y, v: x, w: x}), y);
        assert_eq!(walk(w, &substitution! {x: y, v: x, w: x}), y);

        assert_eq!(
            substitution! {x: "b", z: y, w: vec![Value::from(x), "e".into(), (z).into()]}
                .walk_star(&w.clone().into()),
            Value::from(vec![Value::from("b"), "e".into(), y.clone().into()])
        );
    }

    #[test]
    fn unify_same_var_does_not_modify_substitution() {
        let var_as_val = Var::new("x").into();
        let sub = Substitution::empty().unify(&var_as_val, &var_as_val);
        assert_eq!(sub, Some(Substitution::empty()));
    }

    #[test]
    fn unify_two_vars_extends_substitution() {
        let x = Var::new("x");
        let y = Var::new("y");
        let sub = Substitution::empty().unify(&x.into(), &y.into()).unwrap();
        let expected = Substitution::empty().extend(x, y.into()).unwrap();
        assert_eq!(sub, expected);
    }

    #[test]
    fn unify_value_with_var_extends_substitution() {
        let x = Var::new("x");
        let v = Value::new(0);
        let sub = Substitution::empty().unify(&v, &x.into()).unwrap();
        let expected = Substitution::empty().extend(x, v).unwrap();
        assert_eq!(sub, expected);
    }

    #[test]
    fn unify_same_values_does_not_modify_substitution() {
        let sub = Substitution::empty().unify(&Value::new(42), &Value::new(42));
        assert_eq!(sub, Some(Substitution::empty()));
    }

    #[test]
    fn unify_different_values_fails() {
        let sub = Substitution::empty().unify(&Value::new(1), &Value::new(2));
        assert_eq!(sub, None);
    }
}
