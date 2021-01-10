use super::{logic_variables::Var, object::Object};
use std::collections::HashMap;

pub struct State {}

impl State {
    pub fn new() -> Self {
        State {}
    }

    pub fn is_empty(&self) -> bool {
        true
    }
}

#[derive(Debug)]
pub struct Substitution {
    associations: HashMap<Var, Object>,
}

impl Substitution {
    pub fn new() -> Self {
        Substitution {
            associations: HashMap::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.associations.is_empty()
    }

    pub fn extend(&mut self, x: impl Into<Var>, v: impl Into<Object>) -> bool {
        let x = x.into();
        let v = v.into();
        if self.occurs(&x, &v) {
            false
        } else {
            self.associations.insert(x, v);
            true
        }
    }

    pub fn occurs(&self, x: &Var, t: &Object) -> bool {
        if let Some(p) = t.try_as_pair() {
            self.occurs(x, self.walk(p.0)) || self.occurs(x, self.walk(p.1))
        } else if let Some(v) = t.try_as_var() {
            x == &v
        } else {
            false
        }
    }

    pub fn walk<'a>(&'a self, t: &'a Object) -> &'a Object {
        t.try_as_var()
            .and_then(|t| self.associations.get(&t))
            .map(|v| self.walk(v))
            .unwrap_or(t)
    }

    pub fn unify(&mut self, u: &Object, v: &Object) -> bool {
        if let Some(u) = u.try_as_var() {
            if let Some(v) = v.try_as_var() {
                if u == v {
                    return true;
                }
            }
        }

        if let Some(u) = u.try_as_var() {
            return self.extend(u, v.clone());
        }

        if let Some(v) = v.try_as_var() {
            return self.extend(v, u.clone());
        }

        if let Some((caru, cdru)) = u.try_as_pair() {
            if let Some((carv, cdrv)) = v.try_as_pair() {
                return self.unify(caru, carv) && self.unify(cdru, cdrv);
            }
        }

        u.is_equal(&**v)
    }
}

#[cfg(test)]
mod tests {
    use super::super::{
        logic_variables::MaybeVar,
        object::{MaybePair, Value},
    };
    use super::*;

    pub fn obj(val: impl Value) -> Object {
        Object::new(val)
    }
    pub fn cons(car: impl Value, cdr: impl Value) -> Object {
        obj((obj(car), obj(cdr)))
    }

    impl Value for i64 {
        fn is_equal(&self, other: &dyn Value) -> bool {
            other
                .downcast_ref::<Self>()
                .map(|rhs| self == rhs)
                .unwrap_or(false)
        }
    }
    impl MaybeVar for i64 {
        fn try_as_var(&self) -> Option<Var> {
            None
        }
    }
    impl MaybePair for i64 {
        fn try_as_pair(&self) -> Option<(&Object, &Object)> {
            None
        }
    }

    #[test]
    fn new_states_are_empty() {
        let state = State::new();
        assert!(state.is_empty())
    }

    #[test]
    fn new_substitutions_are_empty() {
        let sub = Substitution::new();
        assert!(sub.is_empty())
    }

    #[test]
    fn can_extend_substitution() {
        let mut sub = Substitution::new();
        assert!(sub.extend("x", obj(1)));
        assert!(!sub.is_empty());
    }

    #[test]
    fn nothing_occurs_in_empty_substitution() {
        let sub = Substitution::new();
        let var = Var::fresh("x");
        assert!(!sub.occurs(&var, &obj(1)))
    }

    #[test]
    fn same_variable_occurs_even_in_empty_substitution() {
        let sub = Substitution::new();
        let var = Var::fresh("x");
        assert!(sub.occurs(&var, &obj(var)))
    }

    #[test]
    fn same_variable_in_car_occurs_even_in_empty_substitution() {
        let sub = Substitution::new();
        let var = Var::fresh("x");
        let pair = cons(var, 0);
        assert!(sub.occurs(&var, &pair))
    }

    #[test]
    fn same_variable_in_cdr_occurs_even_in_empty_substitution() {
        let sub = Substitution::new();
        let var = Var::fresh("x");
        let pair = cons(0, var);
        assert!(sub.occurs(&var, &pair))
    }

    #[test]
    fn cannnot_create_obvious_substitution_cycles() {
        let mut sub = Substitution::new();
        let var = Var::fresh("x");
        assert!(!sub.extend(var, obj(var)));
        assert!(sub.is_empty())
    }

    #[test]
    fn cannnot_create_complex_substitution_cycles() {
        let mut sub = Substitution::new();
        let x = Var::fresh("x");
        let y = Var::fresh("y");
        sub.extend(x, obj(y));
        assert!(!sub.extend(y, cons(x, 0)));
        assert!(!sub.extend(y, cons(0, x)));
    }

    #[test]
    fn cannot_unify_different_values() {
        let mut sub = Substitution::new();
        assert!(!sub.unify(&obj(1), &obj(2)));
        assert!(sub.is_empty());
    }

    #[test]
    fn unifying_equal_values_does_not_modify_substitution() {
        let mut sub = Substitution::new();
        assert!(sub.unify(&obj(1), &obj(1)));
        assert!(sub.is_empty());
    }

    #[test]
    fn unifying_same_variable_does_not_modify_substitution() {
        let mut sub = Substitution::new();
        let var = Var::fresh("x");
        assert!(sub.unify(&obj(var), &obj(var)));
        assert!(sub.is_empty());
    }

    #[test]
    fn unifying_variable_associates_value() {
        let mut sub = Substitution::new();
        let var = Var::fresh("x");

        assert!(sub.unify(&obj(var), &obj(1)));
        assert_eq!(sub.walk(&obj(var)), &obj(1));

        let mut sub = Substitution::new();
        assert!(sub.unify(&obj(1), &obj(var)));
        assert_eq!(sub.walk(&obj(var)), &obj(1));
    }

    #[test]
    fn unifying_pairs_unifies_all_contained_variables() {
        let mut sub = Substitution::new();
        let x = Var::fresh("x");
        let y = Var::fresh("y");
        assert!(sub.unify(&cons(x, 1), &cons(2, y)));
        assert_eq!(sub.walk(&obj(x)), &obj(2));
        assert_eq!(sub.walk(&obj(y)), &obj(1));
    }
}
