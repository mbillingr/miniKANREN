use crate::core::logic_variable::Var;
use crate::core::structure::Structure;
use crate::core::substitution::Substitution;
use crate::core::value::Value;
use std::any::Any;
use std::fmt::{Debug, Formatter, Result};
use std::sync::Arc;

#[derive(Clone, PartialEq)]
pub struct Pair {
    pub first: Value,
    pub second: Value,
}

impl Debug for Pair {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if !f.alternate() {
            write!(f, "(")?;
        }

        if self.second.downcast_ref::<()>().is_some() {
            write!(f, "{:?}", self.first)?;
        } else if let Some(next) = self.second.downcast_ref::<Pair>() {
            write!(f, "{:?} {:#?}", self.first, next)?;
        } else {
            write!(f, "{:?} . {:?}", self.first, self.second)?;
        }

        if !f.alternate() {
            write!(f, ")")?;
        }

        Ok(())
    }
}

impl Structure for Pair {
    fn occurs<'s>(&self, x: &Var, s: &Substitution<'s>) -> bool {
        s.occurs(x, &self.first) || s.occurs(x, &self.second)
    }

    fn unify<'s>(&self, v: &Value, s: Substitution<'s>) -> Option<Substitution<'s>> {
        let other = v.downcast_ref::<Self>()?;
        s.unify(&self.first, &other.first)
            .and_then(|s| s.unify(&self.second, &other.second))
    }

    fn walk_star(self: Arc<Self>, s: &Substitution<'_>) -> Value {
        (s.walk_star(&self.first), s.walk_star(&self.second)).into()
    }

    fn reify_s<'s>(&self, s: Substitution<'s>) -> Substitution<'s> {
        s.reify_s(&self.first).reify_s(&self.second)
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

impl From<(Value, Value)> for Pair {
    fn from(pair: (Value, Value)) -> Self {
        Pair {
            first: pair.0,
            second: pair.1,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pairs_print_two_values_in_parentheses_separated_by_dot() {
        let first = Value::new(());
        let second = Value::new(2);
        let pair = Pair { first, second };

        let repr = format!("{:?}", pair);

        assert_eq!(repr, "(() . 2)")
    }

    #[test]
    fn pairs_print_only_first_value_in_parentheses_if_second_is_nil() {
        let first = Value::new(1);
        let second = Value::new(());
        let pair = Pair { first, second };

        let repr = format!("{:?}", pair);

        assert_eq!(repr, "(1)")
    }

    #[test]
    fn pairs_print_as_list_if_second_is_pair() {
        let a = Value::new(1);
        let b = Value::new(2);
        let c = Value::new(3);
        let pair = Pair {
            first: a,
            second: Value::new(Pair {
                first: b,
                second: c,
            }),
        };

        let repr = format!("{:?}", pair);

        assert_eq!(repr, "(1 2 . 3)")
    }

    #[test]
    fn pairs_print_alternate_omits_parentheses() {
        let first = Value::new(());
        let second = Value::new(2);
        let pair = Pair { first, second };

        let repr = format!("{:#?}", pair);

        assert_eq!(repr, "() . 2")
    }
}
