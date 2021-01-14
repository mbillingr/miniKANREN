//! Logic-compatible numbers.
//!
//! The current implementation uses Peano numbers, which is
//! horribly inefficient but very simple. Will serve as a
//! baseline for future more efficient implementations.

use crate::core::goal::Goal;
use crate::core::logic_variable::Var;
use crate::core::structure::Structure;
use crate::core::substitution::Substitution;
use crate::core::value::Value;
use crate::goals::primitive::eq;
use crate::goals::StatSubs;
use std::any::Any;
use std::fmt::{self, Debug, Formatter};
use std::sync::Arc;

/// Create a numeric value
pub fn num(n: u64) -> Value {
    if n == 0 {
        return Value::new(Zero);
    }

    return Value::new(OneMore(num(n - 1)));
}

/// Creates a goal that succeeds if n is 0.
pub fn zero(n: impl Into<Value>) -> impl Goal<StatSubs> {
    eq(Zero, n.into())
}

/// Creates a goal that succeeds if n is 1.
pub fn oneo(n: impl Into<Value>) -> impl Goal<StatSubs> {
    eq(num(1), n.into())
}

defrel! {
    /// Creates a goal that succeeds if b is one more than a.
    pub inco(a, b) {
        eq(OneMore(a), b)
    }
}

defrel! {
    /// Creates a goal that succeeds if n is a positive number greater than zero.
    pub poso(n) {
        fresh!{ (x),
            inco(x, n)
        }
    }
}

defrel! {
    /// Creates a goal that succeeds if n is a positive number greater than one.
    pub gt1o(n) {
        fresh!{ (x),
            poso(x),
            inco(x, n)
        }
    }
}

defrel! {
    /// Creates a goal that succeeds if n is a natural number.
    pub numbero(n) {
        conde!{
            zero(n.clone());
            fresh!{ (a),
                inco(a.clone(), n),
                numbero(a),
            };
        }
    }
}

defrel! {
    /// Creates a goal that succeeds if a + b equals c
    pub addo(a, b, c) {
        conde!{
            // 0 + b == b
            eq(a.clone(), Zero),
            eq(b.clone(), c.clone());

            // (a>0) + b == c  =>  b + (a-1) == (c-1)
            fresh!{ (a0, c0),
                inco(a0, a),
                inco(c0, c),
                addo(b, a0, c0),
            };
        }
    }
}

defrel! {
    /// Creates a goal that succeeds if a * b equals c
    pub mulo(a, b, c) {
        fresh!{ (a0, c0),
            conde!{
                // 0 * b == 0
                zero(a.clone()),
                zero(c.clone());

                // (a>0) * 0 == 0
                poso(a.clone()),
                zero(b.clone()),
                zero(c.clone());

                // 1 * (b>0) == b
                oneo(a.clone()),
                poso(b.clone()),
                eq(b.clone(), c.clone());

                // (a>1) * 1 == a
                gt1o(a.clone()),
                oneo(b.clone()),
                eq(a.clone(), c.clone());

                // (a>1) * (b>1) == c  =>  (a-1) * b == c-b
                gt1o(a.clone()),
                gt1o(b.clone()),
                inco(a0, a.clone()),
                addo(c0, b.clone(), c.clone()),
                mulo(a0, b, c0);
            }
        }
    }
}

use Peano::*;

#[derive(Clone, PartialEq)]
enum Peano {
    Zero,
    OneMore(Value),
}

impl Debug for Peano {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self.to_int(0) {
            Ok(n) => n.fmt(f),
            Err((n, b)) => write!(f, "{:?}+{}", b, n),
        }
    }
}

impl Peano {
    fn to_int(&self, acc: u64) -> Result<u64, (u64, &Value)> {
        match self {
            Zero => Ok(acc),
            OneMore(base) => match base.downcast_ref::<Self>() {
                Some(n) => n.to_int(acc + 1),
                None => Err((acc + 1, base)),
            },
        }
    }
}

impl Structure for Peano {
    fn occurs<'s>(&self, x: &Var, s: &Substitution<'s>) -> bool {
        match self {
            OneMore(n) => s.occurs(x, n),
            Zero => false,
        }
    }

    fn unify<'s>(&self, v: &Value, s: Substitution<'s>) -> Option<Substitution<'s>> {
        let other = v.downcast_ref::<Self>()?;
        match (self, other) {
            (OneMore(su), OneMore(sv)) => s.unify(su, sv),
            (Zero, Zero) => Some(s),
            _ => None,
        }
    }

    fn walk_star(self: Arc<Self>, s: &Substitution<'_>) -> Value {
        match &*self {
            Zero => Value::from_arc(self),
            OneMore(v) => Value::new(OneMore(s.walk_star(v))),
        }
    }

    fn reify_s<'s>(&self, s: Substitution<'s>) -> Substitution<'s> {
        match &*self {
            Zero => s,
            OneMore(v) => s.reify_s(v),
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{fails, has_unique_solution, succeeds};

    #[test]
    fn zero_succeeds_for_zero_valued_number() {
        succeeds(zero(num(0)));
    }

    #[test]
    fn zero_fails_for_nonzero_numbers() {
        fails(zero(num(1)));
    }

    #[test]
    fn inco_succeeds_for_incremented_numbers() {
        let a = num(1);
        let b = num(2);
        succeeds(inco(a, b));
    }

    #[test]
    fn inco_fails_for_equal_numbers() {
        let a = num(1);
        let b = num(1);
        fails(inco(a, b));
    }

    #[test]
    fn inco_fails_for_too_large_increment() {
        let a = num(1);
        let b = num(3);
        fails(inco(a, b));
    }

    #[test]
    fn inco_fails_if_right_side_is_zero() {
        let a = Var::new("a");
        fails(inco(a, num(0)));
    }

    #[test]
    fn numbero_succeeds_for_arbitrary_numbers() {
        succeeds(numbero(num(0)));
        succeeds(numbero(num(1)));
        succeeds(numbero(num(2)));
        succeeds(numbero(num(10)));
        succeeds(numbero(num(123)));
    }

    #[test]
    fn numbero_fails_for_non_numeric_values() {
        fails(numbero(()));
        fails(numbero("0"));
    }

    #[test]
    fn numbero_generates_numbers() {
        assert_eq!(
            run!(5, q, numbero(q)).into_vec(),
            vec![0, 1, 2, 3, 4].into_iter().map(num).collect::<Vec<_>>()
        );
    }

    #[test]
    fn addo_succeeds_for_all_zeros() {
        succeeds(addo(num(0), num(0), num(0)))
    }

    #[test]
    fn addo_obeys_additive_identity() {
        succeeds(addo(num(0), num(7), num(7)));
        succeeds(addo(num(7), num(0), num(7)));
    }

    #[test]
    fn addo_succeeds_for_correct_addition() {
        succeeds(addo(num(2), num(3), num(5)));
    }

    #[test]
    fn addo_fails_for_wrong_addition() {
        fails(addo(num(1), num(1), num(3)));
    }

    #[test]
    fn addo_computes_sum_of_two_values() {
        has_unique_solution(run!(q, addo(num(1), num(2), q)), num(3));
    }

    #[test]
    fn addo_computes_differences() {
        has_unique_solution(run!(q, addo(num(1), q, num(3))), num(2));

        has_unique_solution(run!(q, addo(q, num(2), num(3))), num(1));
    }

    #[test]
    fn addo_generates_all_number_pairs_that_sum_to_a_value() {
        assert_eq!(
            run!(*, (a, b), addo(a, b, num(3))).into_vec(),
            vec![
                list![num(0), num(3)],
                list![num(3), num(0)],
                list![num(1), num(2)],
                list![num(2), num(1)],
            ]
        );
    }

    #[test]
    fn addo_generates_constrained_pairs_of_numbers() {
        assert_eq!(
            run!(3, (b, c), addo(num(3), b, c)).into_vec(),
            vec![
                list![num(0), num(3)],
                list![num(1), num(4)],
                list![num(2), num(5)]
            ]
        )
    }

    #[test]
    fn addo_fails_if_it_would_need_a_negative_number_as_first_argument() {
        let x = Var::new("x");
        fails(addo(x, num(2), num(1)));
    }

    #[test]
    fn addo_fails_if_it_would_need_a_negative_number_as_second_argument() {
        let x = Var::new("x");
        fails(addo(num(2), x, num(1)));
    }

    #[test]
    fn mulo_succeeds_for_trivial_cases() {
        succeeds(mulo(num(0), num(0), num(0)));
        succeeds(mulo(num(2), num(0), num(0)));
        succeeds(mulo(num(0), num(2), num(0)));
        succeeds(mulo(num(1), num(1), num(1)));
        succeeds(mulo(num(1), num(2), num(2)));
        succeeds(mulo(num(2), num(1), num(2)));
    }

    #[test]
    fn mulo_succeeds_for_correct_multiplication() {
        succeeds(mulo(num(2), num(3), num(6)));
    }

    #[test]
    fn mulo_fails_for_wrong_multiplication() {
        fails(mulo(num(4), num(5), num(3)));
    }

    #[test]
    fn mulo_computes_product_of_two_values() {
        has_unique_solution(run!(q, mulo(num(2), num(3), q)), num(6));
    }

    #[test]
    fn mulo_combinations() {
        for solution in run!((a, b, c), mulo(a, b, c)).take(20) {
            println!("{:?}", solution);
        }
    }

    #[test]
    fn mulo_computes_quotients() {
        has_unique_solution(run!(q, mulo(num(3), q, num(12))), num(4));
        has_unique_solution(run!(q, mulo(q, num(4), num(12))), num(3));
    }

    #[test]
    fn mulo_fails_for_non_integer_quotients() {
        fails(fresh! {(q), mulo(num(3), q, num(7))});
    }

    #[test]
    fn mulo_generates_all_number_pairs_that_multiply_to_a_value() {
        assert_eq!(
            run!(*, (a, b), mulo(a, b, num(6))).into_vec(),
            vec![
                // Actually, the order does not matter but this
                // is what we get with the current implementation.
                list![num(1), num(6)],
                list![num(6), num(1)],
                list![num(2), num(3)],
                list![num(3), num(2)],
            ]
        );
    }

    #[test]
    fn mulo_generates_constrained_pairs_of_numbers() {
        assert_eq!(
            run!(4, (a, c), mulo(a, num(3), c)).into_vec(),
            vec![
                list![num(0), num(0)],
                list![num(1), num(3)],
                list![num(2), num(6)],
                list![num(3), num(9)]
            ]
        )
    }
}
