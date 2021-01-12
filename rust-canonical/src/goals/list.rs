use crate::core::goal::Goal;
use crate::core::logic_variable::Var;
use crate::core::stream::Stream;
use crate::core::value::Value;
use crate::goals::primitive::{conj2, disj2, eq};
use crate::goals::StatSubs;
use crate::{conde, conj, defrel, disj, fresh};

/// Constructs a cons list.
#[macro_export]
macro_rules! list {
    () => { Value::new(()) };

    ($single:expr) => {
        Value::cons($single, ())
    };

    ($car:expr ; $cdr:expr) => {
        Value::cons($car, $cdr)
    };

    (($($first:tt)*), $($rest:tt)*) => {
        Value::cons(list![$($first)*], list![$($rest)*])
    };

    ($first:expr, $($rest:tt)*) => {
        Value::cons($first, list![$($rest)*])
    };

}

defrel! {
    /// Creates a goal that succeeds if p is equivalent to (a d).
    pub conso(a, d, p) {
        eq((a, d), p)
    }
}

defrel! {
    /// Creates a goal that succeeds if p is a pair and its first element is equivalent to a.
    pub caro(p, a) {
        fresh!{ (d),
            eq((a, d.into()), p)
        }
    }
}

defrel! {
    /// Creates a goal that succeeds if p is a pair and its second element is equivalent to d.
    pub cdro(p, d) {
        fresh!{ (a),
            eq((a.into(), d), p)
        }
    }
}

defrel! {
    /// Creates a goal that succeeds if l is a list that contains x.
    pub membero(x, l) {
        conde!(
            caro(l.clone(), x.clone());
            fresh!{ (d),
                cdro(l, d),
                membero(x, d),
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::*;

    #[test]
    fn membero_does_not_succeed_if_value_not_in_list() {
        let result = run!(*, q, membero(0, list!(1, 2, 3)));
        assert_eq!(result.into_vec(), Vec::<()>::new());
    }

    #[test]
    fn membero_does_succeed_if_value_first_in_list() {
        let result = run!(*, q, membero(1, list!(1, 2, 3)));
        assert_eq!(result.into_vec(), vec![Value::rv(0)]);
    }

    #[test]
    fn membero_does_succeed_if_value_in_list() {
        let result = run!(*, q, membero(2, list!(1, 2, 3)));
        assert_eq!(result.into_vec(), vec![Value::rv(0)]);
    }

    #[test]
    fn membero_succeeds_for_all_possible_members() {
        let result = run!(*, q, membero(q, list!(1, 2, 3)));
        assert_eq!(result.into_vec(), vec![1, 2, 3]);
    }

    #[test]
    fn membero_succeeds_for_all_possible_lists() {
        let mut result = run!(q, membero(42, q));
        assert_eq!(result.next().unwrap(), list![Value::new(42) ; Value::rv(0)]);
        assert_eq!(
            result.next().unwrap(),
            list![Value::rv(0), Value::new(42) ; Value::rv(1)]
        );
        assert_eq!(
            result.next().unwrap(),
            list![Value::rv(0), Value::rv(1), Value::new(42) ; Value::rv(2)]
        );
    }
}
