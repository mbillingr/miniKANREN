use crate::core::goal::Goal;
use crate::goals::numbers::{inco, zero};
use crate::goals::primitive::eq;
use crate::{conde, defrel, fresh};

/// Constructs a cons list.
#[macro_export]
macro_rules! list {
    () => { $crate::prelude::Value::new(()) };

    ($single:expr) => {
        $crate::prelude::Value::cons($single, ())
    };

    ($car:expr ; $cdr:expr) => {
        $crate::prelude::Value::cons($car, $cdr)
    };

    (($($first:tt)*), $($rest:tt)*) => {
        $crate::prelude::Value::cons(list![$($first)*], list![$($rest)*])
    };

    ($first:expr, $($rest:tt)*) => {
        $crate::prelude::Value::cons($first, list![$($rest)*])
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
            eq((a, d), p)
        }
    }
}

defrel! {
    /// Creates a goal that succeeds if p is a pair and its second element is equivalent to d.
    pub cdro(p, d) {
        fresh!{ (a),
            eq((a, d), p)
        }
    }
}

defrel! {
    /// Creates a goal that succeeds if p is a pair.
    pub pairo(p) {
        fresh! { (a, d),
            eq((a, d), p)
        }
    }
}

defmatch! {
    /// Creates a goal that succeeds if p is a list.
    pub listo(l) {
        (()) => ;
        ((_ ; tail)) => listo(tail);
    }
}

defmatch! {
    /// Creates a goal that succeeds if l is a list that contains x.
    pub membero(x, l) {
        (x, ({x} ; _)) => ;
        (x, (_ ; t)) => membero(x, t);
    }
}

defrel! {
    /// Creates a goal that succeeds if two lists can be appended two form a third.
    pub appendo(a, b, l) {
        conde!{
            fresh!( (h, at, lt),
                conso(h, at, a.clone()),
                conso(h, lt, l.clone()),
                appendo(at, b.clone(), lt),
            );
            eq(a.clone(), ()), eq(b.clone(), l.clone());
        }
    }
}

defmatch! {
    /// Creates a goal that succeeds if the list has length n.
    pub lengtho(l, n) {
        ((), n) => zero(n);
        ((_ ; tail), n) => fresh!{ (nt), inco(nt, n), lengtho(tail, nt) };
    }
}

defmatch! {
    /// Creates a goal that succeeds if element x can be removed from list l to
    /// form list out. If x does not occur in l it fails.
    pub rembero(x, l, out) {
        (_, _, _) => conso(x.clone(), out.clone(), l.clone());
        (_, (a ; rest), (b ; tmp)) => eq(a, b), rembero(x, rest, tmp);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::value::Value;
    use crate::goals::numbers::num;
    use crate::testing::{fails, has_unique_solution, succeeds};
    use crate::*;

    #[test]
    fn pairo_binds_pair_to_variable() {
        let result = run!(*, q, pairo(q));
        assert_eq!(
            result.into_vec(),
            vec![Value::new((Value::rv(0), Value::rv(1)))]
        );
    }

    #[test]
    fn listo_fails_for_non_list() {
        fails(listo(0));
    }

    #[test]
    fn listo_succeeds_for_empty_list() {
        succeeds(listo(()));
    }

    #[test]
    fn listo_succeeds_for_pair_whose_second_element_is_a_list() {
        succeeds(listo((1, ())));
    }

    #[test]
    fn listo_fails_for_pair_whose_second_element_is_no_list() {
        fails(listo((1, 2)));
    }

    #[test]
    fn listo_generates_all_possible_lists() {
        let mut result = run!(q, listo(q));
        assert_eq!(result.next().unwrap(), list![]);
        assert_eq!(result.next().unwrap(), list![Value::rv(0)]);
        assert_eq!(result.next().unwrap(), list![Value::rv(0), Value::rv(1)]);
        assert_eq!(
            result.next().unwrap(),
            list![Value::rv(0), Value::rv(1), Value::rv(2)]
        );
    }

    #[test]
    fn membero_does_not_succeed_if_value_not_in_list() {
        fails(membero(0, list!(1, 2, 3)));
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

    #[test]
    fn appendo_succeeds_when_inputs_match() {
        succeeds(appendo((), (), ()));
        succeeds(appendo(list![1], (), list![1]));
        succeeds(appendo((), list![1], list![1]));
        succeeds(appendo(list![1], list![2], list![1, 2]));
        succeeds(appendo(list![1, 2], 3, list![1, 2 ; 3]));
    }

    #[test]
    fn appendo_fails_when_first_argument_is_not_a_list() {
        fails(fresh! {(q), appendo(0, (), q)});
    }

    #[test]
    fn appendo_fails_when_third_argument_is_not_a_list() {
        fails(fresh! {(q), appendo(q, (), 0)});
    }

    #[test]
    fn appendo_fails_when_inputs_dont_match() {
        fails(appendo(list![1], list![2], list![]));
    }

    #[test]
    fn appending_an_empty_list_gives_same_list() {
        let result = run!(3, q, appendo(q, (), q));
        assert_eq!(
            result.into_vec(),
            vec![
                list![],
                list![Value::rv(0)],
                list![Value::rv(0), Value::rv(1)]
            ]
        );
    }

    #[test]
    fn appending_to_an_empty_list_gives_appended_value() {
        let result = run!(*, q, appendo((), q, q));
        assert_eq!(result.into_vec(), vec![Value::rv(0)]);
    }

    #[test]
    fn appending_to_a_list() {
        let result = run!(*, q, appendo(list![1, 2], q, list![1, 2 ; q]));
        assert_eq!(result.into_vec(), vec![Value::rv(0)]);
    }

    #[test]
    fn appending_arbitrary_list() {
        let result = run!(3, q, fresh! { (a), appendo(a, list![2, 1], q)});
        assert_eq!(
            result.into_vec(),
            vec![
                list![2, 1],
                list![Value::rv(0), 2, 1],
                list![Value::rv(0), Value::rv(1), 2, 1]
            ]
        );
    }

    #[test]
    fn lengtho_is_zero_for_the_empty_list() {
        succeeds(lengtho((), num(0)));
    }

    #[test]
    fn lengtho_is_one_for_list_with_one_element() {
        succeeds(lengtho(list![0], num(1)));
    }

    #[test]
    fn lengtho_produces_lists_and_matching_lengths() {
        let mut solutions = run!((l, n), lengtho(l, n));
        assert_eq!(solutions.next(), Some(list![(), num(0)]));
        assert_eq!(solutions.next(), Some(list![(Value::rv(0)), num(1)]));
        assert_eq!(
            solutions.next(),
            Some(list![(Value::rv(0), Value::rv(1)), num(2)])
        );
    }

    #[test]
    fn rembero_on_empty_list_always_fails() {
        fails(fresh! {(x), rembero(x, list![], list![])});
    }

    #[test]
    fn rembero_fails_if_item_not_in_list() {
        let q = Var::new("q");
        fails(rembero(0, list![1, 2, 3], q));
    }

    #[test]
    fn rembero_returns_empty_list_if_it_removes_the_only_item() {
        has_unique_solution(run!(q, rembero(0, list![0], q)), list![]);
    }

    #[test]
    fn rembero_generates_possible_removal_combinations() {
        let solutions: Vec<_> = run!(q, rembero(1, list![0, 1, 2, 3, 0, 1, 2, 3], q)).collect();
        assert_eq!(
            solutions,
            vec![list![0, 2, 3, 0, 1, 2, 3], list![0, 1, 2, 3, 0, 2, 3],]
        );
    }

    #[test]
    fn rembero_lists_all_items_that_can_be_removed_from_list() {
        let f = Var::new("f");
        let solutions: Vec<_> = run!(q, rembero(q, list![0, 1, 2, 3], f)).collect();
        assert_eq!(solutions, vec![0, 1, 2, 3]);
    }
}
