//! Demonstrate different kinds of cond

use mini_kanren::goals::list::membero;
use mini_kanren::prelude::*;
use mini_kanren::{conda, conde, conj, defrel, list, run};

defrel! {
    /// Create a goal that succeeds if x occurs in s1 or s2 and r contains the correct result
    whiche(x, s1, s2, r) {
        conde! {
            membero(x.clone(), s1.clone()), membero(x.clone(), s2.clone()), eq(r.clone(), "both");
            membero(x.clone(), s1.clone()), eq(r.clone(), "one");
            membero(x.clone(), s2.clone()), eq(r.clone(), "two");
        }
    }
}

defrel! {
    /// Create a goal that succeeds if x occurs in s1 or s2 and r contains the correct result
    whicha(x, s1, s2, r) {
        conda! {
            conj!(membero(x.clone(), s1.clone()), membero(x.clone(), s2.clone()), eq(r.clone(), "both"));
            conj!(membero(x.clone(), s1.clone()), eq(r.clone(), "one"));
            conj!(membero(x.clone(), s2.clone()), eq(r.clone(), "two"));
        }
    }
}

fn main() {
    println!(
        "Which list contains 3 using conde? {:?}",
        run!(*, q, whiche(3, list![1, 2, 3], list![3, 4, 5], q))
    );
    println!(
        "Which list contains 3 using conda? {:?}",
        run!(*, q, whicha(3, list![1, 2, 3], list![3, 4, 5], q))
    );
}
