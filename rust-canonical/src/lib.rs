//! Rust implementation of the miniKANREN relational logic language.

#[macro_use]
mod macros;
pub mod core;
pub mod database;
#[macro_use]
pub mod goals;
pub mod prelude;
pub mod testing;

pub use prelude::*;

#[cfg(test)]
mod tests {
    use crate::core::goal::RawGoal;
    use crate::core::logic_variable::{ReifiedVar, Var};
    use crate::core::stream::Stream;
    use crate::core::substitution::Substitution;
    use crate::core::value::Value;
    use crate::goals::combinators::{conj2, disj2, ifte};
    use crate::goals::primitive::{alwayso, eq, fail, succeed};
    use crate::{defrel, disj, fresh, run, substitution};
    use std::borrow::Cow;
    use std::collections::HashMap;

    #[test]
    fn it_works() {
        let u = Var::new("u");
        let v = Var::new("v");
        let w = Var::new("w");
        let x = Var::new("x");
        let y = Var::new("y");
        let z = Var::new("z");

        assert!(Substitution::empty().occurs(&x, &Value::var(x.clone())));
        assert!(substitution! {y: x}.occurs(&x, &Value::cons(Value::var(y), ())));
        assert!(substitution! {y: x}.occurs(&x, &vec![Value::var(y)].into()));
        assert!(Substitution::empty()
            .extend(x.clone(), vec![Value::var(x.clone())].into())
            .is_none());
        assert!(substitution! {y: x}
            .extend(x.clone(), vec![Value::var(y.clone())].into())
            .is_none());

        assert_eq!(
            Substitution::empty().unify(
                &Value::new(Some(Value::var(x.clone()))),
                &Value::new(Some(Value::var(y.clone())))
            ),
            Some(substitution!(x: y)),
        );

        assert_eq!(
            Substitution::empty()
                .unify(
                    &Value::new(Some(Value::var(x.clone()))),
                    &Value::new(Some(Value::var(y.clone())))
                )
                .unwrap()
                .unify(
                    &Value::new(Some(Value::var(x.clone()))),
                    &Value::new(Some(Value::new(42)))
                ),
            Some(substitution!(x: y, y: 42)),
        );

        assert_eq!(
            eq(x, Value::var(u.clone())).apply(Substitution::empty()),
            Stream::singleton(substitution!(x: u))
        );
        assert_eq!(
            eq(x, 42).apply(Substitution::empty()),
            Stream::singleton(substitution!(x: 42))
        );
        assert_eq!(
            eq(42, 42).apply(Substitution::empty()),
            Stream::singleton(substitution!())
        );
        assert_eq!(eq(42, 123).apply(Substitution::empty()), Stream::empty());

        assert_eq!(fail().apply(Substitution::empty()), Stream::Empty);
        assert_eq!(eq(true, false).apply(Substitution::empty()), Stream::Empty);
        assert_eq!(
            eq(x, y).apply(Substitution::empty()),
            Stream::singleton(substitution! {x: y})
        );

        assert_eq!(
            disj2(eq("olive", x), eq("oil", x)).apply(Substitution::empty()),
            Stream::cons(
                substitution! {x: "olive"},
                Stream::cons(substitution! {x: "oil"}, Stream::empty())
            )
        );

        // no value - stack overflow
        //assert_eq!(nevero()(Substitution::empty()).take_inf(1), Stream::Empty);

        assert_eq!(
            alwayso().apply(Substitution::empty()).take_inf(3),
            Stream::from_iter(
                vec![
                    Substitution::empty(),
                    Substitution::empty(),
                    Substitution::empty()
                ]
                .into_iter()
            )
        );

        assert_eq!(
            disj2(eq("olive", x), eq("oil", x))
                .apply(Substitution::empty())
                .take_inf(5)
                .len(),
            Some(2)
        );

        assert_eq!(
            conj2(eq("olive", x), eq("oil", x)).apply(Substitution::empty()),
            Stream::empty()
        );

        assert_eq!(
            conj2(eq("olive", x), eq(y.clone(), x.clone())).apply(Substitution::empty()),
            Stream::singleton(substitution! {y: "olive", x: "olive"})
        );

        let a1 = Value::from(vec![
            Value::from(u),
            Value::from(w),
            Value::from(y),
            Value::from(z),
            Value::from(Some(Value::from(vec![Value::from("ice"), Value::from(z)]))),
        ]);
        let a2 = Value::from("corn");
        let a3 = Value::from(vec![Value::from(v), Value::from(u)]);
        let s = substitution! {x: a1, y: a2, w: a3};
        //println!("{:?}", reify((&x).into())(s));
        assert_eq!(
            s.reify(&x.into()),
            Value::from(vec![
                Value::from(ReifiedVar(0)),
                Value::from(vec![Value::from(ReifiedVar(1)), Value::from(ReifiedVar(0))]),
                Value::from("corn"),
                Value::from(ReifiedVar(2)),
                Value::from(vec![Value::from("ice"), Value::from(ReifiedVar(2))])
            ])
        );

        assert_eq!(
            disj2(eq("olive", x), eq("oil", x))
                .run(5)
                .into_iter()
                .map(|s| s.reify(&x.into()))
                .collect::<Vec<_>>(),
            vec![Value::from("olive"), Value::from("oil")],
        );

        assert_eq!(
            ifte(succeed(), eq(false, y.clone()), eq(true, y.clone())).apply(Substitution::empty()),
            Stream::singleton(substitution!(y: false))
        );

        assert_eq!(
            ifte(fail(), eq(false, y.clone()), eq(true, y.clone())).apply(Substitution::empty()),
            Stream::singleton(substitution!(y: true))
        );

        assert_eq!(
            disj!(eq("virgin", x); eq("olive", x); eq("oil", x)).apply(Substitution::empty()),
            Stream::from_iter(
                vec![
                    substitution! {x: "virgin"},
                    substitution! {x: "olive"},
                    substitution! {x: "oil"},
                ]
                .into_iter()
            )
        );

        defrel! {
            teacup(t) {
                disj!(eq("tea", t.clone()); eq("cup", t))
            }
        }

        assert_eq!(
            teacup(x.clone())
                .apply(Substitution::empty())
                .into_iter()
                .collect::<Vec<_>>(),
            vec![substitution!(x: "tea"), substitution!(x: "cup")]
        );

        assert_eq!(
            format!(
                "{:?}",
                fresh!((x, y), eq(x, y)).apply(Substitution::empty())
            ),
            "({x: y})"
        );

        assert_eq!(run!(1, x,), Stream::singleton(Value::from(ReifiedVar(0))));
        assert_eq!(run!(1, x, eq(x, 42)), Stream::singleton(Value::new(42)));
        assert_eq!(
            run!(1, (x, y),),
            Stream::singleton(Value::new(vec![
                Value::from(ReifiedVar(0)),
                Value::from(ReifiedVar(1))
            ]))
        );
        assert_eq!(
            run!(1, (x, y), eq(x, 42)),
            Stream::singleton(Value::new(vec![Value::new(42), Value::from(ReifiedVar(0))]))
        );

        defrel! {
            conso(a, d, p) {
                eq(vec![a, d], p)
            }
        }

        assert_eq!(
            run!(*, x, conso(1, 2, x)),
            Stream::singleton(Value::new(vec![Value::new(1), Value::new(2)]))
        );
        assert_eq!(
            run!(*, x, conso(1, x, vec![Value::new(1), Value::new(2)])),
            Stream::singleton(Value::new(2))
        );
        assert_eq!(
            run!(*, x, conso(x, 2, vec![Value::new(1), Value::new(2)])),
            Stream::singleton(Value::new(1))
        );
        assert_eq!(
            run!(*, x, conso(x.clone(), x, vec![Value::new(1), Value::new(2)])),
            Stream::empty()
        );
        assert_eq!(
            run!(*, x, conso(x.clone(), x, vec![Value::new(3), Value::new(3)])),
            Stream::singleton(Value::new(3))
        );

        assert_eq!(
            run!(5, q, eq(q, "onion")),
            Stream::singleton(Value::new("onion"))
        );

        assert_eq!(
            run!(5, q, eq(q, "onion"), alwayso(),),
            Stream::from_iter(std::iter::repeat(Value::new("onion")).take(5))
        );
    }
}
