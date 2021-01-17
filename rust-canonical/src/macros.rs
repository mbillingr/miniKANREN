//! Macros for embedding miniKANREN as DSL in Rust

/// Creates a goal that succeeds if any of its subgoals succeeds
#[macro_export]
macro_rules! disj {
    () => { $crate::prelude::fail() };
    ($g:expr) => { $g };
    ($g0:expr; $($g:expr);*) => { $crate::prelude::disj2($g0, $crate::disj!($($g);*))}
}

/// Creates a goal that succeeds if all of its subgoals succeed
#[macro_export]
macro_rules! conj {
    () => { $crate::prelude::succeed() };
    ($g:expr) => { $g };
    ($g0:expr, $($g:expr),*) => { $crate::prelude::conj2($g0, $crate::conj!($($g),*))}
}

/// Define a relation.
/// A relation is a function that creates a goal.
#[macro_export]
macro_rules! defrel {

    ($(#[$outer:meta])* pub $name:ident($($args:ident),*) { $($g:expr),* $(,)? }) => {
        $(#[$outer])*
        pub fn $name($($args: impl 'static + Into<$crate::prelude::Value>),*) -> impl $crate::prelude::Goal<$crate::prelude::Substitution<'static>> {
            defrel!(@body: $($args),* { $($g),* })
        }
    };

    ($(#[$outer:meta])* $name:ident($($args:ident),*) { $($g:expr),* $(,)? }) => {
        $(#[$outer])*
        fn $name($($args: impl 'static + Into<$crate::prelude::Value>),*) -> impl $crate::prelude::Goal<$crate::prelude::Substitution<'static>> {
            defrel!(@body: $($args),* { $($g),* })
        }
    };

    ($(#[$outer:meta])* pub trace $name:ident($($args:ident),*) { $($g:expr),* $(,)? }) => {
        $(#[$outer])*
        pub fn $name($($args: impl 'static + Into<$crate::prelude::Value>),*) -> impl $crate::prelude::Goal<$crate::prelude::Substitution<'static>> {
            defrel!(@tracebody: $name, $($args),* { $($g),* })
        }
    };

    ($(#[$outer:meta])* trace $name:ident($($args:ident),*) { $($g:expr),* $(,)? }) => {
        $(#[$outer])*
        fn $name($($args: impl 'static + Into<$crate::prelude::Value>),*) -> impl $crate::prelude::Goal<$crate::prelude::Substitution<'static>> {
            defrel!(@tracebody: $name, $($args),* { $($g),* })
        }
    };

    // alternate syntax: separate goals with ;
    (pub $name:ident($($args:ident),*) { $($g:expr);* $(;)? }) => {
        defrel!{pub $name($($args),*) { $($g),* }}
    };

    // alternate syntax: separate goals with ;
    ($name:ident($($args:ident),*) { $($g:expr);* $(;)? }) => {
        defrel!{$name($($args),*) { $($g),* }}
    };

    (@body: $($args:ident),* { $($g:expr),* }) => {{
        $(
            let $args = $args.into();
        )*
        move |s| {
            $(
                let $args = $args.clone();
            )*
            $crate::prelude::Stream::suspension(move || $crate::conj!($($g),*).apply(s))
        }
    }};

    (@tracebody: $name:ident, $($args:ident),* { $($g:expr),* }) => {{
        $(
            let $args = $args.into();
        )*
        move |s: $crate::prelude::Substitution<'static>| {
            {
                print!("{} apply:", stringify!($name));
                let sx = {
                    $(
                        let $args = $args.clone();
                        print!(" {}={:?}", stringify!($args), s.reify(&$args));
                    )*
                    $crate::conj!($($g),*).apply(s.clone())
                };
                match sx {
                   $crate::prelude::Stream::Pair(first_sub, next) => {
                        print!(" succeeded with");
                        $(print!(" {}={:?}", stringify!($args), first_sub.reify(&$args));)*
                        if next.is_empty() {
                            println!();
                        } else {
                            println!(" ...");
                        }
                   }
                   $crate::prelude::Stream::Suspension(_) => println!(" ..."),
                   $crate::prelude::Stream::Empty => println!(" failed."),
                }
            }

            $(
                let $args = $args.clone();
            )*

            $crate::prelude::Stream::suspension(move || {
                let sx = $crate::conj!($($g),*).apply(s);
                sx
            })
        }
    }};
}

/// Define a relation.
/// A relation is a function that creates a goal.
#[macro_export]
macro_rules! defmatch {

    ($(#[$outer:meta])* pub $name:ident($($args:ident),*) { $($body:tt)* }) => {
        defrel! {
            $(#[$outer])*
            pub $name($($args),*) {
                $crate::matche! { list![$($args.clone()),*],
                    $($body)*
                }
            }
        }
    };

    ($(#[$outer:meta])* $name:ident($($args:ident),*) { $($body:tt)* }) => {
        defrel! {
            $(#[$outer])*
            $name($($args),*) {
                $crate::matche! { list![$($args.clone()),*],
                    $($body)*
                }
            }
        }
    };
}

/// Run one or more goals.
///
/// The syntax `run!(n, var(s), goal1, goal2, ...)` produces at most n
/// solutions in Scheme you wold write `(run n var(s) goal1 goal2 ...)`.
/// The syntax `run!(*, var(s), goal1, goal2, ...)` produces all
/// solutions in Scheme you wold write `(run* var(s) goal1 goal2 ...)`.
/// The latter may result in an infinite recursion which eventually
/// crashes with a stack overflow.
///
/// We support an additional syntax `run!(var(s), goal1, goal2, ...)`
/// that returns a (possibly infinite) iterator over all solutions.
#[macro_export]
macro_rules! run {
    (*, ($($x:ident),*), $($body:tt)*) => {
        $crate::run!(@ *, ($($x),*), $($body)*)
    };

    (*, $q:ident, $($g:expr),* $(,)?) => {
        $crate::run!(@ *, $q, $($g),*)
    };

    ($n:expr, ($($x:ident),*), $($body:tt)*) => {
        $crate::run!(@ $n, ($($x),*), $($body)*)
    };

    ($n:tt, $q:ident, $($g:expr),* $(,)?) => {
        $crate::run!(@ $n, $q, $($g),*)
    };

    (($($x:ident),*), $($body:tt)*) => {
        $crate::run!(@ iter, ($($x),*), $($body)*)
    };

    ($q:ident, $($g:expr),* $(,)?) => {
        $crate::run!(@ iter, $q, $($g),*)
    };

    (@ $n:tt, ($($x:ident),*), $($g:expr),* $(,)?) => {
        $crate::run!(@ $n, q, {
            $crate::fresh!(
                ($($x),*),
                $crate::prelude::eq(vec![$($crate::prelude::Value::var($x.clone())),*], q),
                $($g),*
            )
        })
    };

    (@ *, $q:ident, $($g:expr),* $(,)?) => {{
        let $q = $crate::prelude::Var::new(stringify!($q));
        let var = $crate::prelude::Value::var($q.clone());
        $crate::conj!($($g),*).run_inf().map(move |s| s.reify(&var))
    }};

    (@ iter, $q:ident, $($g:expr),* $(,)?) => {{
        let $q = $crate::prelude::Var::new(stringify!($q));
        let var = $crate::prelude::Value::var($q.clone());
        $crate::conj!($($g),*).iter().map(move |s| s.reify(&var))
    }};

    (@ $n:expr, $q:ident, $($g:expr),* $(,)?) => {{
        let $q = $crate::prelude::Var::new(stringify!($q));
        let var = $crate::prelude::Value::var($q.clone());
        $crate::conj!($($g),*).run($n).map(move |s| s.reify(&var))
    }};
}

/// Bind fresh variables with scope inside the body of `fresh!`.
#[macro_export]
macro_rules! fresh {
    (($($x:ident),*), $($g:expr),* $(,)?) => {{
        $( let $x = $crate::prelude::Var::new(stringify!($x)); )*
        $crate::conj!($($g),*)
    }}
}

/// Creates a goal that succeeds if any of its *lines* succeeds.
/// Every successful *line* contributes one or more values.
///
/// A *line* (separated by `;`) succeeds if all of its
/// goals (separated by `,`) succeed.
#[macro_export]
macro_rules! conde {
    ( $($($g:expr),*;)* ) => {
        $crate::disj!($($crate::conj!( $($g),*));*)
    }
}

/// Creates a goal that succeeds if any of its *lines* succeeds.
/// Only the first *line* that succeeds can contribute values.
///
/// A *line* (separated by `;`) succeeds if all of its
/// goals (separated by `,`) succeed.
#[macro_export]
macro_rules! conda {
    ($($g:expr),*) => { $crate::conj!($($g),*) };

    ($g0:expr, $($g:expr),+; $($rest:tt)*) => {
        $crate::prelude::ifte($g0, $crate::conj!($($g),*), $crate::conda!($($rest)*))
    };

    ($g0:expr; $($rest:tt)*) => {
        $crate::prelude::ifte($g0, $crate::succeed(), $crate::conda!($($rest)*))
    };
}

/// `Condu!` behaves like `conda!`, except that a successful line
/// succeeds only once.
#[macro_export]
macro_rules! condu {
    ($g0:expr, $($g:expr),+;) => {
        $crate::conj!($crate::once($g0), $($g),*)
    };

    ($g0:expr;) => {
        $crate::once($g0)
    };

    ($g0:expr, $($g:expr),+; $($rest:tt)*) => {
        $crate::prelude::ifte($crate::once($g0), $crate::conj!($($g),*), $crate::condu!($($rest)*))
    };

    ($g0:expr; $($rest:tt)*) => {
        $crate::prelude::ifte($crate::once($g0), $crate::succeed(), $crate::condu!($($rest)*))
    };
}

/// `Matche!`  behaves like `conde!` but allows pattern matching.
/// This simplifies destructuring and creation of fresh variables.
///
/// Every successful *line* contributes one or more values.
#[macro_export]
macro_rules! matche {
    ( $val:expr, $( $pat:tt => $($goal:expr),* ; )+ ) => {
        $crate::disj! {
            $( $crate::matche!(@match: $val, $pat => $($goal),*) );*
        }
    };

    // $val matches a single-element list
    (@match: $val:expr, ($h:tt) => $($goal:expr),*) => {
        fresh! { (h),
            $crate::goals::list::conso(h, (), $val.clone()),
            $crate::matche!(@match: h, $h => $($goal),*)
        }
    };

    // $val matches a pair
    (@match: $val:expr, ($h:tt ; $t:tt) => $($goal:expr),*) => {
        fresh! { (h, t),
            $crate::goals::list::conso(h, t, $val.clone()),
            $crate::matche!(@match: h, $h => $crate::matche!(@match: t, $t => $($goal),*))
        }
    };

    // $val matches a a list with at least one item
    (@match: $val:expr, ($h:tt, $($rest:tt)*) => $($goal:expr),*) => {
        fresh! { (h, t),
            $crate::goals::list::conso(h, t, $val.clone()),
            $crate::matche!(@match: h, $h => $crate::matche!(@match: t, ($($rest)*) => $($goal),*))
        }
    };

    // $val matches anything - effectively it's ignored
    (@match: $val:expr, _ => $($goal:expr),*) => {
        $crate::conj!{
            $($goal),*
        }
    };

    // $val matches a name - it is bound to a variable with that name
    (@match: $val:expr, $v:ident => $($goal:expr),*) => {
        fresh! { ($v),
            $crate::prelude::eq($val.clone(), $v),
            $($goal),*
        }
    };

    // $val matches an expression - they are unified
    (@match: $val:expr, $c:expr => $($goal:expr),*) => {
        $crate::conj!{
            $crate::prelude::eq($val.clone(), $c),
            $($goal),*
        }
    };
}

#[cfg(test)]
mod tests {
    use crate::testing::{fails, has_unique_solution, succeeds};
    use crate::{eq, fail, list, succeed};
    use crate::{Goal, Value};

    #[test]
    fn matching_anything_succeeds_always() {
        succeeds(matche! { q,
            _ => ;
        });
    }

    #[test]
    fn matching_fails_if_any_further_goals_fail() {
        fails(matche! { q,
            _ => succeed(), fail(), succeed();
        });
    }

    #[test]
    fn matching_a_constant_binds_value() {
        has_unique_solution(
            run!(
                q,
                matche! { q,
                    1 => ;
                }
            ),
            1.into(),
        );
    }

    #[test]
    fn each_successful_matching_line_contributes_a_value() {
        assert_eq!(
            run!(*, q,
                matche! { q,
                    1 => ;
                    2 => fail();
                    3 => ;
                }
            )
            .into_vec(),
            vec![1, 3],
        );
    }

    #[test]
    fn matching_deconstructs_lists() {
        assert_eq!(
            run!(*, q,
                matche! { q,
                    (_) => ;
                    (_, _) => ;
                    (_, _ ; _) => ;
                }
            )
            .into_vec(),
            vec![
                list![Value::rv(0)],
                list![Value::rv(0), Value::rv(1)],
                list![Value::rv(0), Value::rv(1) ; Value::rv(2)],
            ],
        );
    }

    #[test]
    fn matching_deconstructs_lists_and_binds_fresh_vars() {
        assert_eq!(
            run!(*, q,
                matche! { q,
                    (a) => eq(a, 1);
                    (b, c) => eq(b, c);
                    (a, _ ; d) => eq(d, 2);
                }
            )
            .into_vec(),
            vec![
                list![1],
                list![Value::rv(0), Value::rv(0)],
                list![Value::rv(0), Value::rv(1) ; 2],
            ],
        );
    }

    #[test]
    fn matche_can_escape_outside_vars() {
        let x = 42;
        has_unique_solution(
            run!(
                q,
                matche! { q,
                    // The trick here is that wrapping x in {} turns it into an expression
                    ({x}) => ;
                }
            ),
            list![42],
        );
    }

    #[test]
    fn matche_matches_values() {
        has_unique_solution(
            run!(
                q,
                matche! { list![1, q],
                    (b, c) => eq(b, c);
                }
            ),
            1.into(),
        );
    }
}
