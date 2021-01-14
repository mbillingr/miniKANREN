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

    ($g0:expr, $($g:expr),*; $($rest:tt)*) => {
        $crate::prelude::ifte($g0, $crate::conj!($($g),*), $crate::conda!($($rest)*))
    };
}

/// `Condu!` behaves like `conda!`, except that a successful line
/// succeeds only once.
#[macro_export]
macro_rules! condu {
    ( $($g0:expr, $($g:expr),*);* ) => {
        $crate::conda!($($crate::prelude::once($gO), $($g),*);*)
    }
}
