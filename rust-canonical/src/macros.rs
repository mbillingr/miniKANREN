#[macro_export]
macro_rules! disj {
    () => { fail() };
    ($g:expr) => { $g };
    ($g0:expr; $($g:expr);*) => { disj2($g0, disj!($($g);*))}
}

#[macro_export]
macro_rules! conj {
    () => { succeed() };
    ($g:expr) => { $g };
    ($g0:expr, $($g:expr),*) => { conj2($g0, conj!($($g),*))}
}

#[macro_export]
macro_rules! defrel {
    (pub $name:ident($($args:ident),*) { $($g:expr),* $(,)? }) => {
        pub fn $name($($args: impl 'static + Into<Value>),*) -> impl Goal<StatSubs> {
            $(
                let $args = $args.into();
            )*
            move |s| {
                $(
                    let $args = $args.clone();
                )*
                Stream::suspension(move || conj!($($g),*).apply(s))
            }
        }
    };

    ($name:ident($($args:ident),*) { $($g:expr),* $(,)? }) => {
        fn $name($($args: impl 'static + Into<Value>),*) -> impl Goal<StatSubs> {
            $(
                let $args = $args.into();
            )*
            move |s| {
                $(
                    let $args = $args.clone();
                )*
                Stream::suspension(move || conj!($($g),*).apply(s))
            }
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
}

#[macro_export]
macro_rules! run {
    (*, ($($x:ident),*), $($body:tt)*) => {
        run!(@ *, ($($x),*), $($body)*)
    };

    (*, $q:ident, $($g:expr),* $(,)?) => {
        run!(@ *, $q, $($g),*)
    };

    ($n:expr, ($($x:ident),*), $($body:tt)*) => {
        run!(@ $n, ($($x),*), $($body)*)
    };

    ($n:tt, $q:ident, $($g:expr),* $(,)?) => {
        run!(@ $n, $q, $($g),*)
    };

    (($($x:ident),*), $($body:tt)*) => {
        run!(@ iter, ($($x),*), $($body)*)
    };

    ($q:ident, $($g:expr),* $(,)?) => {
        run!(@ iter, $q, $($g),*)
    };

    (@ $n:tt, ($($x:ident),*), $($g:expr),* $(,)?) => {
        run!(@ $n, q, {
            fresh!(
                ($($x),*),
                eq(vec![$(Value::var($x.clone())),*], q),
                $($g),*
            )
        })
    };

    (@ *, $q:ident, $($g:expr),* $(,)?) => {{
        let $q = Var::new(stringify!($q));
        let var = Value::var($q.clone());
        conj!($($g),*).run_inf().map(move |s| s.reify(&var))
    }};

    (@ iter, $q:ident, $($g:expr),* $(,)?) => {{
        let $q = Var::new(stringify!($q));
        let var = Value::var($q.clone());
        conj!($($g),*).iter().map(move |s| s.reify(&var))
    }};

    (@ $n:expr, $q:ident, $($g:expr),* $(,)?) => {{
        let $q = Var::new(stringify!($q));
        let var = Value::var($q.clone());
        conj!($($g),*).run($n).map(move |s| s.reify(&var))
    }};
}

#[macro_export]
macro_rules! fresh {
    (($($x:ident),*), $($g:expr),* $(,)?) => {{
        $( let $x = Var::new(stringify!($x)); )*
        conj!($($g),*)
    }}
}

#[macro_export]
macro_rules! conde {
    ( $($($g:expr),*);*) => {
        disj!($(conj!( $($g),*));*)
    }
}

#[macro_export]
macro_rules! conda {
    ($($g:expr),*) => { conj!($($g),*) };

    ($g0:expr, $($g:expr),*; $($rest:tt)*) => {
        ifte(g0, conj!($($g),*), conda!($($rest)*))
    };
}

#[macro_export]
macro_rules! condu {
    ( $($g0:expr, $($g:expr),*);*) => {
        conda!($(once($gO), $($g),*);*)
    }
}
