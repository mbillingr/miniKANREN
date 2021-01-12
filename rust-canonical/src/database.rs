use crate::core::value::Value;
use std::sync::Arc;

pub struct Database;

impl Database {
    pub fn new() -> Arc<Self> {
        Arc::new(Database)
    }

    pub fn query(&self, rel_name: &str) -> impl Iterator<Item = impl Iterator<Item = Value>> {
        vec![vec!["foo"], vec!["bar"], vec!["baz"]]
            .into_iter()
            .map(|row| row.into_iter().map(Value::from))
    }
}

#[macro_export]
macro_rules! db_rel {
    ($($rel:ident($($args:ident),*));* $(;)?) => {
        $(
            fn $rel(db: &Arc<Database>, $($args: Var),*) -> impl Goal<StatSubs> {
                let db = db.clone();
                move |s: StatSubs| {
                    let values = db.query(stringify!($rel));
                    let subs = values
                        .filter_map(|mut value| {
                            let s = s.clone();
                            $(let s = s.extend($args, value.next()?)?;)*
                            Some(s)
                        });
                    Stream::from_iter(subs)
                }
            }
        )*
    };
}

#[macro_export]
macro_rules! db_facts {
    ($($db:ident { $($rel:ident($($args:expr),*));* $(;)? })*) => {};
}

#[cfg(test)]
mod tests {
    use super::*;
}
