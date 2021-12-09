//! A simple relational database

use crate::core::value::Value;
use std::collections::HashMap;

pub struct Database {
    db: HashMap<String, Vec<Vec<Value>>>,
    default_table: Vec<Vec<Value>>,
}

impl Database {
    pub fn new() -> Self {
        Database {
            db: HashMap::new(),
            default_table: vec![],
        }
    }

    pub fn insert(&mut self, table_name: &str, row: Vec<Value>) {
        if let Some(table) = self.db.get_mut(table_name) {
            table.push(row)
        } else {
            self.db.insert(table_name.to_string(), vec![row]);
        }
    }

    pub fn query(
        &self,
        table_name: &str,
    ) -> impl Iterator<Item = impl Iterator<Item = &Value> + '_> + '_ {
        let table = self.db.get(table_name).unwrap_or(&self.default_table);
        table.into_iter().map(|row| row.into_iter())
    }
}

/// Defines a database relation.
///
/// The macro produces a goal-generating function, which can be used
/// to query the database.
#[macro_export]
macro_rules! db_rel {
    ($($rel:ident($($args:ident),*));* $(;)?) => {
        $(
            /// Creates a goal that succeeds if the relation is consistent with the database.
            fn $rel(db: &Arc<$crate::database::Database>, $($args: impl Into<$crate::prelude::Value>),*) -> impl Clone + $crate::prelude::Goal<$crate::prelude::Substitution<'static>> {
                let db = db.clone();
                $(let $args = $args.into();)*
                move |s: $crate::prelude::Substitution<'static>| {
                    let values = db.query(stringify!($rel));
                    let subs = values
                        .filter_map(|mut value| {
                            let s = s.clone();
                            $(let s = s.unify(&$args, value.next()?)?;)*
                            Some(s)
                        });
                    $crate::prelude::Stream::from_iter(subs)
                }
            }
        )*
    };
}

/// Insert facts into databases.
#[macro_export]
macro_rules! db_facts {
    ($($db:ident { $($rel:ident($($args:expr),*));* $(;)? })*) => {
        $( $(
            $db.insert(stringify!($rel), vec![$($crate::prelude::Value::new($args)),*]);
        )* )*
    };
}
