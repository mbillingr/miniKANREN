
pub struct Database;

impl Database {
    pub fn new() -> Self {
        Database
    }
}

#[macro_export]
macro_rules! db_rel {
    ($($rel:ident($($args:ident),*));* $(;)?) => { };
}

#[macro_export]
macro_rules! db_facts {
    ($($db:ident { $($rel:ident($($args:expr),*));* $(;)? })*) => {};
}
