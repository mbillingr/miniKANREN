//! Library of goals

use crate::core::substitution::Substitution;

#[macro_use]
pub mod combinators;
#[macro_use]
pub mod list;
#[macro_use]
pub mod primitive;

pub type StatSubs = Substitution<'static>;
