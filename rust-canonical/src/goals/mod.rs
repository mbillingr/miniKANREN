//! Library of goals

#[macro_use]
pub mod combinators;
#[macro_use]
pub mod hashmap;
#[macro_use]
pub mod list;
#[macro_use]
pub mod primitive;

pub mod numbers;

use crate::core::substitution::Substitution;

pub type StatSubs = Substitution<'static>;
