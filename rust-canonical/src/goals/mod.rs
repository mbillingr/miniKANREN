//! Library of goals

use crate::core::substitution::Substitution;

pub mod list;
pub mod primitive;

pub type StatSubs = Substitution<'static>;
