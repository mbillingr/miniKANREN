use crate::core::goal::Goal;
use crate::core::logic_variable::Var;
use crate::core::value::Value;
use crate::goals::StatSubs;

/// Assert that a goal fails
pub fn fails(goal: impl Goal<StatSubs>) {
    let result = run!(1, q, goal);
    assert!(result.is_empty());
}

/// Assert that a goal succeeds at least once
pub fn succeeds(goal: impl Goal<StatSubs>) {
    let result = run!(1, q, goal);
    assert!(!result.is_empty());
}

pub fn has_unique_solution(mut solutions: impl Iterator<Item = Value>, expected: Value) {
    assert_eq!(solutions.next(), Some(expected));
    assert_eq!(solutions.next(), None);
}
