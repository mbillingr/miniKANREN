use crate::prelude::*;

#[test]
fn running_goal_that_fails_produces_empty_stream() {
    let stream = run!(*, q, fail());
    assert!(stream.is_empty())
}

#[test]
fn running_equality_goal_associates_value_to_fresh_variable() {
    let stream = run!(*, q, eq(q, 1));
    assert_eq!(stream, Stream::singleton(1));
}
