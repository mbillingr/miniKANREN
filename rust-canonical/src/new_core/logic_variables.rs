use std::sync::atomic::{AtomicUsize, Ordering};

pub trait MaybeVar {
    fn try_as_var(&self) -> Option<Var>;

    fn is_var(&self) -> bool {
        self.try_as_var().is_some()
    }
}

impl MaybeVar for Var {
    fn try_as_var(&self) -> Option<Var> {
        Some(*self)
    }
}

static VAR_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    name: &'static str,
    id: usize,
}

impl Var {
    pub fn fresh(name: &'static str) -> Self {
        // The C++ reference says: "typical use for relaxed memory is incrementing counters"
        let id = VAR_COUNTER.fetch_add(1, Ordering::Relaxed);
        Var { name, id }
    }

    pub fn name(&self) -> &str {
        self.name
    }
}

impl From<&'static str> for Var {
    fn from(name: &'static str) -> Self {
        Var::fresh(name)
    }
}

#[cfg(test)]
mod unit_tests {
    use super::*;

    #[test]
    fn can_create_fresh_variables_with_name() {
        let var = Var::fresh("x");
        assert_eq!(var.name(), "x");
    }

    #[test]
    fn can_copy_variables() {
        let var_a = Var::fresh("x");
        let var_b = var_a;
        assert_eq!(var_a.name(), var_b.name());
    }

    #[test]
    fn copied_variables_are_equal() {
        let var_a = Var::fresh("x");
        let var_b = var_a;
        assert_eq!(var_a, var_b);
    }

    #[test]
    fn two_variables_with_same_name_are_not_equal() {
        let var_a = Var::fresh("x");
        let var_b = Var::fresh("x");
        assert_ne!(var_a, var_b);
    }

    #[test]
    fn can_convert_str_to_var() {
        let var: Var = "foo".into();
        assert_eq!(var.name(), "foo");
    }
}
