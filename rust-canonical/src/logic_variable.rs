use std::sync::atomic::{AtomicUsize, Ordering};

static VAR_COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    name: &'static str,
    id: usize,
}

impl Var {
    pub fn new(name: &'static str) -> Self {
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
        Var::new(name)
    }
}

impl std::fmt::Debug for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct ReifiedVar(pub usize);

impl std::fmt::Debug for ReifiedVar {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "_{}", self.0)
    }
}

#[cfg(test)]
mod unit_tests {
    use super::*;

    #[test]
    fn can_create_fresh_variables_with_name() {
        let var = Var::new("x");
        assert_eq!(var.name(), "x");
    }

    #[test]
    fn can_copy_variables() {
        let var_a = Var::new("x");
        let var_b = var_a;
        assert_eq!(var_a.name(), var_b.name());
    }

    #[test]
    fn copied_variables_are_equal() {
        let var_a = Var::new("x");
        let var_b = var_a;
        assert_eq!(var_a, var_b);
    }

    #[test]
    fn two_variables_with_same_name_are_not_equal() {
        let var_a = Var::new("x");
        let var_b = Var::new("x");
        assert_ne!(var_a, var_b);
    }

    #[test]
    fn can_convert_str_to_var() {
        let var: Var = "foo".into();
        assert_eq!(var.name(), "foo");
    }
}
