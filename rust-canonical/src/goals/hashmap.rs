use crate::core::structure::Structure;
use crate::{Substitution, Value, Var};
use std::any::Any;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use std::sync::Arc;

#[macro_export]
macro_rules! hashmap {
    () => {
        std::collections::HashMap::new()
    };
}

impl<T: 'static + Debug + Eq + Hash> Structure for HashMap<T, Value> {
    fn occurs<'s>(&self, _x: &Var, _s: &Substitution<'s>) -> bool {
        //s.occurs(x, &self.first) || s.occurs(x, &self.second)
        unimplemented!()
    }

    fn unify<'s>(&self, v: &Value, mut s: Substitution<'s>) -> Option<Substitution<'s>> {
        let other = v.downcast_ref::<Self>()?;
        for (_, v1, v2) in double_iterator(self, other) {
            s = s.unify(v1?, v2?)?;
        }
        Some(s)
    }

    fn walk_star(self: Arc<Self>, _s: &Substitution<'_>) -> Value {
        //(s.walk_star(&self.first), s.walk_star(&self.second)).into()
        unimplemented!()
    }

    fn reify_s<'s>(&self, _s: Substitution<'s>) -> Substitution<'s> {
        //s.reify_s(&self.first).reify_s(&self.second)
        unimplemented!()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }

    fn eqv(&self, other: &Value) -> bool {
        other
            .downcast_ref::<Self>()
            .map(|v| double_iterator(self, v).all(|(_, v1, v2)| v1 == v2))
            .unwrap_or(false)
    }
}

fn double_iterator<'a, K: Eq + Hash, V>(
    first: &'a HashMap<K, V>,
    second: &'a HashMap<K, V>,
) -> impl Iterator<Item = (&'a K, Option<&'a V>, Option<&'a V>)> {
    let keys_in_first = first.iter().map(move |(k, v)| (k, Some(v), second.get(k)));
    let keys_only_in_second = second
        .iter()
        .filter(move |(k, _)| !first.contains_key(k))
        .map(|(k, v)| (k, None, Some(v)));

    keys_in_first.chain(keys_only_in_second)
}

#[cfg(test)]
mod tests {
    use crate::core::value::Value;
    use crate::testing::{ has_unique_solution,};
    use crate::*;
    use std::collections::HashMap;

    #[test]
    fn two_empty_hashmaps_are_unifiable() {
        let a: HashMap<(), Value> = hashmap![];
        let b: HashMap<(), Value> = hashmap![];
        has_unique_solution(run!(q, eq(a, b)), Value::rv(0));
    }
}
