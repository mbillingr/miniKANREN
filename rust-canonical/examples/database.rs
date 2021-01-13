use mini_kanren::core::stream::Stream;
use mini_kanren::database::Database;
use mini_kanren::goals::StatSubs;
use mini_kanren::prelude::*;
use mini_kanren::*;
use std::sync::Arc;

// declare relations
db_rel! {
    mano(p);
    womano(p);
    vitalo(p, v);
}

fn main() {
    // Construct an empty database
    // and add some facts about the world.
    let mut db = Database::new();
    db_facts! {
        db {
            mano("Adam");
            womano("Eve");
            mano("Kain");
            mano("Abel");

            vitalo("Adam", "dead");
            vitalo("Eve", "dead");
            vitalo("Kain", "dead");
            vitalo("Abel", "dead");
        }
    }

    // Share database
    let db = Arc::new(db);

    // run a simple query
    let men: Vec<_> = run!(q, mano(&db, q)).collect();
    println!("All the men in the world: {:?}", men);

    // run another simple query
    let men: Vec<_> = run!(q, vitalo(&db, q, "dead")).collect();
    println!("Dead people: {:?}", men);

    // run a combined query
    let dead_men: Vec<_> = run!(q, mano(&db, q), vitalo(&db, q, "dead")).collect();
    println!("Dead people men: {:?}", dead_men);
}
