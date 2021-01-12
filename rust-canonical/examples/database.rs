use mini_kanren::prelude::*;
use mini_kanren::*;
use mini_kanren::database::Database;

// declare relations
db_rel! {
    mano(p);
    womano(p);
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
        }
    }

    // run a simple query
    let men: Vec<_> = run!(q, /*mano(q)*/).collect();
    println!("All the men in the world: {:?}", men);
}
