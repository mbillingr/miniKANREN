use mini_kanren::core::stream::Stream;
use mini_kanren::database::Database;
use mini_kanren::goals::StatSubs;
use mini_kanren::prelude::*;
use mini_kanren::*;
use std::sync::Arc;

// declare relations
db_rel! {
    food(f);
    drink(d);
    goes_well_with(f, d);
    is_vegetarian(f, k);
}

fn main() {
    // Construct an empty database
    // and add some facts about the world.
    let mut db = Database::new();
    db_facts! {
        db {
            food("Beef");
            food("Fish");
            food("Hamburger");
            food("Pizza");
            food("Salad");
            food("Stone soup");
            food("Veggies");

            drink("Beer");
            drink("Red wine");
            drink("Water");
            drink("White wine");

            goes_well_with("Beef", "Beer");
            goes_well_with("Beef", "Red wine");
            goes_well_with("Fish", "White wine");
            goes_well_with("Hamburger", "Beer");
            goes_well_with("Pizza", "Red wine");
            goes_well_with("Stone soup", "Water");
            goes_well_with("Veggies", "White wine");

            is_vegetarian("Beef", false);
            is_vegetarian("Fish", "some say so");
            is_vegetarian("Hamburger", "canbe");
            is_vegetarian("Pizza", "canbe");
            is_vegetarian("Salad", true);
            is_vegetarian("Stone soup", true);
            is_vegetarian("Veggies", true);
        }
    }

    // Share database
    let db = Arc::new(db);

    // run a simple query
    let food_: Vec<_> = run!(q, food(&db, q)).collect();
    println!("All the food we like: {:?}", food_);

    // run another simple query
    let veggie: Vec<_> = run!(q, is_vegetarian(&db, q, true)).collect();
    println!("Vegetarian food: {:?}", veggie);

    // run a combined query
    let wine_and_veggie: Vec<_> = run!(
        q,
        is_vegetarian(&db, q, true),
        goes_well_with(&db, q, "White wine"),
    )
    .collect();
    println!(
        "Vegetarian food that goes well with white wine: {:?}",
        wine_and_veggie
    );

    // run a query with alternatives
    let any_wine: Vec<_> = run!(
        q,
        disj! {
            goes_well_with(&db, q, "Red wine");
            goes_well_with(&db, q, "White wine")
        }
    )
    .collect();
    println!("Food that goes well with wine: {:?}", any_wine);
}
