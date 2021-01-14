use mini_kanren::prelude::*;
use mini_kanren::run;

fn main() {
    println!("Hello from an example!");
    println!("{:?}", run!(1, (a, b), eq(a, b), eq(a, 1)));
}
