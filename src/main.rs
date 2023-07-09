use std::collections::LinkedList;

use dtensor;

fn main() {
    // let x = dtensor::primitives::Value::literal(1);
    // let y = dtensor::primitives::Value::literal(2.);
    // let z = dtensor::primitives::Value::literal(3.122341);

    // let a = x.clone() + (y.clone() - z.clone()) * y.clone() / x.clone();

    // println!("{:?}", -a.clone());
    // println!("{}", a.evaluate());
    // println!("{}", -a.evaluate());
    // println!("{}", -(-a).evaluate());

    let x = 1.0;
    println!("{:p}", &x);

    let mut b = dtensor::primitives::Value::literal(2.);
    let c = dtensor::primitives::Value::literal(0.1234);
    for i in 1..100000 {
        // println!("{}", i);
        b = -b + c.clone();
    }

    let y = 1.0;
    println!("{:p}", &y);

    // println!("{}", b.clone().id());
    // println!("{:?}", b.clone());
    println!("{}", b.evaluate());

    // let mut ll = LinkedList::new();
    // for i in 1..1000000000 {
    //     ll.push_back(1.0);
    // }

    // println!("{:#?}", ll.len());
}
