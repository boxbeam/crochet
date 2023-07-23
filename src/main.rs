use crochet::int_parser;

fn main() {
    let num = "-1234";
    println!("{}", int_parser()(num).unwrap().0);
}
