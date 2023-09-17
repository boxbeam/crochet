use crochet::int_parser;

fn main() {
    let num = "-0";
    println!("{}", int_parser()(num).unwrap().0);
}
