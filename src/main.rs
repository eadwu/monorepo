mod Parser;

use std::env;
use structopt::StructOpt;

fn main() {
    let args = Parser::BoxpubOptions::from_args();
    println!("{:?}", args);

    let path = env::current_dir().unwrap();
    println!("The current directory is {}", path.display());
}
