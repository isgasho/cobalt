extern crate cobalt;

use std::env;
use std::fs::File;
use std::io::{Read, Write};

const USAGE: &str = "usage: cobalt <input file> [<output file>]";

pub fn main() {
    let mut args = env::args().skip(1);
    if let Some(ref input) = args.next() {
        let output = args.next();
        let output = output.as_ref().map(|t| &**t).unwrap_or("./out.ra");
        if args.next().is_some() {
            eprintln!("{}", USAGE);
            return;
        }

        if let Ok(mut file) = File::open(input) {
            let mut src = String::new();
            if let Err(err) = file.read_to_string(&mut src) {
                eprintln!("Error while reading {}: {:?}", input, err);
            } else {
                match cobalt::compile(input, &src) {
                    Ok(res) => {
                        if let Ok(mut file) = File::create(output) {
                            writeln!(file, "{:#?}", res).expect("error while writing to file");
                            /*writeln!(file, "v2.0 raw").expect("error while writing to file");
                            for bytes in res.chunks(4) {
                                for b in bytes {
                                    write!(file, "{:02x} ", b).expect("error while writing to file");
                                }
                                write!(file, "\n").expect("error while writing to file");
                            }*/
                            println!("successfully compiled `{}` to `{}`", input, output);
                        } else {
                            eprintln!("unable to create file: {}", output);
                        }
                    }
                    Err(_) => eprintln!("unable to compile `{}`", input),
                }
            }
        } else {
            eprintln!("unable to open file: {}", input);
        }
    } else {
        eprintln!("{}", USAGE);
    }
}
