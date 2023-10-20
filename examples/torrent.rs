use std::{env::args, fs, process};

fn main() {
    let path = args().nth(1).unwrap_or_else(|| {
        let cmd = args().next().unwrap();
        eprintln!("usage: {cmd} TORRENT_PATH");
        process::exit(100);
    });

    let content = fs::read(&path).unwrap_or_else(|e| {
        eprintln!("failed to read file {path}: {e}");
        process::exit(101);
    });

    let result = bencode::decode(&content).unwrap_or_else(|e| {
        eprintln!("failed to decode file contents: {e}");
        process::exit(102);
    });

    println!("{result}");
}
