use std::{
    io::{self, Write},
    path::PathBuf,
};

use clap::{Parser, Subcommand};
use monkey_rust::{Lexer, evaluation::eval_statements, parser};

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Launch a REPL
    Repl,
    Run {
        path: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();
    let Some(command) = cli.command else {
        return;
    };

    match command {
        Commands::Repl => {
            if let Err(error) = run_repl() {
                eprintln!("REPL crashed: {error}")
            }
        }
        Commands::Run { path } => {
            let foo = path.into_os_string().into_string().unwrap();
            println!("run {foo} todo");
        }
    }
}

fn run_repl() -> io::Result<()> {
    let reader = io::stdin();

    print!("Welcome to the Monkey REPL!\n>> ");
    io::stdout().flush()?;
    let mut input = String::new();
    while let Ok(_bytes) = reader.read_line(&mut input) {
        let lexer = Lexer::new(&input);
        match parser::Parser::new(lexer).parse_program() {
            Ok(program) => match eval_statements(program.statements) {
                Ok(result) => println!("{result}"),
                Err(error) => println!("[Evaluation Error] {error}"),
            },
            Err(error) => println!("[Parser Error] {error}"),
        }

        input.clear();
        print!(">> ");
        io::stdout().flush()?;
    }

    Ok(())
}
