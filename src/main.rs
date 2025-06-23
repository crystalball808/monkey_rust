use std::{
    fs,
    io::{self, Write},
    path::PathBuf,
};

use clap::{Parser, Subcommand};
use monkey_rust::{
    Lexer,
    evaluation::{Environment, eval_statements},
    parser,
};

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
            let script = fs::read_to_string(path).expect("No such file");

            run_script(&script);
        }
    }
}

fn run_script(script: &str) {
    let mut environment = Environment::new();

    let lexer = Lexer::new(script);
    match parser::Parser::new(lexer).parse_program() {
        Ok(program) => match eval_statements(program.statements, &mut environment) {
            Ok(result) => println!("{}", result.0),
            Err(error) => println!("[Evaluation Error] {error}"),
        },
        Err(error) => println!("[Parser Error] {error}"),
    }
}

const PROMPT: &str = "\x1b[1;32m>>\x1b[0m ";
fn run_repl() -> io::Result<()> {
    let reader = io::stdin();

    print!("Welcome to the Monkey REPL!\n{PROMPT}");
    io::stdout().flush()?;
    let mut input = String::new();
    let mut environment = Environment::new();
    while let Ok(_bytes) = reader.read_line(&mut input) {
        let lexer = Lexer::new(&input);
        match parser::Parser::new(lexer).parse_program() {
            Ok(program) => match eval_statements(program.statements, &mut environment) {
                Ok(result) => println!("{}", result.0),
                Err(error) => println!("[Evaluation Error] {error}"),
            },
            Err(error) => println!("[Parser Error] {error}"),
        }

        input.clear();
        print!("{PROMPT}");
        io::stdout().flush()?;
    }

    Ok(())
}
