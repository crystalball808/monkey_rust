use std::{
    collections::HashMap,
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
            let foo = path.into_os_string().into_string().unwrap();
            println!("run {foo} todo");
        }
    }
}

fn run_repl() -> io::Result<()> {
    let reader = io::stdin();
    let mut inputs = Vec::new();

    print!("Welcome to the Monkey REPL!\n>> ");
    io::stdout().flush()?;
    let mut input = String::new();
    let mut environment = Environment {
        store: HashMap::new(),
    };
    while let Ok(_bytes) = reader.read_line(&mut input) {
        inputs.push(input.clone());
        input.clear();

        let saved_input = inputs.last().unwrap();

        let lexer = Lexer::new(saved_input);
        match parser::Parser::new(lexer).parse_program() {
            Ok(program) => match eval_statements(program.statements, &mut environment) {
                Ok(result) => println!("{}", result.0),
                Err(error) => println!("[Evaluation Error] {error}"),
            },
            Err(error) => println!("[Parser Error] {error}"),
        }

        print!(">> ");
        io::stdout().flush()?;
    }

    Ok(())
}
