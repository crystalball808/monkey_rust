use std::{
    cell::RefCell,
    collections::HashMap,
    io::{self, Write},
    path::PathBuf,
    rc::Rc,
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
    let mut inputs = RefCell::new(Vec::new());

    print!("Welcome to the Monkey REPL!\n>> ");
    io::stdout().flush()?;
    let mut input = RefCell::new(String::new());
    let environment = Rc::new(RefCell::new(Environment::new()));
    while let Ok(_bytes) = reader.read_line(&mut input.borrow_mut()) {
        inputs.borrow_mut().push(input.borrow().clone());
        input.borrow_mut().clear();

        let foo = inputs.borrow();
        let saved_input = foo.last().unwrap();

        let lexer = Lexer::new(&saved_input);
        match parser::Parser::new(lexer).parse_program() {
            Ok(program) => match eval_statements(program.statements, environment.clone()) {
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
