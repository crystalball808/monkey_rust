use std::{
    cell::RefCell,
    fs,
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
            let script = fs::read_to_string(path).expect("No such file");

            run_script(&script);
        }
    }
}
fn run_script(script: &str) {
    let environment = Rc::new(RefCell::new(Environment::new()));

    let lexer = Lexer::new(script);
    match parser::Parser::new(lexer).parse_program() {
        Ok(program) => match eval_statements(program.statements, environment) {
            Ok(result) => println!("{}", result.0),
            Err(error) => println!("[Evaluation Error] {error}"),
        },
        Err(error) => println!("[Parser Error] {error}"),
    }
}

use std::cell::UnsafeCell;

fn run_repl() -> io::Result<()> {
    let reader = io::stdin();

    let inputs = UnsafeCell::new(Vec::new());
    let environment = Rc::new(RefCell::new(Environment::new()));

    print!("Welcome to the Monkey REPL!\n>> ");
    io::stdout().flush()?;
    let mut input = String::new();

    while let Ok(_bytes) = reader.read_line(&mut input) {
        unsafe {
            let inputs_mut = &mut *inputs.get();
            inputs_mut.push(input.clone());

            let inputs_ref = &*inputs.get();
            let saved_input = inputs_ref.get_unchecked(inputs_ref.len() - 1);

            let lexer = Lexer::new(saved_input);
            match parser::Parser::new(lexer).parse_program() {
                Ok(program) => match eval_statements(program.statements, environment.clone()) {
                    Ok(result) => println!("{}", result.0),
                    Err(error) => println!("[Evaluation Error] {error}"),
                },
                Err(error) => println!("[Parser Error] {error}"),
            }
        }

        print!(">> ");
        io::stdout().flush()?;
        input.clear();
    }

    Ok(())
}
