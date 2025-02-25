use std::path::PathBuf;

use clap::{Parser, Subcommand};

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
            println!("Repl todo")
        }
        Commands::Run { path } => {
            let foo = path.into_os_string().into_string().unwrap();
            println!("run {foo} todo");
        }
    }
}
