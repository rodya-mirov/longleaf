#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate lazy_static;

use std::io::{self, prelude::*};

mod parser;
mod vm;

#[macro_use]
mod macros;

use parser::ReplInput;
use vm::VM;

fn main() {
    println!("Welcome to the longleaf REPL. For help, type 'help'.");
    print!("> ");

    let mut vm = VM::new();

    let _ = io::stdout().flush();

    'main_loop: for line in io::stdin().lock().lines() {
        let line = line.unwrap();

        let user_command = parser::parse_repl_input(&line);

        match user_command {
            Err(e) => println!("Could not parse repl input; error: {}", e),
            Ok(ReplInput::Help) => {
                print_help();
            }
            Ok(ReplInput::Exit) => {
                print_quit_dialogue();
                break 'main_loop;
            }
            Ok(ReplInput::VarDefn(name, expr_node)) => match timed!(vm.evaluate_expr(expr_node)) {
                Ok(val) => {
                    let def_result = vm.define_variable(&name, val);
                    if let Err(e) = def_result {
                        println!("Error defining variable {}: {:?}", name, e);
                    }
                }
                Err(e) => {
                    println!("Error evaluating expression: {:?}", e);
                }
            },
            Ok(ReplInput::Expr(expr_node)) => match timed!(vm.evaluate_expr(expr_node)) {
                Ok(val) => {
                    println!("{:?}", val);
                }
                Err(e) => {
                    println!("Error evaluating expression: {:?}", e);
                }
            },
        }

        print!("> ");

        let _ = io::stdout().flush();
    }
}

fn print_help() {
    println!("Available commands:");
    println!("  help: prints this dialogue");
    println!("  exit: terminates the main loop and stops the process (same as quit)");
    println!("  quit: terminates the main loop and stops the process (same as exit)");
    println!();
    println!("You can also type a mathematical expression and it will evaluate it, for example, `[2, 3]+[4, 5]` (prints `[6, 8]`)");
}

fn print_quit_dialogue() {
    println!("Shutting down, goodbye");
}
