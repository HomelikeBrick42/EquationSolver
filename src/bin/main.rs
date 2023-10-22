use anyhow::Result;
use equation_solver::{parser::parse, simplify_expression};

fn main() -> Result<()> {
    while let Some(line) = {
        use std::io::Write;

        print!("> ");
        std::io::stdout().flush()?;
        std::io::stdin().lines().next().transpose()?
    } {
        match parse(&line) {
            Ok(expression) => {
                println!("{expression}");
                let simplified = simplify_expression(expression);
                println!("{simplified}");
            }
            Err(message) => println!("{message}"),
        }
    }
    Ok(())
}
