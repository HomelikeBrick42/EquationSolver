use anyhow::Result;

fn main() -> Result<()> {
    while let Some(line) = {
        use std::io::Write;

        print!("> ");
        std::io::stdout().flush()?;
        std::io::stdin().lines().next().transpose()?
    } {
        let line = line.trim();
        println!("{line}");
    }
    Ok(())
}
