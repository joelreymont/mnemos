use std::io::{self, Read, Write};

fn main() -> anyhow::Result<()> {
    // Placeholder stdio loop: echo input until shutdown is implemented.
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf)?;
    io::stdout().write_all(buf.as_bytes())?;
    Ok(())
}
