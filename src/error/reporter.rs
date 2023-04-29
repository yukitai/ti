use std::fmt::Display;

use colored::Colorize;

use super::error::TiError;

pub struct TiReporter<'a> {
    errors: Vec<TiError<'a>>,
}

impl<'a> TiReporter<'a> {
    pub fn new() -> Self {
        Self { errors: Vec::new() }
    }

    pub fn report(&mut self, error: TiError<'a>) {
        self.errors.push(error);
    }

    pub fn has_error(&self) -> bool {
        let mut error_count = 0usize;
        for error in &self.errors {
            if error.is_error() {
                error_count += 1;
            }
        }
        error_count > 0
    }
}

impl Display for TiReporter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut error_count = 0usize;
        for error in &self.errors {
            if error.is_error() {
                error_count += 1;
            }
            write!(f, "{}\n\n", error)?;
        }
        if error_count > 0 {
            writeln!(
                f,
                "{}: {} {} {}",
                "Error".bright_red(),
                format!("total {}", error_count).bright_white(),
                format!("error{}", if error_count > 1 { "s" } else { "" }).bright_white(),
                "generated".bright_white()
            )?;
        }
        Ok(())
    }
}
