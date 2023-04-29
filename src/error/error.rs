use std::fmt::Display;

use colored::Colorize;

use crate::types::types::TiType;

pub type TiWhere = (usize, usize, usize);

pub fn ti_where_expand(lhs: &TiWhere, rhs: &TiWhere) -> TiWhere {
    (lhs.0, lhs.1, rhs.2)
}

type TiBounds = (usize, usize);

pub enum TiSyntaxError {
    LexerError(String),
    ParserError(String),
}

impl Display for TiSyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LexerError(err) => {
                write!(f, "{} {}", "(lexer)".bright_yellow(), err.bright_white())?;
            }
            Self::ParserError(err) => {
                write!(f, "{} {}", "(parser)".bright_yellow(), err.bright_white())?;
            }
        }
        Ok(())
    }
}

pub enum TiErrorType {
    TypeError {
        ti_expected_type: TiType,
        ti_given_type: TiType,
    },
    NilError {},
    SyntaxError {
        syntax_error: TiSyntaxError,
    },
    Warning {
        message: String,
    },
    Note {
        message: String,
    },
}

pub struct TiError<'a> {
    ti_error: TiErrorType,
    ti_where: TiWhere,
    ti_source: &'a str,
}

impl<'a> TiError<'a> {
    pub fn new(ti_error: TiErrorType, ti_where: TiWhere, ti_source: &'a str) -> Self {
        TiError {
            ti_error,
            ti_where,
            ti_source,
        }
    }

    fn bounds(&self) -> TiBounds {
        let mut left = self.ti_where.1;
        let mut right = self.ti_where.2;
        let len = self.ti_source.len();
        while left > 0
            && match &self.ti_source[left..=left] {
                "\n" | "\r" => {
                    left += 1;
                    false
                }
                _ => true,
            }
        {
            left -= 1;
        }
        while right < len
            && match &self.ti_source[right..=right] {
                "\n" | "\r" => false,
                _ => true,
            }
        {
            right += 1;
        }
        (left, right)
    }

    pub fn is_error(&self) -> bool {
        match self.ti_error {
            TiErrorType::Note { .. } => false,
            TiErrorType::Warning { .. } => false,
            _ => true,
        }
    }
}

impl Display for TiError<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bounds = self.bounds();
        let session = &self.ti_source[bounds.0..bounds.1];
        let position =
            format!("[::{}:{}]", self.ti_where.0, self.ti_where.1 - bounds.0 + 1).bright_cyan();
        match &self.ti_error {
            TiErrorType::Note { message } => {
                write!(
                    f,
                    "{}: {} {} {}\n\n",
                    "Note".bright_magenta(),
                    message.bright_white(),
                    "=>".bright_cyan(),
                    position
                )?;
                write!(
                    f,
                    "{} {} {}\n",
                    format!("{:>5}", self.ti_where.0).bright_cyan(),
                    "|".bright_cyan(),
                    session
                )?;
                write!(
                    f,
                    "        {}{}",
                    " ".repeat(self.ti_where.1 - bounds.0),
                    "~".repeat(self.ti_where.2 - self.ti_where.1)
                        .bright_yellow()
                )?;
            }
            TiErrorType::Warning { message } => {
                write!(
                    f,
                    "{}: {} {} {}\n\n",
                    "TypeError".bright_yellow(),
                    message.bright_white(),
                    "=>".bright_cyan(),
                    position
                )?;
                write!(
                    f,
                    "{} {} {}\n",
                    format!("{:>5}", self.ti_where.0).bright_cyan(),
                    "|".bright_cyan(),
                    session
                )?;
                write!(
                    f,
                    "        {}{}",
                    " ".repeat(self.ti_where.1 - bounds.0),
                    "~".repeat(self.ti_where.2 - self.ti_where.1)
                        .bright_yellow()
                )?;
            }
            TiErrorType::TypeError {
                ti_expected_type,
                ti_given_type,
            } => {
                write!(
                    f,
                    "{}: {} {} {}\n\n",
                    "TypeError".bright_red(),
                    format!(
                        "mismatched type {:?} and {:?}",
                        ti_given_type, ti_expected_type
                    )
                    .bright_white(),
                    "=>".bright_cyan(),
                    position
                )?;
                write!(
                    f,
                    "{} {} {}\n",
                    format!("{:>5}", self.ti_where.0).bright_cyan(),
                    "|".bright_cyan(),
                    session
                )?;
                write!(
                    f,
                    "        {}{}",
                    " ".repeat(self.ti_where.1 - bounds.0),
                    "~".repeat(self.ti_where.2 - self.ti_where.1)
                        .bright_yellow()
                )?;
            }
            TiErrorType::NilError {} => {
                write!(
                    f,
                    "{}: {} {} {}\n\n",
                    "NilError".bright_red(),
                    "nil values should be carefully handled".bright_white(),
                    "=>".bright_cyan(),
                    position
                )?;
                write!(
                    f,
                    "{} {} {}\n",
                    format!("{:>5}", self.ti_where.0).bright_cyan(),
                    "|".bright_cyan(),
                    session
                )?;
                write!(
                    f,
                    "        {}{}",
                    " ".repeat(self.ti_where.1 - bounds.0),
                    "~".repeat(self.ti_where.2 - self.ti_where.1)
                        .bright_yellow()
                )?;
            }
            TiErrorType::SyntaxError { syntax_error } => {
                write!(
                    f,
                    "{}: {} {} {}\n\n",
                    "SyntaxError".bright_red(),
                    syntax_error,
                    "=>".bright_cyan(),
                    position
                )?;
                write!(
                    f,
                    "{} {} {}\n",
                    format!("{:>5}", self.ti_where.0).bright_cyan(),
                    "|".bright_cyan(),
                    session
                )?;
                write!(
                    f,
                    "        {}{}",
                    " ".repeat(self.ti_where.1 - bounds.0),
                    "~".repeat(self.ti_where.2 - self.ti_where.1)
                        .bright_yellow(),
                )?;
            }
        }
        Ok(())
    }
}
