pub mod backend;
pub mod error;
pub mod frontend;
pub mod types;
pub mod vm;

pub mod app {
    use std::{fs, io::Write};

    use crate::backend::analyzer::analyzer::Analyzer;

    use super::{
        backend::emitter::{emitter::TiEmitter, js::JSEmitter},
        error::reporter::TiReporter,
        frontend::{lexer::lexer::TiLexer, parser::parser::TiParser},
    };
    use colored::Colorize;

    pub fn ti_error(message: &str) -> ! {
        println!("{}: {}", "error".bright_red(), message.bright_white());
        std::process::exit(1)
    }

    pub fn ti_unwrap_or_report<T>(opt: Option<T>, message: &str) -> T {
        match opt {
            Some(val) => val,
            None => ti_error(message),
        }
    }

    pub fn ti_get_output_from_file(file: &str) -> String {
        let mut fns: Vec<&str> = file.split(".").collect();
        fns.pop();
        fns.push("o");
        fns.join(".")
    }

    pub fn ti_compile(fsrc: &str, fres: &str) -> std::io::Result<()> {
        let file_source = fs::read_to_string(fsrc)?;
        let source = &file_source;
        /* let source = "\
        fn sub(a, b) => a - b\r
        fn main() => sub(1, 0)\r
        main()\r
        "; */
        let mut reporter = TiReporter::new();

        println!("{}", "\n\x1b[1A\x1b[scompile ...".bright_white());

        let lexer = TiLexer::new(source);
        let tokens = lexer.tokenize(&mut reporter);

        // println!("{:#?}", tokens);
        // std::process::exit(0);

        let mut parser = TiParser::new(tokens, "(nil)", source);
        parser.parse(&mut reporter);

        println!(
            "\x1b[u{} {}",
            "compile".bright_white(),
            "done".bright_green()
        );

        // println!("{:#?}", parser.ti_ast);

        let mut analyzer = Analyzer::new(parser);

        analyzer.analyze(&mut reporter);

        let mut emitter = TiEmitter::new(analyzer.ti_parser, JSEmitter::new("  "));

        let code = emitter.emit().to_string();

        println!("\n{}", reporter);

        if !reporter.has_error() {
            /* println!(
                "{} {}\n\n{}",
                "javascript".bold().bright_cyan(),
                "output".bright_cyan(),
                code.italic().on_black()
            ) */

            let mut output = fs::File::create(fres)?;
            write!(output, "const ti_println = console.log;\r\n{}", code)?;
        }
        Ok(())
    }
}
