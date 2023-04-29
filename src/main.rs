use std::{env, fs, io::Write};

use colored::Colorize;
use ti::{
    backend::emitter::{emitter::TiEmitter, js::JSEmitter},
    error::reporter::TiReporter,
    frontend::{lexer::lexer::TiLexer, parser::parser::TiParser},
};

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_source = fs::read_to_string(&args[1]).unwrap();
    let source = &file_source;
    /* let source = "\
    fn sub(a, b) => a - b\r
    fn main() => sub(1, 0)\r
    main()\r
    "; */
    let mut reporter = TiReporter::new();

    print!("{}", "\x1b[scompile ...\n".bright_white());

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

    let mut emitter = TiEmitter::new(parser, JSEmitter::new("  "));

    let code = emitter.emit().to_string();

    println!("\n{}", reporter);

    if !reporter.has_error() {
        /* println!(
            "{} {}\n\n{}",
            "javascript".bold().bright_cyan(),
            "output".bright_cyan(),
            code.italic().on_black()
        ) */

        let mut output = fs::File::create(&args[2]).unwrap();
        write!(output, "const println = console.log;\r\n{}", code);
    }
}
