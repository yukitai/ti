extern crate commander;
use commander::Commander;

use ti::app::*;

fn main() {
    let command = Commander::new()
        .version(&env!("CARGO_PKG_VERSION").to_string())
        .usage("compile -f [file_name] -o [output]")
        .usage_desc("Compile the crate or a single file")
        .option_str("-f, --file [file_name]", "set the source file", None)
        .option_str("-o, --output [output]", "set the output file", None)
        .parse_env_or_exit();

    match command.get_all_args().get(0).and_then(|s| Some(s.as_str())) {
        Some("compile") => {
            let file = ti_unwrap_or_report(command.get_str("f"), "expected option -f or --file");
            let output = if let Some(o) = command.get_str("o") {
                o
            } else {
                ti_get_output_from_file(&file)
            };
            ti_compile(&file, &output).unwrap();
        }
        Some(cmd) if cmd.starts_with("-") => {
            ti_error(&format!("unknown command `{}`", cmd));
        }
        _ => {
            todo!();
        }
    }
}
