use std::path::Path;
use ksmc::{assembler::assemble, disassemble::disassemble};
use ksmc::Logger;

fn main() {
    // system args
    // "path" : required
    // -o "path" : Optional - same location if None
    // -v : Optional verbosity
    // -dump : Optional disassemble instead of assemble

    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: kasm <path> [-o <path>] [-v] [-dump]");
        std::process::exit(1);
    }

    let input = args.get(1).cloned().unwrap_or("".into());
    let mut output: Option<String> = None;
    let mut verbosity = 0;
    let mut dump = false;

    let mut i = 2;
    while i < args.len() {
        match args[i].as_str() {
            "-o" => {
                if i + 1 < args.len() {
                    output = Some(args[i + 1].clone());
                    i += 1;
                } else {
                    eprintln!("Error: -o requires a file path");
                    std::process::exit(1);
                }
            }
            "-v"   if verbosity == 0 => verbosity = 1,
            "-vv"  if verbosity == 0 => verbosity = 2,
            "-vvv" if verbosity == 0 => verbosity = 3,
            "--dump" => dump = true,
            _ => {}
        }
        i += 1;
    }

    if input.is_empty() {
        eprintln!("Error: no input file specified");
        std::process::exit(1);
    }

    let input_path = Path::new(&input);
    if !input_path.exists() {
        eprintln!("Error: file not found: {}", input);
        std::process::exit(1);
    }

    let ext = input_path.extension().and_then(|s| s.to_str()).unwrap_or("").to_lowercase();
    let is_ksm = ext == "ksm";

    let output_path = output.unwrap_or_else(|| {
        let stem = input_path.file_stem().unwrap().to_str().unwrap();
        let mut new_path = input_path.with_file_name(stem);
        if dump || is_ksm {
            new_path.set_extension("kasm");
        } else {
            new_path.set_extension("ksm");
        }
        new_path.to_string_lossy().into_owned()
    });

    let logger = Logger::new(verbosity);

    if dump || is_ksm {
        logger.log(1, format!("Disassembling {} → {}", input, output_path));
        match disassemble(input, output_path) {
            Ok(_) => logger.log(1, format!("Done.")),
            Err(e) => eprintln!("{}", e),
        };
    } else {
        logger.log(1, format!("Assembling {} → {}", input, output_path));
        match assemble(input, output_path, &logger) {
            Ok(_) => logger.log(1, format!("Done.")),
            Err(e) => eprintln!("{}", e),
        };
    }
}

