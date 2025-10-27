# ksmc
An assembler and disassembler for kRISC, plus a compiler for a language intended to replace Kerboscript — implemented in Rust.

[![crates.io](https://img.shields.io/badge/crates.io-ksmc-orange.svg)](https://crates.io)
[![Build Status](https://img.shields.io/badge/build-passing-brightgreen.svg)](https://github.com/DigitalCodeCrafter/ksmc/actions)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

Table of contents
- Features
- Quick start
- Installation
- Usage
  - Assembler
  - Disassembler
  - Compiler
- Examples
- Contributing
- License
- Contact

Features
- Assemble kRISC assembly into binary machine code.
- Disassemble kRISC binaries back to human-readable assembly.
- Compile a higher-level Kerboscript-like language to kRISC assembly/binary.
- Single Rust-based CLI (ksmc) with subcommands for each tool.
- Designed for correctness, readability of output, and developer ergonomics.

Quick start
1. Build from source:
   cargo build --release
2. Run the assembler on an assembly file:
   ./target/release/ksmc assemble examples/hello.krisc -o hello.bin
3. Disassemble a binary:
   ./target/release/ksmc disassemble hello.bin -o out.asm
4. Compile a Kerboscript-replacement script:
   ./target/release/ksmc compile scripts/example.ks -o program.bin

Installation
- From source:
  git clone https://github.com/DigitalCodeCrafter/ksmc.git
  cd ksmc
  cargo build --release
  # binary will be in target/release/ksmc

- Install locally (if published as a crate or for local path):
  cargo install --path .

- Prebuilt releases:
  Check the Releases page for platform builds (if available).

Usage
Note: The exact flags and subcommand names in your repo may differ — adjust these examples to match the CLI implemented in ksmc.

Assembler
- Basic:
  ksmc assemble input.krisc -o output.bin
- Read from stdin and write to stdout:
  cat input.krisc | ksmc assemble - > output.bin
- Verbose / debug output (example):
  ksmc assemble input.krisc -o output.bin --verbose

Disassembler
- Basic:
  ksmc disassemble input.bin -o output.krisc
- Show raw bytes alongside instructions:
  ksmc disassemble input.bin --hex

Compiler (Kerboscript replacement -> kRISC)
- Compile a script to binary:
  ksmc compile script.ks -o program.bin
- Compile and emit assembly:
  ksmc compile script.ks --emit-asm -o program.krisc

Global help
- Show top-level help and subcommand docs:
  ksmc --help
  ksmc assemble --help
  ksmc disassemble --help
  ksmc compile --help

Examples
Example kRISC assembly (examples/hello.krisc)
; comments start with semicolon
LOAD R0, 0x100
ADD  R1, R0, #5
STORE R1, 0x200
HALT

Assemble:
ksmc assemble examples/hello.krisc -o examples/hello.bin

Disassemble:
ksmc disassemble examples/hello.bin -o examples/hello_out.krisc

Example high-level script (scripts/example.ks)
print("Hello, kRISC!")
set throttle 0.5
stage

Compile:
ksmc compile scripts/example.ks -o scripts/example.bin
ksmc compile scripts/example.ks --emit-asm -o scripts/example.krisc

Output locations and formats
- The assembler produces raw kRISC binary files by default.
- The disassembler emits plain-text assembly that should re-assemble to the same binary (modulo assembler formatting and labels).
- The compiler can emit either assembly (for inspection) or binary (for running on kRISC).

Project layout (suggested)
- src/                 — Rust source
- examples/            — example assembly and binaries
- scripts/             — high-level scripts to compile
- tests/               — integration tests and sample programs
- docs/                — design notes and ISA reference

Contributing
- Issues: open issues for bugs, feature requests, and questions.
- Pull requests:
  - Fork the repo and create a feature branch.
  - Run cargo fmt and cargo clippy; include tests when applicable.
  - Describe changes, motivation, and any backward-incompatible effects in the PR.
- Code style: follow Rust idioms; keep public APIs documented with rustdoc.
- Tests: include unit tests and, when possible, integration tests that confirm assembled binaries round-trip with the disassembler.

Development tips
- Use cargo test to run tests.
- Use cargo fmt and cargo clippy for formatting and linting.
- Add example files to examples/ and test them in CI to prevent regressions.

License
- Add your chosen license to the repository (LICENSE file). Common choices:
  - MIT
  - Apache-2.0
  - MIT OR Apache-2.0
Replace the license badge above and the LICENSE file accordingly.

Changelog
- Keep a concise changelog or use GitHub Releases to summarize notable changes per release.

Contact & support
- Open an issue for bug reports and feature requests.
- For questions, contact the maintainers via GitHub (open an issue or discussion).

Notes / TODO
- Add an ISA reference and encoding table to docs/.
- Add CI that runs tests, examples, and builds release artifacts.
- Add example programs and an emulator harness (if desired) for easier testing.
