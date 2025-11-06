# ksmc

A rust-based, assembler, disassembler, and compiler for **kOS** - a scripting mod for Kerbal Space Program.

## Features

 - **Assembler & Disassembler** for `.ksm` (kOS machine code) binaries
 - **Compiler** from high-level code to kerboscipt bytecode

## Building

Requires **Rust 1.86+**

```bash
git clone https://github.com/<your-username>/ksmc.git
cd ksmc
cargo build --release
```

## Usage

Assemble a `.ksm` binary:

```bash
./target/release/ksmc input.kasm -o output.ksm
```

Disassemble a `.ksm` binary:

```bash
./target/release/ksmc input.ksm -o output.kasm --dump
```

look at `src/main.rs` for further info

## License

Licensed under [MIT License](LICENSE)

## Roadmap

 - minimal working compiler
 - pointers (needs extension of kOS)
 - expansive type system
 - optimization passes
 - compiler safety options

## NOTE

This project is not affiliated with the official [kOS](https://ksp-kos.github.io/KOS/) team.
It’s an independent project for learning and personal interest.
