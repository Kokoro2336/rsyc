use clap::Parser;
use lalrpop_util::lalrpop_mod;
use std::fs::read_to_string;
use std::io::{Result, Write};
use std::rc::Rc;

mod asm;
mod frontend;
mod base;
mod opt;
mod utils;
mod log;
use crate::frontend::ast::Node;

// 引用 lalrpop 生成的解析器
// 因为我们刚刚创建了 sysy.lalrpop, 所以模块名是 sysy
lalrpop_mod!(sysy);

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// enable this to transform src to koopa ir.
    #[arg(short = 'k', long = "koopa", default_value_t = false)]
    koopa: bool,

    /// enable this to transform koopa ir to riscv asm.
    #[arg(short = 'r', long = "riscv", default_value_t = false)]
    riscv: bool,

    /// positional argument for input file.
    #[arg(value_name = "INPUT")]
    input: std::path::PathBuf,

    /// use this flag to specify output file.
    #[arg(short, long, default_value = None)]
    output: std::path::PathBuf,
}

fn main() -> Result<()> {
    // preprocess argv so single-dash long-style `-koopa` becomes `--koopa`
    let args = std::env::args_os()
        .enumerate()
        .map(|(i, a)| {
            if i == 0 {
                return a;
            }
            if let Some(s) = a.to_str() {
                if s.starts_with('-') && !s.starts_with("--") && &s[1..] == "koopa" {
                    return std::ffi::OsString::from(format!("--{}", &s[1..]));
                } else if s.starts_with('-') && !s.starts_with("--") && &s[1..] == "riscv" {
                    return std::ffi::OsString::from(format!("--{}", &s[1..]));
                }
            }
            a
        })
        .collect::<Vec<_>>();

    let cli = Cli::parse_from(args);

    let input = cli.input;
    let output = cli.output;

    // 读取输入文件
    let input = read_to_string(input)?;

    // 调用 lalrpop 生成的 parser 解析输入文件
    let result = sysy::CompUnitParser::new().parse(&input);
    Ok(())
}
