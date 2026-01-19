use clap::Parser;
use lalrpop_util::lalrpop_mod;
use std::fs::read_to_string;
use std::io::Result;
use std::path::PathBuf;

mod asm;
mod base;
mod debug;
mod frontend;
mod opt;
mod utils;
use crate::base::Pass;
use crate::debug::setup;
use crate::frontend::ast::Node;
use crate::frontend::semantic::Semantic;

use debug::graph::{dump_graph, GraphNode};
use debug::info;

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
    // setup logging
    // We need to keep this guard alive for the entire duration of the program.
    let _guard = setup("parse.log");
    info!("Logger initialized.");

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
    let mut result = sysy::CompUnitParser::new().parse(&input).unwrap();
    info!("\nParsed result: {:#?}", result);

    info!("Start Semantic Analysis.");
    {
        let mut pass: Box<dyn Pass> = Box::new(Semantic::new(&mut result));
        pass.run();
    }
    info!("Finish Semantic Analysis");

    // Try to dump graph to log file
    dump_graph(true, &*result, "ast");

    Ok(())
}
