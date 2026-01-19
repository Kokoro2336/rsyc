use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

pub use graphviz_rust::attributes::*;
pub use graphviz_rust::dot_generator::*;
pub use graphviz_rust::dot_structures::*;
use graphviz_rust::printer::{DotPrinter, PrinterContext};

use crate::debug::info;

pub trait GraphNode: std::any::Any {
    fn visit(&self, stmts: &mut Vec<Stmt>, visited: &mut std::collections::HashSet<usize>);
}

const GRAPH_DUMP_DIR: &str = "./graphs";

pub fn dump_graph(directed: bool, node: &dyn GraphNode, name: &str) {
    let mut stmts = vec![];
    let mut visited: std::collections::HashSet<usize> = std::collections::HashSet::new();
    node.visit(&mut stmts, &mut visited);

    // dump the graph
    let graph = if directed {
        Graph::DiGraph {
            id: Id::Plain("\"Graph\"".to_string()),
            stmts,
            strict: false,
        }
    } else {
        Graph::Graph {
            id: Id::Plain("\"Graph\"".to_string()),
            stmts,
            strict: false,
        }
    };

    let mut ctx = PrinterContext::default();
    ctx.with_semi();
    let graph_dot = graph.print(&mut ctx);
    info!("Graph DOT format: \n{}", graph_dot);

    // Since graphviz-rust might enforce strict Output formats, the most robust way
    // to force "ascii" (which is a valid DOT flag but maybe not in the crate's enum)
    // is often standard Command execution:
    use std::io::Write;
    use std::process::{Command, Stdio};

    // 2. Feed it to graph-easy
    let mut child = Command::new("dot")
        // set type
        .arg("-Tsvg") // Options: ascii, boxart, unicode
        // set size
        .arg("-Gsize=10,10")
        // fill the canvas
        .arg("-Gratio=fill")
        // use curved edges
        .arg("-Gsplines=curved")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("Failed to run 'graph-easy'. Is it installed?");

    {
        let stdin = child.stdin.as_mut().expect("Failed to open stdin");
        stdin
            .write_all(graph_dot.as_bytes())
            .expect("Failed to write to stdin");
    }
    let output = child.wait_with_output().expect("Failed to read stdout");
    // Check for errors in the tool's execution
    if !output.status.success() {
        let err = String::from_utf8_lossy(&output.stderr);
        panic!("graph-easy failed: {}", err);
    }

    let graph_path = PathBuf::from(GRAPH_DUMP_DIR).join(format!("{}.html", name));
    if !PathBuf::from(GRAPH_DUMP_DIR).exists() {
        std::fs::create_dir_all(GRAPH_DUMP_DIR).expect("Failed to create graph dump directory");
    }
    let mut file = File::create(graph_path).expect("Failed to create graph dump file");
    file.write_all(&output.stdout)
        .expect("Failed to write graph dump to file");

    info!("Graph Dumped.");
}
