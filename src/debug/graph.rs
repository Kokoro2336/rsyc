use std::fs::File;
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
    info!("Graph DOT:\n{}", graph_dot);

    // Since graphviz-rust might enforce strict Output formats, the most robust way
    // to force "ascii" (which is a valid DOT flag but maybe not in the crate's enum)
    // is often standard Command execution:
    use std::io::Write;
    use std::process::{Command, Stdio};

    // 1. Setup 'unflatten' (The Producer)
    let mut unflatten_child = Command::new("unflatten")
        .arg("-l")
        .arg("3")
        .arg("-c")
        .arg("5")
        .stdin(Stdio::piped()) // We write to this
        .stdout(Stdio::piped()) // Its output goes to dot
        .spawn()
        .expect("Failed to spawn unflatten");

    // 2. Setup 'dot' (The Consumer)
    // It reads from unflatten, so we don't touch its stdin from Rust
    let dot_child = Command::new("dot")
        .arg("-Tsvg")
        .arg("-Gsize=20,20")
        .arg("-Gratio=fill")
        .arg("-Gsplines=curved")
        .stdin(
            unflatten_child
                .stdout
                .take() // Connect unflatten's stdout to dot's stdin
                .expect("Failed to open unflatten stdout"),
        )
        .stdout(Stdio::piped())
        .spawn()
        .expect("Failed to spawn dot");

    // 3. Write data to the *start* of the pipe (unflatten)
    if let Some(mut stdin) = unflatten_child.stdin.take() {
        stdin
            .write_all(graph_dot.as_bytes())
            .expect("Failed to write to unflatten");
    } // 'stdin' is dropped here, sending EOF to unflatten. Important!

    // 4. Wait for the chain to finish
    // We must wait for unflatten so it finishes flushing data to dot
    unflatten_child.wait().expect("unflatten failed");

    // 5. Capture the final output from dot
    let output = dot_child.wait_with_output().expect("Failed to read stdout");

    if !output.status.success() {
        let err = String::from_utf8_lossy(&output.stderr);
        panic!("dot failed: {}", err);
    }

    let graph_path = PathBuf::from(GRAPH_DUMP_DIR).join(format!("{}.svg", name));
    if !PathBuf::from(GRAPH_DUMP_DIR).exists() {
        std::fs::create_dir_all(GRAPH_DUMP_DIR).expect("Failed to create graph dump directory");
    }
    let mut file = File::create(graph_path).expect("Failed to create graph dump file");
    file.write_all(&output.stdout)
        .expect("Failed to write graph dump to file");

    info!("Graph Dumped.");
}
