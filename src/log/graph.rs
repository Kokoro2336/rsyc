pub use graphviz_rust::attributes::*;
use graphviz_rust::cmd::{CommandArg, Format};
pub use graphviz_rust::dot_generator::*;
pub use graphviz_rust::dot_structures::*;
use graphviz_rust::exec;
use graphviz_rust::printer::{DotPrinter, PrinterContext};

use crate::log::info;

pub trait GraphNode: std::any::Any {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    );
}

pub fn dump_graph(directed: bool, node: &dyn GraphNode) {
    let mut stmts = vec![];
    let mut id_counter = 0;
    let mut visited: std::collections::HashSet<usize> = std::collections::HashSet::new();
    node.visit(&mut stmts, &mut id_counter, &mut visited);

    // dump the graph
    let graph = if directed {
        Graph::DiGraph {
            id: Id::Plain("Graph".to_string()),
            stmts,
            strict: false,
        }
    } else {
        Graph::Graph {
            id: Id::Plain("Graph".to_string()),
            stmts,
            strict: false,
        }
    };

    let graph_dot = {
        let raw_dot = graph.print(&mut PrinterContext::default());
        raw_dot
            .replace("]", "];\n") // End of node attributes
            .replace("}", "}\n") // End of graph
            .replace(" {", " {\n") // Start of graph
            // Attempt to split edges like "0 -> 1 2" into "0 -> 1;\n 2"
            // This regex approach is cleaner, but for simple strings, manual replace works:
            .replace(" -> ", " -> ")
    };
    info!("Graph DOT format: \n{}", graph_dot);

    // Since graphviz-rust might enforce strict Output formats, the most robust way
    // to force "ascii" (which is a valid DOT flag but maybe not in the crate's enum)
    // is often standard Command execution:
    use std::io::Write;
    use std::process::{Command, Stdio};

    // 2. Feed it to graph-easy
    let mut child = Command::new("graph-easy")
        .arg("--as=boxart") // Options: ascii, boxart, unicode
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("Failed to run 'graph-easy'. Is it installed?");

    {
        let stdin = child.stdin.as_mut().expect("Failed to open stdin");
        stdin
            .write_all(graph_dot.trim().as_bytes())
            .expect("Failed to write to stdin");
    }

    let output = child.wait_with_output().expect("Failed to read stdout");
    info!(
        "Graph Dumped: \n{}",
        String::from_utf8_lossy(&output.stdout)
    );
}
