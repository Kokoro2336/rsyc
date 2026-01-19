use crate::base::r#type::Type;
use crate::log::graph::{self, attr, id, Attribute, Edge, EdgeTy, GraphNode, Id, NodeId, Stmt, Vertex};
use crate::frontend::ast::*;

/* GraphNode implementations for AST nodes */
impl GraphNode for FnDecl {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![
                attr!("label", {
                    format!(
                        "\"FnDecl: {}() | Params: {:?} | Return Type: {:?}\"",
                        self.name, self.params, self.return_type
                    )
                }),
                attr!("shape", "box"),
            ],
        )));
        // Add to visited
        visited.insert(ptr);

        let to = (&*self.body as *const dyn Node) as *const () as usize;
        if visited.contains(&to) {
            return;
        }

        stmts.push(Stmt::Edge(Edge {
            ty: EdgeTy::Pair(
                Vertex::N(NodeId(Id::Plain(id_counter.to_string()), None)),
                Vertex::N(NodeId(
                    Id::Plain({
                        *id_counter += 1;
                        id_counter.to_string()
                    }),
                    None,
                )),
            ),
            attributes: vec![],
        }));

        *id_counter += 1;
        self.body.visit(stmts, id_counter, visited);
    }
}

impl GraphNode for Break {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![attr!("label", "\"Break\""), attr!("shape", "diamond")],
        )));
        // Add to visited
        visited.insert(ptr);
    }
}

impl GraphNode for Continue {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![attr!("label", "\"Continue\""), attr!("shape", "diamond")],
        )));
        // Add to visited
        visited.insert(ptr);
    }
}

impl GraphNode for Return {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![attr!("label", "\"Return\""), attr!("shape", "oval")],
        )));
        // Add to visited
        visited.insert(ptr);

        let ptr = &**self.0.as_ref().unwrap() as *const dyn Node as *const () as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Edge(Edge {
            ty: EdgeTy::Pair(
                Vertex::N(NodeId(Id::Plain(id_counter.to_string()), None)),
                Vertex::N(NodeId(
                    Id::Plain({
                        *id_counter += 1;
                        id_counter.to_string()
                    }),
                    None,
                )),
            ),
            attributes: vec![],
        }));

        *id_counter += 1;
        self.0.as_ref().unwrap().visit(stmts, id_counter, visited);
    }
}

impl GraphNode for Block {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![attr!("label", "\"Block\""), attr!("shape", "box")],
        )));
        // Add to visited
        visited.insert(ptr);

        let old_counter = *id_counter;
        for stmt in &self.statements {
            let to = (&**stmt as *const dyn Node) as *const () as usize;
            if visited.contains(&to) {
                continue;
            }

            stmts.push(Stmt::Edge(Edge {
                ty: EdgeTy::Pair(
                    Vertex::N(NodeId(Id::Plain(old_counter.to_string()), None)),
                    Vertex::N(NodeId(
                        Id::Plain({
                            *id_counter += 1;
                            id_counter.to_string()
                        }),
                        None,
                    )),
                ),
                attributes: vec![],
            }));

            *id_counter += 1;
            stmt.visit(stmts, id_counter, visited);
        }
    }
}

impl GraphNode for Assign {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![attr!("label", "\"Assign\""), attr!("shape", "box")],
        )));
        // Add to visited
        visited.insert(ptr);
        let old_counter = *id_counter;

        let to_lhs = (&*self.lhs as *const dyn Node) as *const () as usize;
        if !visited.contains(&to_lhs) {
            stmts.push(Stmt::Edge(Edge {
                ty: EdgeTy::Pair(
                    Vertex::N(NodeId(Id::Plain(old_counter.to_string()), None)),
                    Vertex::N(NodeId(
                        Id::Plain({
                            *id_counter += 1;
                            id_counter.to_string()
                        }),
                        None,
                    )),
                ),
                attributes: vec![],
            }));

            *id_counter += 1;
            self.lhs.visit(stmts, id_counter, visited);
        }

        let to_rhs = (&*self.rhs as *const dyn Node) as *const () as usize;
        if !visited.contains(&to_rhs) {
            stmts.push(Stmt::Edge(Edge {
                ty: EdgeTy::Pair(
                    Vertex::N(NodeId(Id::Plain(old_counter.to_string()), None)),
                    Vertex::N(NodeId(
                        Id::Plain({
                            *id_counter += 1;
                            id_counter.to_string()
                        }),
                        None,
                    )),
                ),
                attributes: vec![],
            }));

            *id_counter += 1;
            self.rhs.visit(stmts, id_counter, visited);
        }
    }
}

impl GraphNode for If {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![attr!("label", "\"If\""), attr!("shape", "diamond")],
        )));
        // Add to visited
        visited.insert(ptr);
        let old_counter = *id_counter;

        let to_cond = (&*self.condition as *const dyn Node) as *const () as usize;
        if visited.contains(&to_cond) {
            return;
        }

        stmts.push(Stmt::Edge(Edge {
            ty: EdgeTy::Pair(
                Vertex::N(NodeId(Id::Plain(old_counter.to_string()), None)),
                Vertex::N(NodeId(
                    Id::Plain({
                        *id_counter += 1;
                        id_counter.to_string()
                    }),
                    None,
                )),
            ),
            attributes: vec![],
        }));

        *id_counter += 1;
        self.condition.visit(stmts, id_counter, visited);

        let to_then = (&*self.then_block as *const dyn Node) as *const () as usize;
        if visited.contains(&to_then) {
            return;
        }

        stmts.push(Stmt::Edge(Edge {
            ty: EdgeTy::Pair(
                Vertex::N(NodeId(Id::Plain(old_counter.to_string()), None)),
                Vertex::N(NodeId(
                    Id::Plain({
                        *id_counter += 1;
                        id_counter.to_string()
                    }),
                    None,
                )),
            ),
            attributes: vec![],
        }));

        *id_counter += 1;
        self.then_block.visit(stmts, id_counter, visited);

        if let Some(else_block) = &self.else_block {
            let to_else = (&**else_block as *const dyn Node) as *const () as usize;
            if visited.contains(&to_else) {
                return;
            }

            stmts.push(Stmt::Edge(Edge {
                ty: EdgeTy::Pair(
                    Vertex::N(NodeId(Id::Plain(old_counter.to_string()), None)),
                    Vertex::N(NodeId(
                        Id::Plain({
                            *id_counter += 1;
                            id_counter.to_string()
                        }),
                        None,
                    )),
                ),
                attributes: vec![],
            }));

            *id_counter += 1;
            else_block.visit(stmts, id_counter, visited);
        }
    }
}

impl GraphNode for While {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![attr!("label", "\"While\""), attr!("shape", "diamond")],
        )));
        // Add to visited
        visited.insert(ptr);
        let old_counter = *id_counter;

        let to_cond = (&*self.condition as *const dyn Node) as *const () as usize;
        if visited.contains(&to_cond) {
            return;
        }

        stmts.push(Stmt::Edge(Edge {
            ty: EdgeTy::Pair(
                Vertex::N(NodeId(Id::Plain(old_counter.to_string()), None)),
                Vertex::N(NodeId(
                    Id::Plain({
                        *id_counter += 1;
                        id_counter.to_string()
                    }),
                    None,
                )),
            ),
            attributes: vec![],
        }));

        *id_counter += 1;
        self.condition.visit(stmts, id_counter, visited);

        let to_body = (&*self.body as *const dyn Node) as *const () as usize;
        if visited.contains(&to_body) {
            return;
        }

        stmts.push(Stmt::Edge(Edge {
            ty: EdgeTy::Pair(
                Vertex::N(NodeId(Id::Plain(old_counter.to_string()), None)),
                Vertex::N(NodeId(
                    Id::Plain({
                        *id_counter += 1;
                        id_counter.to_string()
                    }),
                    None,
                )),
            ),
            attributes: vec![],
        }));

        *id_counter += 1;
        self.body.visit(stmts, id_counter, visited);
    }
}

impl GraphNode for BinaryOp {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![
                attr!("label", { format!("\"BinaryOp: {:?}\"", self.op) }),
                attr!("shape", "box"),
            ],
        )));
        // Add to visited
        visited.insert(ptr);
        let old_counter = *id_counter;

        let to_lhs = (&*self.lhs as *const dyn Node) as *const () as usize;
        if visited.contains(&to_lhs) {
            return;
        }

        stmts.push(Stmt::Edge(Edge {
            ty: EdgeTy::Pair(
                Vertex::N(NodeId(Id::Plain(old_counter.to_string()), None)),
                Vertex::N(NodeId(
                    Id::Plain({
                        *id_counter += 1;
                        id_counter.to_string()
                    }),
                    None,
                )),
            ),
            attributes: vec![],
        }));

        *id_counter += 1;
        self.lhs.visit(stmts, id_counter, visited);

        let to_rhs = (&*self.rhs as *const dyn Node) as *const () as usize;
        if visited.contains(&to_rhs) {
            return;
        }

        stmts.push(Stmt::Edge(Edge {
            ty: EdgeTy::Pair(
                Vertex::N(NodeId(Id::Plain(old_counter.to_string()), None)),
                Vertex::N(NodeId(
                    Id::Plain({
                        *id_counter += 1;
                        id_counter.to_string()
                    }),
                    None,
                )),
            ),
            attributes: vec![],
        }));

        *id_counter += 1;
        self.rhs.visit(stmts, id_counter, visited);
    }
}

impl GraphNode for UnaryOp {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![
                attr!("label", { format!("\"UnaryOp: {:?}\"", self.op) }),
                attr!("shape", "box"),
            ],
        )));
        // Add to visited
        visited.insert(ptr);
        let old_counter = *id_counter;

        let to_operand = (&*self.operand as *const dyn Node) as *const () as usize;
        if visited.contains(&to_operand) {
            return;
        }

        stmts.push(Stmt::Edge(Edge {
            ty: EdgeTy::Pair(
                Vertex::N(NodeId(Id::Plain(old_counter.to_string()), None)),
                Vertex::N(NodeId(
                    Id::Plain({
                        *id_counter += 1;
                        id_counter.to_string()
                    }),
                    None,
                )),
            ),
            attributes: vec![],
        }));

        *id_counter += 1;
        self.operand.visit(stmts, id_counter, visited);
    }
}

impl GraphNode for Call {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![
                attr!("label", { format!("\"Call: {}()\"", self.func_name) }),
                attr!("shape", "box"),
            ],
        )));
        // Add to visited
        visited.insert(ptr);
        let old_counter = *id_counter;

        for arg in &self.args {
            let to_arg = (&**arg as *const dyn Node) as *const () as usize;
            if visited.contains(&to_arg) {
                continue;
            }

            stmts.push(Stmt::Edge(Edge {
                ty: EdgeTy::Pair(
                    Vertex::N(NodeId(Id::Plain(old_counter.to_string()), None)),
                    Vertex::N(NodeId(
                        Id::Plain({
                            *id_counter += 1;
                            id_counter.to_string()
                        }),
                        None,
                    )),
                ),
                attributes: vec![],
            }));

            *id_counter += 1;
            arg.visit(stmts, id_counter, visited);
        }
    }
}

impl GraphNode for VarDecl {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![
                attr!("label", {
                    format!(
                        "\"VarDecl: {} | Type: {} | Mutable: {}\"",
                        self.name, self.typ, self.mutable
                    )
                }),
                attr!("shape", "box"),
            ],
        )));
        // Add to visited
        visited.insert(ptr);

        if let Some(init_value) = &self.init_value {
            let to_init = (&**init_value as *const dyn Node) as *const () as usize;
            if visited.contains(&to_init) {
                return;
            }

            stmts.push(Stmt::Edge(Edge {
                ty: EdgeTy::Pair(
                    Vertex::N(NodeId(Id::Plain(id_counter.to_string()), None)),
                    Vertex::N(NodeId(
                        Id::Plain({
                            *id_counter += 1;
                            id_counter.to_string()
                        }),
                        None,
                    )),
                ),
                attributes: vec![],
            }));

            *id_counter += 1;
            init_value.visit(stmts, id_counter, visited);
        }
    }
}

impl GraphNode for VarAccess {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![
                attr!("label", {
                    format!("\"VarAccess: {} | Type: {}\"", self.name, self.typ)
                }),
                attr!("shape", "ellipse"),
            ],
        )));
        // Add to visited
        visited.insert(ptr);
    }
}

impl GraphNode for ConstArray {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![
                attr!("label", {
                    format!("\"ConstArray: {} | Type: {}\"", self.name, self.typ)
                }),
                attr!("shape", "box"),
            ],
        )));
        // Add to visited
        visited.insert(ptr);
        let old_counter = *id_counter;

        for init_value in &self.init_values {
            let to_init = (&**init_value as *const dyn Node) as *const () as usize;
            if visited.contains(&to_init) {
                continue;
            }

            stmts.push(Stmt::Edge(Edge {
                ty: EdgeTy::Pair(
                    Vertex::N(NodeId(Id::Plain(old_counter.to_string()), None)),
                    Vertex::N(NodeId(
                        Id::Plain({
                            *id_counter += 1;
                            id_counter.to_string()
                        }),
                        None,
                    )),
                ),
                attributes: vec![],
            }));

            *id_counter += 1;
            init_value.visit(stmts, id_counter, visited);
        }
    }
}

impl GraphNode for LocalArray {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![
                attr!("label", {
                    format!("\"LocalArray: {} | Type: {}\"", self.name, self.typ)
                }),
                attr!("shape", "box"),
            ],
        )));
        // Add to visited
        visited.insert(ptr);

        if let Some(init_values) = &self.init_values {
            let old_counter = *id_counter;

            for init_value in init_values {
                let to_init = (&**init_value as *const dyn Node) as *const () as usize;
                if visited.contains(&to_init) {
                    continue;
                }

                stmts.push(Stmt::Edge(Edge {
                    ty: EdgeTy::Pair(
                        Vertex::N(NodeId(Id::Plain(old_counter.to_string()), None)),
                        Vertex::N(NodeId(
                            Id::Plain({
                                *id_counter += 1;
                                id_counter.to_string()
                            }),
                            None,
                        )),
                    ),
                    attributes: vec![],
                }));

                *id_counter += 1;
                init_value.visit(stmts, id_counter, visited);
            }
        }
    }
}

impl GraphNode for ArrayAccess {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![
                attr!("label", {
                    format!("\"ArrayAccess: {} | Type: {}\"", self.name, self.typ)
                }),
                attr!("shape", "ellipse"),
            ],
        )));
        // Add to visited
        visited.insert(ptr);
        let old_counter = *id_counter;

        for index in &self.indices {
            let to_index = (&**index as *const dyn Node) as *const () as usize;
            if visited.contains(&to_index) {
                continue;
            }

            stmts.push(Stmt::Edge(Edge {
                ty: EdgeTy::Pair(
                    Vertex::N(NodeId(Id::Plain(old_counter.to_string()), None)),
                    Vertex::N(NodeId(
                        Id::Plain({
                            *id_counter += 1;
                            id_counter.to_string()
                        }),
                        None,
                    )),
                ),
                attributes: vec![],
            }));

            *id_counter += 1;
            index.visit(stmts, id_counter, visited);
        }
    }
}

impl GraphNode for Empty {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![attr!("label", "\"Empty\""), attr!("shape", "box")],
        )));
        // Add to visited
        visited.insert(ptr);
    }
}

impl GraphNode for Literal {
    fn visit(
        &self,
        stmts: &mut Vec<Stmt>,
        id_counter: &mut usize,
        visited: &mut std::collections::HashSet<usize>,
    ) {
        let ptr = self as *const _ as usize;
        if visited.contains(&ptr) {
            return;
        }

        let label = match self {
            Literal::Int(val) => format!("\"Literal: Int({})\"", val),
            Literal::Float(val) => format!("\"Literal: Float({})\"", val),
        };

        stmts.push(Stmt::Node(graph::Node::new(
            graph::NodeId(Id::Plain(id_counter.to_string()), None),
            vec![attr!("label", label), attr!("shape", "oval")],
        )));
        // Add to visited
        visited.insert(ptr);
    }
}

macro_rules! impl_graph_node_for_raw_node {
    ($($t:ty),*) => {
        $( 
            impl GraphNode for $t {
                fn visit(
                    &self,
                    _stmts: &mut Vec<Stmt>,
                    _id_counter: &mut usize,
                    _visited: &mut std::collections::HashSet<usize>,
                ) {
                    panic!(concat!(stringify!($t), " should not be visited in graph dump"));
                }
            }
        )*
    };
}

impl_graph_node_for_raw_node!(DeclAggr, RawDecl, RawDef, ArrayInitVal);
