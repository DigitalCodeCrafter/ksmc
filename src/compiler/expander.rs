use std::{collections::HashSet, path::{Path, PathBuf}};
use crate::compiler::{ast::{Node, NodeId, NodeKind, AST}, lexer::Token, CompilerError, ToCompileResult};

#[derive(Debug)]
pub enum ExpandError {
    NoProgramRoot,
    NotFound(PathBuf),
    FileRead(PathBuf, std::io::Error),
}
impl<T> ToCompileResult<T> for Result<T, ExpandError> {
    fn into_cresult(self) -> Result<T, super::CompilerError> {
        self.map_err(|err| CompilerError::ExpandError(err))
    }
}

pub struct Expander<'a> {
    pub ast: &'a mut AST,
    pub base_path: &'a Path,
    pub lexer: fn(&str) -> Result<Vec<Token>, CompilerError>,
    pub parser: fn(Vec<Token>) -> Result<AST, CompilerError>,
    pub expanded: HashSet<PathBuf>,
}
impl<'a> Expander<'a> {
    pub fn new(ast: &'a mut AST, base_path: &'a Path, lexer: fn(&str) ->Result<Vec<Token>, CompilerError>, parser: fn(Vec<Token>) ->Result<AST, CompilerError>) -> Self {
        Self {
            ast,
            base_path,
            lexer,
            parser,
            expanded: HashSet::new()
        }
    }

    pub fn expand_modules(&mut self) -> Result<(), CompilerError> {
        let items = self.ast.items.clone();
        for item_id in items {
            self.expand_node(item_id, &self.base_path)?;
        }
        Ok(())
    }

    fn expand_node(&mut self, node_id: NodeId, current_path: &Path) -> Result<(), CompilerError> {
        let kind = self.ast.nodes[node_id].kind.clone();

        match kind {
            NodeKind::Module { public: _, ref name, ref items } => {
                // expand if not yet inlined
                if items.is_none() {
                    let module_path = resolve_module_path(current_path, name);
                    self.expand_module_file(node_id, &module_path)?;
                }
                // search for children if inlined
                if let NodeKind::Module { ref items, .. } = self.ast.nodes[node_id].kind {
                    if let Some(ids) = items {
                        let ids = ids.clone();
                        let new_base = resolve_module_dir(current_path, name);
                        for child_id in ids {
                            self.expand_node(child_id, &new_base)?;
                        }
                    }
                }
            }

            _ => {}
        }

        Ok(())
    }

    fn expand_module_file(&mut self, node_id: NodeId, path: &Path) -> Result<(), CompilerError> {
        if !path.exists() {
            return Err(ExpandError::NotFound(path.to_path_buf())).into_cresult();
        }

        // already expanded
        if self.expanded.contains(path) {
            return Ok(());
        }
        self.expanded.insert(path.to_path_buf());

        // read and parse
        let src = std::fs::read_to_string(path)
            .map_err(|e| ExpandError::FileRead(path.to_path_buf(), e)).into_cresult()?;

        let tokens = (self.lexer)(&src)?;
        let mut parsed_ast = (self.parser)(tokens)?;

        // rebase ids and extend
        let offset = self.ast.nodes.len();
        rebase_node_ids(&mut parsed_ast.nodes, offset);

        self.ast.nodes.extend(parsed_ast.nodes);

        for id in &mut parsed_ast.items {
            *id += offset;
        }

        if let NodeKind::Module { items, .. } = &mut self.ast.nodes[node_id].kind {
            *items = Some(parsed_ast.items);
        }

        Ok(())
    }
}

fn rebase_node_ids(nodes: &mut [Node], offset: usize) {
    use NodeKind::*;
    for node in nodes {
        match &mut node.kind {
            Unary { expr: id, .. } |
            Loop { block: id } |
            ExprStmt { expr: id } |
            UseDecl { use_tree: id , ..} |
            TupleIndexExpression { tuple: id , .. } |
            TypeSlice { ty: id } |
            UsePath {tree: id , ..} => *id += offset,

            Return { expr: optid } |
            Break { expr: optid } => {
                if let Some(id) = optid {
                    *id += offset
                }
            }

            Block { nodes: ids } |
            Tuple { elements: ids } |
            Array { elements: ids } |
            PathExpression { segments: ids } |
            TypePath { segments: ids } |
            TypeTuple { elements: ids } |
            UseGroup { trees: ids } |
            PathSegment {args: ids , ..} => {
                for id in ids {
                    *id += offset;
                }
            }

            Binary { lhs: id0, rhs: id1, .. } |
            ArrayRepeat { value: id0, count: id1 } |
            IndexExpression { array: id0 , index: id1 } |
            TypeArray { ty: id0, len: id1 } => {
                *id0 += offset;
                *id1 += offset;
            }
            
            Call { callee: id, args: ids } => {
                *id += offset;
                for id in ids {
                    *id += offset;
                }
            }
            
            If { cond, then_block, else_block } => {
                *cond += offset;
                *then_block += offset;
                if let Some(else_id) = else_block {
                    *else_id += offset;
                }
            }
            
            LetStmt {ty: optid0, value: optid1 , ..} => {
                if let Some(id) = optid0 {
                    *id += offset
                }
                if let Some(id) = optid1 {
                    *id += offset
                }
            }

            Function { params, return_type: optid, body: id, ..} => {
                *id += offset;
                if let Some(id) = optid {
                    *id += offset
                }
                for (_, id) in params {
                    *id += offset
                }
            }

            Module { items: optids , ..} => {
                if let Some(ids) = optids {
                    for id in ids {
                        *id += offset
                    }
                }
            }

            _ => {}
        }
    }
}

fn resolve_module_path(base: &Path, name: &str) -> PathBuf {
    let mut path = base.to_path_buf();
    path.push(name);

    if path.with_extension(crate::compiler::FILE_SUFFIX).exists() {
        path.with_extension(crate::compiler::FILE_SUFFIX)
    } else {
        path.push(format!("mod.{}", crate::compiler::FILE_SUFFIX));
        path
    }
}

fn resolve_module_dir(base: &Path, name: &str) -> PathBuf {
    let mut dir = base.to_path_buf();
    dir.push(name);
    dir
}

