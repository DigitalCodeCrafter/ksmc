use std::{collections::HashSet, path::{Path, PathBuf}};
use crate::compiler::{ast::{Node, NodeId, NodeKind, AST}, lexer::Token, CompilerError};

#[derive(Debug)]
pub enum ExpandError {
    NoProgramRoot,
    NotFound(PathBuf),
    FileRead(PathBuf, std::io::Error),
}
impl From<ExpandError> for CompilerError {
    fn from(value: ExpandError) -> Self {
        CompilerError::ExpandError(value)
    }
}

pub fn expand_all(ast: &mut AST, base_path: &Path) -> Result<(), CompilerError> {
    let mut expander = Expander::new(ast, base_path, |src| {
        super::lexer::lex_all(src).map_err(|e| e.into())
    }, |tokens| {
        super::parser::parse_all(tokens).map_err(|e| e.into())
    });
    expander.expand_modules()
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
                        let mut dir = current_path.to_path_buf();
                        dir.push(name);
                        for child_id in ids {
                            self.expand_node(child_id, &dir)?;
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
            return Err(ExpandError::NotFound(path.to_path_buf()).into());
        }

        // already expanded
        if self.expanded.contains(path) {
            return Ok(());
        }
        self.expanded.insert(path.to_path_buf());

        // read and parse
        let src = std::fs::read_to_string(path)
            .map_err(|e| ExpandError::FileRead(path.to_path_buf(), e))?;

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
    for node in nodes {
        for child_id in node.children_mut() {
            *child_id += offset;
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
