use std::collections::HashMap;
use crate::compiler::{CompilerError, ast::{AST, NodeId, NodeKind}};

// TODO: validate "super", "crate", and "self" usage

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub usize);
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub usize);

#[derive(Debug, Clone)]
pub enum ResolveError {
    UnresolvedPath {
        path: Vec<String>,
        scope: ScopeId,
    },
    UnresolvedName {
        name: String,
        scope: ScopeId,
    },
    PrivateSymbol {
        path: Vec<String>,
        symbol: SymbolId,
        from_scope: ScopeId,
    },
}
impl From<Vec<ResolveError>> for CompilerError {
    fn from(value: Vec<ResolveError>) -> Self {
        CompilerError::ResolveError(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Namespace {
    Value,  // function, variable, ...
    Type,   // module, type, struct, enum, ...
}

#[derive(Debug, Clone)]
pub enum SymbolKind {
    /// variable defined using "let" or function parameter
    Local {
        mutable: bool,
        declared: Option<NodeId>,
        init: Option<NodeId>,
    },
    /// function defined using "fn"
    Function {
        ret: Option<NodeId>,
        body: NodeId,
    },
    /// module defined using "mod"
    Module
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub kind: SymbolKind,
    /// scope of definition (virtual for "let" statemets)
    pub defining_scope: ScopeId,
    /// for symbols introducing new scopes (e.g., functions, modules)
    pub inner_scope: Option<ScopeId>,
    /// visiblity from non-descendants
    pub public: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    /// only explicit access to other modules using paths
    Module,
    /// only has implicit access to its modules scopes items
    Function,
    /// has implicit access to local vars of parent scope
    Block,
    /// introduced via local symbol definition (same implict access as Block)
    Virtual,
}

#[derive(Debug, Clone)]
pub struct UseBinding {
    pub use_decl: NodeId,
    pub public: bool,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub kind: ScopeKind,
    /// lexical parent, None for the root scope
    pub parent: Option<ScopeId>,
    /// closest containing module (self id for module kind)
    pub module_scope: ScopeId,
    /// table for Value namespace
    pub value_symbols: HashMap<String, SymbolId>,
    /// table for Type namespace
    pub type_symbols: HashMap<String, SymbolId>,
    /// lazy use expansion
    pub uses: Vec<UseBinding>,
}

#[derive(Debug)]
struct ResolverCache {
    /// (scope_id, name, namespace, from_scope) -> SymbolId
    name_cache: HashMap<(ScopeId, String, Namespace, ScopeId), Result<SymbolId, ResolveError>>,

    /// (path_hash, from_scope, namespace) -> SymbolId
    path_cache: HashMap<(u64, ScopeId, Namespace), Result<Vec<SymbolId>, ResolveError>>,

    /// (scope_id, use_decl_id, name) -> SymbolId
    use_cache: HashMap<(ScopeId, NodeId, String), Option<Result<SymbolId, ResolveError>>>,
}
impl ResolverCache {
    fn new() -> Self {
        Self {
            name_cache: HashMap::new(),
            path_cache: HashMap::new(),
            use_cache: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    pub bindings: HashMap<NodeId, SymbolId>,
    pub symbols: Vec<Symbol>,
}
impl SymbolTable {
    /// gets the resolved symbol of a path node
    pub fn get_bound(&self, node: NodeId) -> Option<&Symbol> {
        let id =self.bindings.get(&node)?;
        Some(&self.symbols[id.0])
    }

    /// gets the resolved symbol id of a path node
    pub fn get_id(&self, node: NodeId) -> Option<SymbolId> {
        self.bindings.get(&node).copied()
    }

    /// gets the symbol id corresponding symbol
    pub fn get_symbol(&self, sym_id: SymbolId) -> &Symbol {
        &self.symbols[sym_id.0]
    }
}

pub fn resolve_all(ast: &AST) -> Result<SymbolTable, Vec<ResolveError>> {
    let mut resolver = Resolver::new(ast);
    
    if resolver.resolve().is_ok() {
        Ok(resolver.into_symbol_table())
    } else {
        Err(resolver.errors)
    }
}

#[derive(Debug)]
pub struct Resolver<'a> {
    ast: &'a AST,
    scopes: Vec<Scope>,
    symbols: Vec<Symbol>,
    root_scope: ScopeId,
    cache: ResolverCache,
    node_to_scope: HashMap<NodeId, ScopeId>,
    bindings: HashMap<NodeId, SymbolId>,
    errors: Vec<ResolveError>,
}
impl<'a> Resolver<'a> {
    pub fn new(ast: &'a AST) -> Self {
        Self {
            ast,
            scopes: Vec::new(),
            symbols: Vec::new(),
            root_scope: ScopeId(0),
            cache: ResolverCache::new(),
            node_to_scope: HashMap::new(),
            bindings: HashMap::new(),
            errors: Vec::new(),
        }
    }

    pub fn resolve(&mut self) -> Result<(), &[ResolveError]> {
        self.root_scope = self.new_scope(ScopeKind::Module, None);

        self.build_scopes();
        
        for (node_id, node) in self.ast.nodes.iter().enumerate() {
            let scope_id = self.node_to_scope.get(&NodeId(node_id)).copied().unwrap_or(self.root_scope);

            match &node.kind {
                NodeKind::PathExpression { segments , ..} => {
                    let path = self.node_to_string_path(segments);

                    match self.resolve_path_from(&path, scope_id, Namespace::Value) {
                        Ok(sym_ids) => {
                            debug_assert_eq!(segments.len(), sym_ids.len());
                            for (seg_id, sym_id) in segments.iter().zip(sym_ids.iter()) {
                                self.bindings.insert(*seg_id, *sym_id);
                            }
                            if let Some(&last_sym) = sym_ids.last() {
                                self.bindings.insert(NodeId(node_id), last_sym);
                            }
                        },
                        Err(err) => self.errors.push(err),
                    }
                }

                NodeKind::TypePath { segments, .. } => {
                    let path = self.node_to_string_path(segments);

                    match self.resolve_path_from(&path, scope_id, Namespace::Type) {
                        Ok(sym_ids) => {
                            debug_assert_eq!(segments.len(), sym_ids.len());
                            for (seg_id, sym_id) in segments.iter().zip(sym_ids.iter()) {
                                self.bindings.insert(*seg_id, *sym_id);
                            }
                            if let Some(&last_sym) = sym_ids.last() {
                                self.bindings.insert(NodeId(node_id), last_sym);
                            }
                        },
                        Err(err) => self.errors.push(err),
                    }
                }

                _ => {}
            }
        }

        
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(&self.errors)
        }
    }

    pub fn into_symbol_table(self) -> SymbolTable {
        SymbolTable {
            bindings: self.bindings,
            symbols: self.symbols,
        }
    }
}

impl<'a> Resolver<'a> {
    fn build_scopes(&mut self) {
        let mut worklist: Vec<(Box<dyn Iterator<Item = NodeId>>, ScopeId)> = vec![(Box::new(self.ast.items.iter().copied()), self.root_scope)];

        while let Some((nodes, mut current_scope)) = worklist.pop() {
            for node_id in nodes {
                match &self.ast.nodes[node_id.0].kind {
                    // simplest case: introduce a new Block scope
                    NodeKind::Block { nodes } => {
                        let block_scope = self.new_scope(ScopeKind::Block, Some(current_scope));
                        worklist.push((Box::new(nodes.iter().copied()), block_scope));
                    }
                    // special case: introduce a new virutal scope
                    NodeKind::LetStmt { name, value, mutable, ty } => {
                        // expression evaluated before definition
                        if let Some(expr) = value {
                            worklist.push((Box::new(std::iter::once(*expr)), current_scope));
                        }
                        if let Some(ty) = ty {
                            worklist.push((Box::new(std::iter::once(*ty)), current_scope));
                        }

                        // new virtual scope
                        let new_scope = self.new_scope(ScopeKind::Virtual, Some(current_scope));

                        let sym = Symbol {
                            kind: SymbolKind::Local {
                                mutable: *mutable,
                                declared: *ty,
                                init: *value,
                            },
                            defining_scope: new_scope,
                            inner_scope: None,
                            public: false,
                        };

                        self.add_symbol(sym, new_scope, Namespace::Value, name.clone());

                        // following nodes should be inside the new virtual scope
                        current_scope = new_scope;
                    }
                    // item case: defined in non-virtual and adds a new item scope
                    NodeKind::Function { public, name, body, params, return_type } => {
                        // define in non-virtual
                        let def_scope = self.find_structural_scope(current_scope);
                        
                        // new inner function scope
                        let func_scope = self.new_scope(ScopeKind::Function, Some(current_scope));

                        let sym = Symbol {
                            kind: SymbolKind::Function {
                                ret: *return_type,
                                body: *body,
                            },
                            defining_scope: def_scope,
                            inner_scope: Some(func_scope),
                            public: *public,
                        };
                        self.add_symbol(sym, def_scope, Namespace::Value, name.clone());

                        // add parameters
                        for (param_name, type_node) in params.iter() {
                            let param_sym = Symbol {
                                kind: SymbolKind::Local {
                                    mutable: false,
                                    declared: Some(*type_node),
                                    init: None,
                                },
                                defining_scope: func_scope,
                                inner_scope: None,
                                public: false,
                            };
                            self.add_symbol(param_sym, func_scope, Namespace::Value, param_name.clone());
                            worklist.push((Box::new(std::iter::once(*type_node)), def_scope));
                        }
                        if let Some(ty) = return_type {
                            worklist.push((Box::new(std::iter::once(*ty)), def_scope));
                        }

                        worklist.push((Box::new(std::iter::once(*body)), func_scope));
                    }
                    // module case: defined in non-virtual and adds a new module scope
                    NodeKind::Module { name, items, public } => {
                        // define in non-virutal
                        let def_scope = self.find_structural_scope(current_scope);

                        // new inner module scope
                        let inner = self.new_scope(ScopeKind::Module, Some(current_scope));

                        let sym = Symbol {
                            kind: SymbolKind::Module,
                            defining_scope: def_scope,
                            inner_scope: Some(inner),
                            public: *public,
                        };
                        self.add_symbol(sym, def_scope, Namespace::Type, name.clone());

                        if let Some(items) = items {
                            worklist.push((Box::new(items.iter().copied()), inner));
                        }
                    }
                    // use case: add a marker for lazy expansion in non-virtual
                    NodeKind::UseDecl { public, use_tree } => {
                        // mark in non-virtual
                        let def_scope = self.find_structural_scope(current_scope);
                        
                        self.scopes[def_scope.0].uses.push(UseBinding { use_decl: *use_tree, public: *public });
                    }
                    
                    NodeKind::TypePath { segments, .. }
                    | NodeKind::PathExpression { segments, .. } => {
                        self.node_to_scope.insert(node_id, current_scope);
                        worklist.push((Box::new(segments.iter().copied()), current_scope));
                    }

                    node => {
                        worklist.push((Box::new(node.children()), current_scope));
                    }
                }
            }
        }
    }

    fn new_scope(&mut self, kind: ScopeKind, parent: Option<ScopeId>) -> ScopeId {
        let id = self.scopes.len();
        let module_scope = match kind {
            ScopeKind::Module => ScopeId(id),
            _ => parent.map(|p| self.scopes[p.0].module_scope).unwrap()
        };
        self.scopes.push(Scope {
            kind,
            parent,
            module_scope,
            value_symbols: HashMap::new(),
            type_symbols: HashMap::new(),
            uses: Vec::new(),
        });
        ScopeId(id)
    }

    fn add_symbol(&mut self, sym: Symbol, scope_id: ScopeId, ns: Namespace, name: String) -> SymbolId {
        let id = self.symbols.len();
        self.symbols.push(sym);
        let tabel = match ns {
            Namespace::Value => &mut self.scopes[scope_id.0].value_symbols,
            Namespace::Type => &mut self.scopes[scope_id.0].type_symbols,
        };
        tabel.insert(name, SymbolId(id));
        SymbolId(id)
    }

    fn find_structural_scope(&self, mut scope: ScopeId) -> ScopeId {
        while let ScopeKind::Virtual = self.scopes[scope.0].kind {
            scope = self.scopes[scope.0].parent.unwrap();
        }
        scope
    }
}

impl<'a> Resolver<'a> {
    // turns a path specified using PathSegment nodes into a string path
    fn node_to_string_path(&self, segs: &[NodeId]) -> Vec<String> {
        segs.iter()
            .map(|&id| {
                if let NodeKind::PathSegment { ident, .. } = &self.ast.nodes[id.0].kind {
                    ident.clone()
                } else {
                    "<invalid>".to_string()
                }
            })
            .collect()
    }

    /// resolve a `path` starting at `from_scope` to a symbol in namespace `namespace` without using cache lookup.
    /// Returns an error if the path couln't be resolved or the symbol is not visible
    fn resolve_path_uncached(&mut self, path: &[String], from_scope: ScopeId, namespace: Namespace) -> Result<Vec<SymbolId>, ResolveError> {
        if path.is_empty() {
            return Err(ResolveError::UnresolvedPath { path: vec![], scope: from_scope });
        }

        let mut current_scope = from_scope;
        let mut symbol_history = vec![];

        for (i, seg_name) in path.iter().enumerate() {
            let is_last = i == path.len() - 1;
            
            if i == 0 && seg_name == "crate" {
                current_scope = self.root_scope;
                continue;
            }
            
            // lookup in current scope
            // Namespace::Type enforeced if not last segment
            let sym_id = self.lookup_in_scope(seg_name, current_scope,  if is_last { namespace } else { Namespace::Type }, from_scope)?;
            if !self.symbol_is_visible_from(sym_id, from_scope) {
                return Err(ResolveError::PrivateSymbol { path: path[..=i].to_vec(), symbol: sym_id, from_scope });
            }
            symbol_history.push(sym_id);

            if !is_last {
                let sym = &self.symbols[sym_id.0];
                if let SymbolKind::Module = sym.kind {
                    current_scope = sym.inner_scope.unwrap();
                } else {
                    return Err(ResolveError::UnresolvedPath { path: path.to_vec(), scope: current_scope });
                }
            }
        }

        Ok(symbol_history)
    }

    /// resolve a `path` starting at `from_scope` to a symbol in namespace `namespace`.
    /// Returns an error if the path couln't be resolved or the symbol is not visible
    fn resolve_path_from<'this, 'path>(&'this mut self, path: &'path [String], from_scope: ScopeId, namespace: Namespace) -> Result<Vec<SymbolId>, ResolveError> {
        let path_hash = self.hash_path(path);
        let key = (path_hash, from_scope, namespace);
        if let Some(cached) = self.cache.path_cache.get(&key) {
            return cached.clone();
        }

        let result = self.resolve_path_uncached(path, from_scope, namespace);
        self.cache.path_cache.insert(key, result.clone());
        result
    }

    fn hash_path(&self, path: &[String]) -> u64 {
        use std::hash::{Hash, Hasher};
        let mut h = std::collections::hash_map::DefaultHasher::new();
        path.hash(&mut h);
        h.finish()
    }

    /// looks up a symbol in namespace `ns` named `name` in `scope_id` from `from_scope` without using cache lookup
    fn lookup_in_scope_uncached(&mut self, name: &str, mut scope_id: ScopeId, ns: Namespace, from_scope: ScopeId) -> Result<SymbolId, ResolveError> {
        loop {
            let table = match ns {
                Namespace::Value => &self.scopes[scope_id.0].value_symbols,
                Namespace::Type => &self.scopes[scope_id.0].type_symbols,
            };
            
            if let Some(&sym_id) = table.get(name) {
                return Ok(sym_id);
            }
            
            // search uses
            if let Some(result) = self.lookup_via_uses(scope_id, name, ns, from_scope) {
                return result;
            }
            
            // drop scope
            let scope = &self.scopes[scope_id.0];
            match scope.kind {
                ScopeKind::Block | ScopeKind::Virtual => {
                    // continue search in parent
                    if let Some(parent) = scope.parent {
                        scope_id = parent;
                        continue;
                    }
                }
                // only see the module scope
                ScopeKind::Function | ScopeKind::Module => {
                    if scope_id != scope.module_scope {
                        scope_id = scope.module_scope;
                        continue;
                    }
                }
            }

            break;
        }

        Err(ResolveError::UnresolvedName { name: name.to_string(), scope: scope_id })
    }

    /// looks up a symbol in namespace `ns` named `name` in `scope_id` from `from_scope`
    fn lookup_in_scope(&mut self, name: &str, scope_id: ScopeId, ns: Namespace, from_scope: ScopeId) -> Result<SymbolId, ResolveError> {
        if let Some(cached) = self.cache.name_cache.get(&(scope_id, name.to_string(), ns, from_scope)) {
            return cached.clone();
        }

        let result = self.lookup_in_scope_uncached(name, scope_id, ns, from_scope);
        self.cache.name_cache.insert((scope_id, name.to_string(), ns, from_scope), result.clone());
        result
    }

    /// check if `name` of namespace `ns` can be found through this `use_binding` in scope `scope_id`.
    /// Returns an error if the use is invalid or the scope can't see the symbol.
    fn lookup_via_single_use(&mut self, scope_id: ScopeId, use_binding: &UseBinding, name: &str, ns: Namespace) -> Option<Result<SymbolId, ResolveError>> {
        let mut stack = vec![(use_binding.use_decl, vec![])];

        while let Some((node_id, mut prefix)) = stack.pop() {
            let node = &self.ast.nodes[node_id.0];
            match &node.kind {
                NodeKind::UsePath { ident, tree } => {
                    prefix.push(ident.clone());
                    stack.push((*tree, prefix));
                }
                NodeKind::UseGroup { trees } => {
                    for &t in trees {
                        stack.push((t, prefix.clone()));
                    }
                }
                NodeKind::UseName { ident } => {
                    if ident == name {
                        let mut path = prefix;
                        path.push(ident.clone());
                        return Some(self.resolve_path_from(&path, scope_id, ns).map(|ids| ids[ids.len() - 1]));
                    }
                }
                NodeKind::UseRename { ident, name: alias } => {
                    if alias == name {
                        let mut path = prefix;
                        path.push(ident.clone());
                        return Some(self.resolve_path_from(&path, scope_id, ns).map(|ids| ids[ids.len() - 1]));
                    }
                }
                NodeKind::UseGlob => {
                    let target_sym = match self.resolve_path_from(&prefix, scope_id, Namespace::Type) {
                        Ok(sym_ids) => sym_ids[sym_ids.len() - 1],
                        Err(err) => return Some(Err(err))
                    };

                    let Some(target_scope) = self.symbols[target_sym.0].inner_scope else {
                        return Some(Err(ResolveError::UnresolvedPath {
                            path: prefix.iter().cloned().chain(std::iter::once("*".to_string())).collect(),
                            scope: scope_id,
                        }));
                    };

                    let table = match ns {
                        Namespace::Value => &self.scopes[target_scope.0].value_symbols,
                        Namespace::Type => &self.scopes[target_scope.0].type_symbols,
                    };
                    
                    // check if the use declaration scope can see the symbol
                    if let Some(&sym_id) = table.get(name) {
                        return Some(if self.symbol_is_visible_from(sym_id, scope_id) {
                            Ok(sym_id)
                        } else {
                            Err(ResolveError::PrivateSymbol {
                                path: prefix.iter().cloned().chain(std::iter::once(name.to_string())).collect(),
                                symbol: sym_id,
                                from_scope: scope_id,
                            })
                        })
                    }
                }
                _ => {}
            }
        }

        None
    }

    /// try to find a symbol using the scopes use aliases
    /// 
    /// * `scope_id`: the scope of the use aliases \
    /// * `name`: the name of the symbol to find \
    /// * `ns`: what namespace the symbol resides in \
    /// * `from_scope`: the scope that is trying to find the symbol
    fn lookup_via_uses(&mut self, scope_id: ScopeId, name: &str, ns: Namespace, from_scope: ScopeId) -> Option<Result<SymbolId, ResolveError>> {
        // Quick check: has this query been seen before?
        let uses = self.scopes[scope_id.0].uses.clone();
        for use_binding in &uses {
            if use_binding.public || self.is_within(from_scope, scope_id) {
                let key = (scope_id, use_binding.use_decl, name.to_string());
                if let Some(cached) = self.cache.use_cache.get(&key) {
                    if cached.is_some() {
                        return cached.clone();
                    }
                    continue;
                }

                let result = self.lookup_via_single_use(scope_id, use_binding, name, ns);
                self.cache.use_cache.insert(key, result.clone());
                if result.is_some() {
                    return result;
                }
            }
        }
        None
    }
     
    /// only publics are visible except when accessing from a descendant
    fn symbol_is_visible_from(&self, sym_id: SymbolId, from_scope: ScopeId) -> bool {
        let sym = &self.symbols[sym_id.0];
        sym.public || self.is_within(from_scope, sym.defining_scope)
    }

    /// checks if `scope` is a descendant of `ancestor`
    fn is_within(&self, scope: ScopeId, ancestor: ScopeId) -> bool {
        let mut current = scope;
        loop {
            if current == ancestor {
                return true;
            }
            if let Some(parent) = self.scopes[current.0].parent {
                current = parent;
            } else {
                return false;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn find_ident(ast: &super::super::ast::AST, name: &str, nth: usize) -> Option<NodeId> {
        let mut scheduled: Vec<NodeId> = ast.items.iter().rev().cloned().collect();
        let mut found = 0;

        while let Some(node_id) = scheduled.pop() {
            match &ast.nodes[node_id.0].kind {
                NodeKind::UseName { ident }
                | NodeKind::UsePath { ident, .. }
                | NodeKind::Module { name: ident, .. }
                | NodeKind::LetStmt { name: ident, .. } => {
                    if ident == name {
                        found += 1;
                        if found >= nth {
                            return Some(node_id);
                        }
                    }
                }

                NodeKind::TypePath { segments, .. }
                | NodeKind::PathExpression { segments, .. } => {
                    let last = segments.iter()
                        .map(|&id| {
                            if let NodeKind::PathSegment { ident, .. } = &ast.nodes[id.0].kind {
                                ident.clone()
                            } else {
                                "<invalid>".to_string()
                            }
                        })
                        .last();
                    if let Some(ident) = last {
                        if ident == name {
                            found += 1;
                            if found >= nth {
                                return Some(node_id);
                            }
                        }
                    }
                }

                NodeKind::Function { name: ident, params, .. } => {
                    if ident == name {
                        found += 1;
                        if found >= nth {
                            return Some(node_id);
                        }
                    }
                    for (ident, _) in params {
                        if ident == name {
                            found += 1;
                            if found >= nth {
                                return Some(node_id);
                            }
                        }
                    }
                }

                NodeKind::UseRename { ident, name: rename } => {
                    if ident == name {
                        found += 1;
                        if found >= nth {
                            return Some(node_id);
                        }
                    }
                    if rename == name {
                        found += 1;
                        if found >= nth {
                            return Some(node_id);
                        }
                    }
                }
                
                _ => {}
            }

            scheduled.extend(ast.nodes[node_id.0].children().collect::<Vec<_>>().iter().rev());
        }

        return None;
    }

    #[test]
    fn resolves_simple_let_binding() {
        let code = r#"
            fn main() {
                let x = 5;
                let y = x;
            }
        "#;

        let tokens = super::super::lexer::lex_all(&code).unwrap();
        let ast = super::super::parser::parse_all(tokens).unwrap();

        let mut resolver = Resolver::new(&ast);
        let result = resolver.resolve();

        assert!(result.is_ok());
        let sym_x_ref = resolver.bindings[&find_ident(&ast, "x", 2).unwrap()];
        assert!(matches!(resolver.symbols[sym_x_ref.0].kind, SymbolKind::Local { declared: None, init: Some(_), .. }), "x should be a inferred initialized local var");
    }

    #[test]
    fn resolves_shadowed_variables() {
        let code = r#"
            fn f() {
                let x;
                {
                    let x = 2;
                    let y = x;
                }
            }
        "#;

        let tokens = super::super::lexer::lex_all(&code).unwrap();
        let ast = super::super::parser::parse_all(tokens).unwrap();

        let mut resolver = Resolver::new(&ast);
        let result = resolver.resolve();

        assert!(result.is_ok());
        let y_ref_x = resolver.bindings[&find_ident(&ast, "x", 3).unwrap()];
        assert!(matches!(resolver.symbols[y_ref_x.0].kind, SymbolKind::Local { declared: None, init: Some(_), .. }));
    }

    #[test]
    fn enforces_visibility() {
        let code = r#"
            mod foo {
                fn secret() {}
                pub fn open() {}
            }
            fn main() {
                foo::open();
                foo::secret(); // should fail
            }
        "#;

        let tokens = super::super::lexer::lex_all(&code).unwrap();
        let ast = super::super::parser::parse_all(tokens).unwrap();

        let mut resolver = Resolver::new(&ast);
        let errors = resolver.resolve().unwrap_err();
        assert_eq!(errors.len(), 1);
        assert!(matches!(&errors[0], ResolveError::PrivateSymbol { path, .. } if path == &["foo", "secret"]));
    }

    #[test]
    fn resolves_public_reexport() {
        let code = r#"
            mod foo {
                mod bar {
                    pub fn func() {}
                }
                pub use bar::func;
            }
            fn main() {
                foo::func(); // allowed via reexport
            }
        "#;

        let tokens = super::super::lexer::lex_all(&code).unwrap();
        let ast = super::super::parser::parse_all(tokens).unwrap();

        let mut resolver = Resolver::new(&ast);
        let result = resolver.resolve();
        assert!(result.is_ok());
    }
}
