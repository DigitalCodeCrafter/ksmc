
pub type TypeId = usize;

#[derive(Debug)]
pub enum UnifyError {
    Mismatch(TypeId, TypeId),
}

mod env {
    use std::collections::HashMap;
    use crate::compiler::resolver::SymbolId;
    use super::{TypeId, UnifyError};

    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub enum TypeKind {
        /// Built-in primitive type
        Primitive(String),

        // User-defined type (structs, enums, aliases)
        User(SymbolId),

        /// Type variable (for inferrence)
        Var(u32),

        /// Function type: (params) -> return
        Function {
            params: Vec<TypeId>,
            ret: TypeId
        },

        /// Tuple type: (T1, T2, ...)
        Tuple(Vec<TypeId>),

        /// Array type: [T; N]
        Array {
            elem: TypeId,
            len: Option<u32>
        },

        /// Generic instantiation: e.g. Option<T>, Vec<T>
        Generic {
            base: TypeId,
            args: Vec<TypeId>
        },
    }

    pub struct TypeEnv {
        arena: Vec<TypeKind>,
        subst: HashMap<u32, TypeId>,            // type var -> resolved type
        builtins: HashMap<String, TypeId>,      // "i32" -> TypeId
        interned: HashMap<TypeKind, TypeId>,    // type interning
        next_var: u32,
    }

    impl TypeEnv {
        pub fn new() -> Self {
            let mut env = TypeEnv {
                arena: Vec::new(),
                subst: HashMap::new(),
                builtins: HashMap::new(),
                interned: HashMap::new(),
                next_var: 0,
            };

            for name in ["i32", "f64", "bool", "str", "unit"] {
                env.intern_builtin(name);
            }

            env
        }

        fn intern_builtin(&mut self, name: &str) -> TypeId {
            let ty = TypeKind::Primitive(name.to_string());
            let id = self.intern(ty.clone());
            self.builtins.insert(name.to_string(), id);
            id
        }

        pub fn intern(&mut self, ty: TypeKind) -> TypeId {
            if let Some(&id) = self.interned.get(&ty) {
                return id;
            }
            let id = self.arena.len();
            self.arena.push(ty.clone());
            self.interned.insert(ty, id);
            id
        }

        pub fn new_var(&mut self) -> TypeId {
            let id = self.next_var;
            self.next_var += 1;
            self.intern(TypeKind::Var(id))
        }

        pub fn get(&self, id: TypeId) -> &TypeKind {
            &self.arena[id]
        }

        pub fn get_builtin(&self, name: &str) -> Option<TypeId> {
            self.builtins.get(name).copied()
        }

        pub fn normalize(&mut self, ty: TypeId) -> TypeId {
            match &self.get(ty) {
                TypeKind::Var(v) => {
                    let v = *v;
                    if let Some(resolved) = self.subst.get(&v) {
                        let normalized = self.normalize(*resolved);

                        self.subst.insert(v, normalized);
                        normalized
                    } else {
                        ty
                    }
                }
                _ => ty,
            }
        }

        pub fn unify(&mut self, a: TypeId, b: TypeId) -> Result<TypeId, UnifyError> {
            let a = self.normalize(a);
            let b = self.normalize(b);

            if a == b {
                return Ok(a);
            }

            match (self.get(a).clone(), self.get(b).clone()) {
                // unify type variables
                (TypeKind::Var(va), _) => {
                    self.subst.insert(va, b);
                    Ok(b)
                }
                (_, TypeKind::Var(vb)) => {
                    self.subst.insert(vb, a);
                    Ok(a)
                }

                // unify functions
                (
                    TypeKind::Function { params: pa, ret: ra },
                    TypeKind::Function { params: pb, ret: rb }
                ) => {
                    if pa.len() != pb.len() {
                        return Err(UnifyError::Mismatch(a, b));
                    }
                    for (ta, tb) in pa.iter().zip(pb.iter()) {
                        self.unify(*ta, *tb)?;
                    }
                    self.unify(ra, rb)
                }

                // unify tuples
                (TypeKind::Tuple(ta), TypeKind::Tuple(tb)) => {
                    if ta.len() != tb.len() {
                        return Err(UnifyError::Mismatch(a, b));
                    }
                    for (xa, xb) in ta.iter().zip(tb.iter()) {
                        self.unify(*xa, *xb)?;
                    }
                    Ok(a)
                }

                // unify arrays
                (
                    TypeKind::Array { elem: ea, len: la },
                    TypeKind::Array { elem: eb, len: lb },
                ) if la == lb => self.unify(ea, eb),

                // identical user types
                (TypeKind::User(sa), TypeKind::User(sb)) if sa == sb => Ok(a),

                // indetical primitives (failsafe)
                (TypeKind::Primitive(na), TypeKind::Primitive(nb)) if na == nb => Ok(a),

                _ => Err(UnifyError::Mismatch(a, b))
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn simple_inferrence() {
            let mut tenv = TypeEnv::new();

            let i32_t = tenv.get_builtin("i32").unwrap();

            let mut tvar_a = tenv.new_var();
            let mut tvar_b = tenv.new_var();

            // a = i32
            tenv.unify(tvar_a, i32_t).unwrap();
            // a = b
            tenv.unify(tvar_b, tvar_a).unwrap();

            tvar_a = tenv.normalize(tvar_a);
            tvar_b = tenv.normalize(tvar_b);

            assert_eq!(tenv.get(tvar_a), tenv.get(tvar_b))
        }
    }
}