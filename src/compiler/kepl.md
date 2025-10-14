# The KEP programming language

## Progrgess

- [ ] Lexing
- [ ] Parsing
- [ ] Encoding

## Example

```
fn main() -> () {
    
}
```

## Grammar

```

function  : "fn" ident "(" [typed_arg],* ")" [-> type]? "{" [code]* "}"
typed_arg : ident ":" type
type      : ident
ident     : [_a-zA-Z][_a-zA-Z0-9]*

```
