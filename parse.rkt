(load "types.rkt")

(define scanner-spec
  '([whitespace (whitespace) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [module-name (letter (arbno (or letter digit "_" "-"))) symbol]
    [number ((or "-" "") digit (arbno digit)) number]))

;arbno | separated-list
(define grammar-spec
  '([prgm ((arbno mod-def) expr) a-prgm]
    [mod-def ("module" module-name "interface" iface "body" mod-body) a-mod-def]
    [iface ("[" (arbno decl) "]") simple-iface]
    [decl (identifier ":" decl-type) val-decl]
    [decl-type (type) plain-decl-type]
    [decl-type (iface) iface-decl-type]
    [mod-body ((arbno mod-def) "[" (arbno def) "]") defs-mod-body]
    [def (identifier "=" def-val) val-def]
    [def-val (expr) expr-def-val]
    [def-val (module-name) mod-def-val]
    [expr (number) num-expr]
    [expr (identifier) var-expr]
    [expr ("-" "(" expr "," expr ")") diff-expr]
    [expr ("zero?" "(" expr ")") zero?-expr]
    [expr ("if" expr "then" expr "else" expr) if-expr]
    [expr ("let" (arbno identifier "=" expr) "in" expr) let-expr]
    [expr ("letrec" (arbno identifier "(" (arbno identifier ":" type) ")" "->" type "=" expr) 
           "in" expr) letrec-expr]
    [expr ("proc" "(" (arbno identifier ":" type) ")" expr) proc-expr]
    [expr ("(" expr (arbno expr) ")") call-expr]
    [expr ("from" module-name "take" identifier (arbno "take" identifier)) qualified-var-expr]
    [type ("Int") int-type]
    [type ("Bool") bool-type]
    [type ("(" (separated-list type "*") "->" type ")") proc-type]
    ))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar-spec))

