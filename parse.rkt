(load "types.rkt")

(define scanner-spec
  '([whitespace (whitespace) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number ((or "-" "") digit (arbno digit)) number]))

;arbno | separated-list
(define grammar-spec
  '([prgm ((arbno mod-def) expr) a-prgm]
    [mod-def ("module" identifier "interface" iface "body" mod-body) a-mod-def] [iface ("[" (arbno decl) "]") simple-iface]
    [iface ("(" "(" identifier ":" iface ")" "=>" iface ")") proc-iface]
    [decl (identifier ":" type) val-decl]
    [decl ("opaque" identifier) opaque-type-decl]
    [decl ("transparent" identifier "=" type) transparent-type-decl]
    [mod-body ("[" (arbno def) "]") defs-mod-body]
    [mod-body ("module-proc" "(" identifier ":" iface ")" mod-body) proc-mod-body]
    [mod-body (identifier) var-mod-body]
    [mod-body ("(" identifier identifier ")") app-mod-body]
    [def (identifier "=" expr) val-def]
    [def ("type" identifier "=" type) type-def]
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
    [expr ("from" identifier "take" identifier) qualified-var-expr]
    [type ("Int") int-type]
    [type ("Bool") bool-type]
    [type ("(" (separated-list type "*") "->" type ")") proc-type]
    [type (identifier) named-type]
    [type ("from" identifier "take" identifier) qualified-type]
    ))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar-spec))

