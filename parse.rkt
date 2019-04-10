(load "types.rkt")

(define scanner-spec
  '([whitespace (whitespace) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number ((or "-" "") digit (arbno digit)) number]
    [module-name (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    ))

;arbno | separated-list
(define grammar-spec
  '([prgm ((arbno mod-def) expr) a-prgm]
    [mod-def ("module" module-name "interface" iface "body" mod-body) a-mod-def]
    [iface ("[" (arbno decl) "]") simple-iface]
    [decl (identifier ":" type) val-decl]
    [mod-body ("[" (arbno def) "]") defs-mod-body]
    [def (identifier "=" expr) val-def]
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
    [expr (module-name "." identifier) qualified-var-expr]
    [type ("Int") int-type]
    [type ("Bool") bool-type]
    [type ("(" (separated-list type "*") "->" type ")") proc-type]
    ))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar-spec))

