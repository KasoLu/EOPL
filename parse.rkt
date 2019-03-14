(load "types.rkt")

(define scanner-spec
  '([whitespace (whitespace) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number ((or "-" "") digit (arbno digit)) number]))

;arbno | separated-list
(define grammar-spec
  '([prgm (expr) a-prgm]
    [expr (number) num-expr]
    [expr (identifier) var-expr]
    [expr ("-" "(" expr "," expr ")") diff-expr]
    [expr ("zero?" "(" expr ")") zero?-expr]
    [expr ("if" expr "then" expr "else" expr) if-expr]
    [expr ("let" (arbno identifier "=" expr) "in" expr) let-expr]
    [expr ("letrec" (arbno identifier "(" (arbno identifier ":" opty) ")" "->" opty "=" expr) 
           "in" expr) letrec-expr]
    [expr ("proc" "(" (arbno identifier ":" opty) ")" expr) proc-expr]
    [expr ("(" expr (arbno expr) ")") call-expr]
    [expr ("newref" "(" expr ")") newref-expr]
    [expr ("deref" "(" expr ")") deref-expr]
    [expr ("setref" "(" expr "," expr ")") setref-expr]
    [expr ("begin" expr (arbno ";" expr) "end") begin-expr]
    [type ("Int") int-type]
    [type ("Bool") bool-type]
    [type ("(" (separated-list type "*") "->" type ")") proc-type]
    [type ("refto" type) ref-type]
    [type ("Void") void-type]
    [opty ("?") no-type]
    [opty (type) an-type]
    ))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar-spec))

