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
    [expr ("letrec" (arbno identifier "(" (arbno identifier ":" type) ")" "->" type "=" expr) 
           "in" expr) letrec-expr]
    [expr ("proc" "(" (arbno identifier ":" type) ")" expr) proc-expr]
    [expr ("(" expr (arbno expr) ")") call-expr]
    [expr ("newpair" "(" expr "," expr ")") pair-expr]
    [expr ("unpair" identifier identifier "=" expr "in" expr) unpair-expr]
    [type ("Int") int-type]
    [type ("Bool") bool-type]
    [type ("(" (separated-list type "*") "->" type ")") proc-type]
    [type ("pairof" type "*" type) pair-type (ty1 ty2)]
    ))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar-spec))

