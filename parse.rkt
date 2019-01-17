(load "types.rkt")

(define scanner-spec
  '([whitespace (whitespace) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number ((or "-" "") digit (arbno digit)) number]))
(define grammar-spec
  '([program (expression) a-program]
    [expression (number) const-exp]
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("zero?" "(" expression ")") zero?-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]
    [expression (identifier) var-exp]
    [expression ("let" (arbno identifier "=" expression) "in" expression) let-exp]
    [expression ("proc" "(" (arbno identifier) ")" expression) proc-exp]
    [expression ("(" expression (arbno expression) ")") call-exp]
    [expression ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression)
                 "in" expression) letrec-exp]
    ))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar-spec))
