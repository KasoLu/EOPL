(load "types.rkt")

; Expression ::= set Identifier = Expression
; Expression ::= ref Identifier
; Expression ::= deref( Identifier )
; Expression ::= setref( Identifier , Expression )
; Expression ::= newarray( Expression , Expression )
; Expression ::= arrayref( Expression , Expression )
; Expression ::= arrayset( Expression , Expression, Expression )

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
    [expression ("begin" expression (arbno ";" expression) "end") begin-exp]
    [expression ("set" identifier "=" expression) assign-exp]
    [expression ("ref" identifier) ref-exp]
    [expression ("deref" "(" identifier ")") deref-exp]
    [expression ("setref" "(" identifier "," expression ")") setref-exp]
    [expression ("newarray" "(" expression "," expression ")") newarray-exp]
    [expression ("arrayref" "(" expression "," expression ")") arrayref-exp]
    [expression ("arrayset" "(" expression "," expression "," expression ")") arrayset-exp]
    ))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar-spec))
