(load "types.rkt")

; Expression ::= set Identifier = Expression

(define scanner-spec
  '([whitespace (whitespace) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number ((or "-" "") digit (arbno digit)) number]))

;arbno | separated-list 
(define grammar-spec
  '([program ((arbno class-decl) expression) a-program]
    [class-decl ("class" identifier "extends" identifier
                 (arbno "field" identifier) (arbno method-decl)) a-class-decl]
    [method-decl ("method" identifier "(" (separated-list identifier ",") ")" 
                  expression) a-method-decl]
    [expression (number) const-exp]
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("zero?" "(" expression ")") zero?-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]
    [expression (identifier) var-exp]
    [expression ("let" (separated-list identifier "=" expression ",") "in" expression) let-exp]
    [expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp]
    [expression ("(" expression (arbno expression) ")") call-exp]
    [expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" 
                 "=" expression) "in" expression) letrec-exp]
    [expression ("begin" expression (arbno ";" expression) "end") begin-exp]
    [expression ("set" identifier "=" expression) assign-exp]
    [expression ("+" "(" expression "," expression ")") plus-exp]
    [expression ("list" "(" (separated-list expression ",") ")") list-exp]
    [expression ("print" "(" expression ")") print-exp]
    [expression ("new" identifier "(" (separated-list expression ",") ")") new-object-expr]
    [expression ("send" expression identifier 
                 "(" (separated-list expression ",") ")") method-call-expr]
    [expression ("super" identifier "(" (separated-list expression ",") ")") super-call-expr]
    [expression ("self") self-expr]
    [expression ("instanceof" expression identifier) instanceof-expr]
    ))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar-spec))
