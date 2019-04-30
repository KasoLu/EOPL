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
                 (arbno "implements" identifier)
                 (arbno "field" type identifier)
                 (arbno method-decl)) a-class-decl]
    [class-decl ("interface" identifier "extends" identifier
                 (arbno abs-method-decl)) an-interface-decl]
    [method-decl ("method" type identifier 
                  "(" (separated-list identifier ":" type ",") ")" expression) a-method-decl]
    [abs-method-decl ("method" type identifier 
                      "(" (separated-list identifier ":" type ",") ")") an-abs-method-decl]
    [expression (number) num-expr]
    [expression ("-" "(" expression "," expression ")") diff-expr]
    [expression ("zero?" "(" expression ")") zero?-expr]
    [expression ("if" expression "then" expression "else" expression) if-expr]
    [expression (identifier) var-expr]
    [expression ("let" (separated-list identifier "=" expression ",") "in" expression) let-expr]
    [expression ("proc" "(" (separated-list identifier ":" type ",") ")" "->" type 
                 expression) proc-expr]
    [expression ("(" expression (arbno expression) ")") call-expr]
    [expression ("letrec" (arbno type identifier "(" (separated-list identifier ":" type ",") ")" 
                 "=" expression) "in" expression) letrec-expr]
    [expression ("begin" expression (arbno ";" expression) "end") begin-expr]
    [expression ("set" identifier "=" expression) assign-expr]
    [expression ("+" "(" expression "," expression ")") plus-expr]
    [expression ("list" "(" expression (arbno "," expression) ")") list-expr]
    [expression ("print" "(" expression ")") print-expr]
    [expression ("new" identifier "(" (separated-list expression ",") ")") new-object-expr]
    [expression ("send" expression identifier 
                 "(" (separated-list expression ",") ")") method-call-expr]
    [expression ("super" identifier "(" (separated-list expression ",") ")") super-call-expr]
    [expression ("self") self-expr]
    [expression ("cast" expression identifier) cast-expr]
    [expression ("instanceof" expression identifier) instanceof-expr]
    [type ("Int") int-type]
    [type ("Bool") bool-type]
    [type ("(" (separated-list type "*") "->" type ")") proc-type]
    [type ("Void") void-type]
    [type (identifier) class-type]
    [type ("Listof" type) list-type]
    ))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar-spec))
