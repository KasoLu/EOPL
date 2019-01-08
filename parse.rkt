(load "types.rkt")

; Expression ::= set Identifier = Expression
; Program    ::= Statement
; Statement  ::= Identifier = Expression
;            ::= print Expression
;            ::= { {Statement}*(;) }
;            ::= if Expression Statement Statement
;            ::= while Expression Statement
;            ::= var {Identifier}*(,) ; Statement
;            ::= read Identifier
;            ::= do Statement while Expression

(define scanner-spec
  '([whitespace (whitespace) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number ((or "-" "") digit (arbno digit)) number]))
(define grammar-spec
  '([program (statement) a-program]
    [statement (identifier "=" expression) assign-stmt]
    [statement ("print" expression) print-stmt]
    [statement ("{" (separated-list statement ";") "}") multi-stmt]
    [statement ("if" expression statement statement) if-stmt]
    [statement ("while" expression statement) while-stmt]
    [statement ("var" (separated-list identifier ",") ";" statement) var-stmt]
    [statement ("read" identifier) read-stmt]
    [statement ("do" statement "while" expression) do-while-stmt]
    [expression (number) const-exp]
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("not" "(" expression ")") not-exp]
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
    ))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar-spec))
