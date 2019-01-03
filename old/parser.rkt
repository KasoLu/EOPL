; Scanner-spec      ::= ({Regexp-and-action}*)
; Regexp-and-action ::= (Name ({Regexp}*) Action)
; Name              ::= Symbol
; Regexp            ::= String | letter | digit | whitespace | any
;                   ::= (not Character) | (or {Regexp}*)
;                   ::= (arbno Regexp) | (concat {Regexp}*)
; Action            ::= skip | symbol | number | string

; Grammer     ::= ({Production}*)
; Production  ::= (Lhs ({Rhs-item}*) Prod-name)
; Lhs         ::= Symbol
; Rhs-item    ::= Symbol | String
;             ::= (arbno {Rhs-item}*)
;             ::= (separated-list {Rhs-item}* Striing)
; Prod-name   ::= Symbol


(define identifier? symbol?)

(define-datatype program program?
  [a-program 
    [exp1 expression?]])
(define-datatype expression expression?
  [const-exp 
    [num number?]]
  [diff-exp 
    [exp1 expression?]
    [exp2 expression?]]
  [zero?-exp
    [exp1 expression?]]
  [if-exp
    [exp1 expression?]
    [exp2 expression?]
    [exp3 expression?]]
  [var-exp
    [var identifier?]]
  [let-exp
    [var identifier?]
    [exp1 expression?]
    [body expression?]])

(define scanner-let
  '([whitespace (whitespace) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number ((or "-" "") digit (arbno digit)) number]))
(define grammar-let
  '([program (expression) a-program]
    [expression (number) const-exp]
    [expression ("-" "(" expression "," expression ")") diff-exp]
    [expression ("zero?" "(" expression ")" "," "(" expression ")") zero?-exp]
    [expression ("if" expression "then" expression "else" expression) if-exp]
    [expression (identifier) var-exp]
    [expression ("let" identifier "=" expression "in" expression) let-exp]))

(define scanner
  (sllgen:make-string-scanner scanner-let grammar-let))
(define scan&parse-let
  (sllgen:make-string-parser scanner-let grammar-let))

