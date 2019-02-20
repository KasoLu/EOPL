(load "types.rkt")

;InpPgm ::= InpExp
;InpExp ::= Number
;InpExp ::= -(InpExp, InpExp)
;InpExp ::= zero?(InpExp)
;InpExp ::= if InpExp then InpExp else InpExp
;InpExp ::= Identifier
;InpExp ::= let {Identifier = InpExp}* in InpExp
;InpExp ::= letrec {Identifier ({Identifier}*) = InpExp}* in InpExp
;InpExp ::= proc({Identifier}*) InpExp
;InpExp ::= (InpExp {InpExp}*)
;InpExp ::= +({InpExp}*(,))

;arbno | separated-list

(define scanner-spec
  '([whitespace (whitespace) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number ((or "-" "") digit (arbno digit)) number]))

(define grammar-spec-inp
  '([inpgam (inpexp) a-inppgm]
    [inpexp (number) inp-const-exp]
    [inpexp (identifier) inp-var-exp]
    [inpexp ("-" "(" inpexp "," inpexp ")") inp-diff-exp]
    [inpexp ("zero?" "(" inpexp ")") inp-zero?-exp]
    [inpexp ("if" inpexp "then" inpexp "else" inpexp) inp-if-exp]
    [inpexp ("let" (arbno identifier "=" inpexp) "in" inpexp) inp-let-exp]
    [inpexp ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" inpexp) 
             "in" inpexp) inp-letrec-exp]
    [inpexp ("proc" "(" (arbno identifier) ")" inpexp) inp-proc-exp]
    [inpexp ("(" inpexp (arbno inpexp) ")") inp-call-exp]
    [inpexp ("+" "(" (separated-list inpexp ",") ")") inp-sum-exp]
    ))

(define scan&parse-inp
  (sllgen:make-string-parser scanner-spec grammar-spec-inp))
