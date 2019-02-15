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

;CpsPgm ::= TsfExp
;SmpExp ::= Number
;SmpExp ::= Identifier
;SmpExp ::= -(SmpExp, SmpExp)
;SmpExp ::= zero?(SmpExp)
;SmpExp ::= proc({Identifier}*) TsfExp
;TsfExp ::= SmpExp
;TsfExp ::= let {Identifier = SmpExp}* in TsfExp
;TsfExp ::= letrec {Identifier ({Identifier}*) = TsfExp}* in TsfExp
;TsfExp ::= if SmpExp then TsfExp else TsfExp
;TsfExp ::= (SmpExp {SmpExp}*)

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
    ))

(define scan&parse-inp
  (sllgen:make-string-parser scanner-spec grammar-spec-inp))

(define grammar-spec-cps
  '([cpspgm (tsfexp) a-cpspgm]
    [smpexp (number) smp-const-exp]
    [smpexp (identifier) smp-var-exp]
    [smpexp ("-" "(" smpexp "," smpexp ")") smp-diff-exp]
    [smpexp ("zero?" "(" smpexp ")") smp-zero?-exp]
    [tsfexp (smpexp) smpexp->tsfexp]
    [tsfexp ("if" smpexp "then" tsfexp "else" tsfexp) tsf-if-exp]
    [tsfexp ("let" (arbno identifier "=" smpexp) "in" tsfexp) tsf-let-exp]
    [tsfexp ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" tsfexp)
             "in" tsfexp) tsf-letrec-exp]
    [smpexp ("proc" "(" (arbno identifier) ")" tsfexp) smp-proc-exp]
    [tsfexp ("(" smpexp (arbno smpexp) ")") tsf-call-exp]
    ))

(define scan&parse-cps
  (sllgen:make-string-parser scanner-spec grammar-spec-cps))
