(load "types.rkt")

;CpsPgm ::= TsfExp
;SmpExp ::= Number
;SmpExp ::= Identifier
;SmpExp ::= -(CstExp, CstExp)
;SmpExp ::= zero?(CstExp)
;SmpExp ::= proc({Identifier}*) TsfExp
;TsfExp ::= SmpExp
;TsfExp ::= let {Identifier = SmpExp}* in TsfExp
;TsfExp ::= letrec {Identifier ({Identifier}*) = TsfExp}* in TsfExp
;TsfExp ::= if SmpExp then TsfExp else TsfExp
;TsfExp ::= (SmpExp {SmpExp}*)
;CstExp ::= Number | Identifier

(define scanner-spec
  '([whitespace (whitespace) skip]
    [identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol]
    [number ((or "-" "") digit (arbno digit)) number]))

(define grammar-spec-cps
  '([cpspgm (tsfexp) a-cpspgm]
    [smpexp (number) smp-const-exp]
    [smpexp (identifier) smp-var-exp]
    [smpexp ("-" "(" cstexp "," cstexp ")") smp-diff-exp]
    [smpexp ("zero?" "(" cstexp ")") smp-zero?-exp]
    [tsfexp (smpexp) smpexp->tsfexp]
    [tsfexp ("if" smpexp "then" tsfexp "else" tsfexp) tsf-if-exp]
    [tsfexp ("let" (arbno identifier "=" smpexp) "in" tsfexp) tsf-let-exp]
    [tsfexp ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" tsfexp)
             "in" tsfexp) tsf-letrec-exp]
    [smpexp ("proc" "(" (arbno identifier) ")" tsfexp) smp-proc-exp]
    [tsfexp ("(" smpexp (arbno smpexp) ")") tsf-call-exp]
    [cstexp (number) cst-const-exp]
    [cstexp (identifier) cst-var-exp]
    ))

(define scan&parse-cps
  (sllgen:make-string-parser scanner-spec grammar-spec-cps))
