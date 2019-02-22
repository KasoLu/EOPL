(load "types.rkt")

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
    [inpexp ("try" inpexp "catch" "(" identifier ")" inpexp) inp-try-exp]
    [inpexp ("raise" inpexp) inp-raise-exp]
    ))

(define scan&parse-inp
  (sllgen:make-string-parser scanner-spec grammar-spec-inp))

(define grammar-spec-out
  '([outpgm (tpfexp) a-outpgm]
    [smpexp (number) smp-const-exp]
    [smpexp (identifier) smp-var-exp]
    [smpexp ("-" "(" smpexp "," smpexp ")") smp-diff-exp]
    [smpexp ("zero?" "(" smpexp ")") smp-zero?-exp]
    [smpexp ("proc" "(" (arbno identifier) ")" tpfexp) smp-proc-exp]
    [smpexp ("+" "(" (separated-list smpexp ",") ")") smp-sum-exp]
    [tpfexp (smpexp) smpexp->tpfexp]
    [tpfexp ("if" smpexp "then" tpfexp "else" tpfexp) tpf-if-exp]
    [tpfexp ("let" (arbno identifier "=" smpexp) "in" tpfexp) tpf-let-exp]
    [tpfexp ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" tpfexp)
             "in" tpfexp) tpf-letrec-exp]
    [tpfexp ("(" smpexp (arbno smpexp) ")") tpf-call-exp]
    ))

(define scan&parse-out
  (sllgen:make-string-parser scanner-spec grammar-spec-out))
