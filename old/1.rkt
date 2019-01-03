; 1.7
; nth-elem : List x Int -> SchemeVal
(define (nth-elem lst n)
  (define (nth-elem-aux lst n ori-lst ori-n)
    (if (null? lst)
      (report-list-too-short ori-lst ori-n)
      (if (zero? n)
        (car lst)
        (nth-elem-aux (cdr lst) (- n 1) ori-lst ori-n))))
  (nth-elem-aux lst n lst n))
(define (report-list-too-short lst n)
  (eopl:error 'nth-elem "~s does not have ~s elements.~%" lst (+ n 1)))

; 1.9
; remove : Sym x Listof(Sym) -> Listof(Sym)
(define (remove s los)
  (if (null? los)
    '()
    (if (eqv? (car los) s)
      (remove s (cdr los))
      (cons (car los) (remove s (cdr los))))))

; 1.12
; subst : Sym x Sym x S-list -> S-list
(define (subst new old slist)
  (if (null? slist)
    '()
    (cons (let ([sexp (car slist)])
            (if (symbol? sexp)
              (if (eqv? sexp old)
                new
                sexp)
              (subst new old sexp)))
          (subst new old (cdr slist)))))

; 1.13
; S-list ::= ({S-exp}*)
; S-exp  ::= Symbol | S-list
(define (subst new old slist)
  (define (subst-in-sexp sexp)
    (if (symbol? sexp)
      (if (eqv? sexp old)
        new
        sexp)
      (subst new old sexp)))
  (map subst-in-sexp slist))

; 1.15
; duple : Int x SchemeVal -> Listof(SchemeVal)
(define (duple n x)
  (if (zero? n)
    '()
    (cons x (duple (- n 1) x))))

; 1.16
; invert : Listof(List(SchemeVal, SchemeVal)) -> Listof(List(SchemeVal, SchemeVal))
(define (invert lst)
  (define (invert-aux val)
    (list (cadr val) (car val)))
  (map invert-aux lst))

; 1.17
; down : Listof(SchemeVal) -> Listof(List(SchemeVal))
(define (down lst)
  (map (lambda (val) (list val)) lst))

; 1.18
; swapper : Symbol x Symbol x S-list -> S-list
(define (swapper s1 s2 slist)
  (define (swapper-aux sexp)
    (if (symbol? sexp)
      (cond [(eqv? sexp s1) s2]
            [(eqv? sexp s2) s1]
            [else sexp])
      (swapper s1 s2 sexp)))
  (map swapper-aux slist))

; 1.19
; list-set : Listof(SchemeVal) x Int x SchemeVal -> Listof(SchemeVal)
(define (list-set lst n x)
  (cond [(null? lst) '()]
        [(zero? n) (cons x (list-set (cdr lst) (- n 1) x))]
        [else (cons (car lst) (list-set (cdr lst) (- n 1) x))]))

; 1.20
; count-occurrences : Symbol x S-list -> Int
(define (count-occurrences s slist)
  (define (slist-aux slist n)
    (if (null? slist)
      n
      (+ (sexp-aux (car slist) n)
         (slist-aux (cdr slist) n))))
  (define (sexp-aux sexp n)
    (if (symbol? sexp)
      (if (eqv? sexp s) (+ n 1) n)
      (slist-aux sexp n)))
  (slist-aux slist 0))

; 1.21
; product : Listof(Symbol) x Listof(Symbol) -> Listof(List(Symbol, Symbol))
(define (product sos1 sos2)
  (let ([trans (map (lambda (s1) 
                      (map (lambda (s2) 
                             (list s1 s2))
                           sos2))
                    sos1)])
    (foldr append '() trans)))

; 1.22
; filter-in : Pred x Listof(SchemeVal) -> Listof(SchemeVal)
(define (filter-in pred lst)
  (cond [(null? lst) '()]
        [(pred (car lst)) (cons (car lst) (filter-in pred (cdr lst)))]
        [else (filter-in pred (cdr lst))]))

; 1.23
; list-index : Pred x Listof(SchemeVal) -> Listof(SchemeVal)
(define (list-index pred lst)
  (define (list-index-aux lst i)
    (cond [(null? lst) #f]
          [(pred (car lst)) i]
          [else (list-index-aux (cdr lst) (+ i 1))]))
  (list-index-aux lst 0))

; 1.24
; every? : Pred x Listof(SchemeVal) -> Listof(SchemeVal)
(define (every? pred lst)
  (cond [(null? lst) #t]
        [else (and (pred (car lst))
                   (every? pred (cdr lst)))]))

; 1.25
; exists? : Pred x Listof(SchemeVal) -> Listof(SchemeVal)
(define (exists? pred lst)
  (cond [(null? lst) #f]
        [else (or (pred (car lst))
                  (exists? pred (cdr lst)))]))

; 1.26
; up : Listof(SchemeVal) -> Listof(SchemeVal)
(define (up lst)
  (cond [(null? lst) '()]
        [(pair? (car lst)) (append (car lst) (up (cdr lst)))]
        [else (cons (car lst) (up (cdr lst)))]))

; 1.27
; flatten : S-list -> Listof(Symbol)
(define (flatten slist)
  (define (sexp-aux sexp)
    (if (symbol? sexp)
      (list sexp)
      (flatten sexp)))
  (foldr append '() (map sexp-aux slist)))

; 1.28
; merge : Listof(Int) x Listof(Int) -> Listof(Int)
(define (merge loi1 loi2)
  (cond [(and (null? loi1) (null? loi2)) '()]
        [(null? loi1) (cons (car loi2) (merge loi1 (cdr loi2)))]
        [(null? loi2) (cons (car loi1) (merge (cdr loi1) loi2))]
        [else (let ([i1 (car loi1)] [i2 (car loi2)])
                (if (< i1 i2)
                  (cons i1 (merge (cdr loi1) loi2))
                  (cons i2 (merge loi1 (cdr loi2)))))]))

; 1.29
; sort : Listof(Int) -> Listof(Int)
(define (sort* loi)
  (if (null? loi)
    '()
    (append (sort* (filter (lambda (i) (< i (car loi))) (cdr loi)))
            (list (car loi))
            (sort* (filter (lambda (i) (>= i (car loi))) (cdr loi))))))

; 1.30
; sort/pred : Listof(Int) -> Listof(Int)
(define (sort/pred pred loi)
  (if (null? loi)
    '()
    (append (sort/pred pred (filter (lambda (i) (pred i (car loi))) (cdr loi)))
            (list (car loi))
            (sort/pred pred (filter (lambda (i) (not (pred i (car loi)))) (cdr loi))))))
; 1.31
; B-Tree ::= Int | (Symbol B-Tree B-Tree)
; leaf : Int -> B-Tree
(define (leaf i) i)
; interior-node : Symbol x B-Tree x B-Tree -> B-Tree
(define (interior-node s btl btr) (list s btl btr))
; leaf? : B-Tree -> Bool
(define (leaf? bt) (number? bt))
; lson : B-Tree -> B-Tree
(define (lson bt)
  (if (leaf? bt)
    (eopl:error 'lson "is a leaf node ~s" bt)
    (cadr bt)))
; rson : B-Tree -> B-Tree
(define (rson bt)
  (if (leaf? bt)
    (eopl:error 'rson "is a leaf node ~s" bt)
    (caddr bt)))
; contents-of : B-Tree -> Int
(define (contents-of bt)
  (if (leaf? bt)
    bt
    (car bt)))

; 1.32
; double-tree : B-Tree -> B-Tree
(define (double-tree bt)
  (let ([val (contents-of bt)])
    (if (leaf? bt)
      (leaf (* 2 val))
      (interior-node val
                     (double-tree (lson bt)) 
                     (double-tree (rson bt))))))

; 1.33
; mark-leaves-with-red-depth : B-Tree -> B-Tree
(define (mark-leaves-with-red-depth bt)
  (define (aux bt depth)
    (let ([cont (contents-of bt)])
      (cond [(leaf? bt) (leaf depth)]
            [(eqv? (contents-of bt) 'red) 
             (let ([new-depth (+ 1 depth)])
               (interior-node cont 
                              (aux (lson bt) new-depth)
                              (aux (rson bt) new-depth)))]
            [else (interior-node cont
                                 (aux (lson bt) depth)
                                 (aux (rson bt) depth))])))
  (aux bt 0))

; 1.34
; BSTree ::= () | (Int BSTree BSTree)
; path : Int x BSTree -> Listof(Symbol)
(define (path n bst)
  (cond [(null? bst) '()]
        [(= n (car bst)) '()]
        [(< n (car bst)) (cons 'left (path n (cadr bst)))]
        [(> n (car bst)) (cons 'right (path n (caddr bst)))]))

; 1.35
; number-leaves : B-Tree -> B-Tree
(define (number-leaves bt)
  (define (number-leaves-aux bt i)
    (if (leaf? bt)
      (cons (+ i 1) (leaf i))
      (let ([lt (number-leaves-aux (lson bt) i)])
        (let ([rt (number-leaves-aux (rson bt) (car lt))])
          (cons (car rt)
                (interior-node (contents-of bt) (cdr lt) (cdr rt)))))))
  (cdr (number-leaves-aux bt 0)))

; 1.36
; number-elems : List -> Listof(List(Int, SchemeVal))
(define (number-elems lst)
  (define (g val lst)
    (cons val (map (lambda (v) (list (+ 1 (car v)) (cadr v))) lst)))
  (if (null? lst) 
    '()
    (g (list 0 (car lst))
       (number-elems (cdr lst)))))
