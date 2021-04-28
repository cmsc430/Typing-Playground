#lang racket
(provide (all-defined-out))

;;; Definitions

; Our terms
(struct Var (x)       #:prefab)
(struct App (e1 e2)   #:prefab)
(struct Abs (x e)     #:prefab)
(struct True  ()      #:prefab)
(struct False ()      #:prefab)
(struct If (e1 e2 e3) #:prefab)

(define (term? e)
  (match e
    [(Var x)     (symbol? x)]
    [(App e1 e2) (and (term? e1) (term? e2))]
    [(Abs x e)   (and (symbol? x) (term? e))]
    [(True)      #t]
    [(False)     #t]
    [(If e1 e2 e3) (and (term? e1) (term? e2) (term? e3))]
    [_ #f]))

; Our types
(struct TBool ()      #:prefab)
(struct TFun  (t1 t2) #:prefab)
(struct TVar  (a)     #:prefab)

; Type well-formedness
(define (type? t)
  (match t
    [(TBool) #t]
    [(TFun t1 t2) (and (type? t1) (type? t2))]
    [(TVar a) (symbol? a)]
    [_ #f]))

; Schemes: \forall a1...an. t
(struct Scheme (as t) #:prefab)

(define (scheme? s)
  (match s
    [(Scheme as t) (and ((listof symbol?) as) (type? t))]))

;;; Operational Semantics

; Substitution
(define/contract (subst x v e)
  (-> symbol? term? term? term?)
  (match e
    [(Var y) (if (equal? x y) v e)]
    [(App e1 e2) (App (subst x v e1) (subst x v e2))]
    [(Abs y e) (Abs y (if (equal? x y) e (subst x v e)))]
    [(If e1 e2 e3) (If (subst x v e1) (subst x v e2) (subst x v e3))]
    [_ e]))

; Our (big-step) reduction
(define/contract (reduce e)
  (-> term? term?)
  (match e
    [(App e1 e2)
     (match (reduce e1)
       [(Abs x e) (subst x (reduce e2) e)]
       [_ (error "Applying not an abstraction")])]
    [(If e1 e2 e3)
     (match (reduce e1)
       [(True ) (reduce e2)]
       [(False) (reduce e3)])]
    [_ e]))

;;; Static Typing - Hindley-Millner

;; Utility

(define/contract (free-tvars t)
  (-> type? (listof symbol?))
  (match t
    [(TVar x) (list x)]
    [(TFun t1 t2) (remove-duplicates (append (free-tvars t1) (free-tvars t2)))]
    [(TBool)  '()]))

(define/contract (free-svars s)
  (-> scheme? (listof symbol?))
  (match s
    [(Scheme as t) (remq* as (free-tvars t))]))

(define (lookup-with-default l x def)
;  (displayln (~a "Calling lwd " l x def))
  (match l
    ['() def]
    [(cons (list y v) l) (if (equal? x y) v (lookup-with-default l x def))]))

; subst := ListOf (VAR * TYPE)
(define (subst? s)
  ((listof (lambda (e) (match e
                        [(list x t) (and (symbol? x) (type? t))]
                        [_ #f]))) s))

(define/contract (subst-type sub t)
  (-> subst? type? type?)
;  (displayln (~a "Calling subst-type with: " sub t (type? t) (subst? sub)))
  (match t
    [(TVar x) (lookup-with-default sub x t)]
    [(TFun t1 t2) (TFun (subst-type sub t1) (subst-type sub t2))]
    [(TBool)  (TBool)]
    [_ (error (~a "WTF: " t))]
    ))

(define/contract (remove-subst as sub)
  (-> (listof symbol?) subst? subst?)
  (match sub
    ['() '()]
    [(cons (list a t) sub) (if (member a as) (remove-subst as sub) (cons (list a t) (remove-subst as sub)))]))

(define/contract (subst-scheme sub s)
  (-> subst? scheme? scheme?)
;  (displayln (~a "Substing scheme " sub " " s))
  (match s 
    [(Scheme as t)
;     (displayln (~a "Scheme is " as " " t))
     (Scheme as (subst-type (remove-subst as sub) t))]))

; TEnv := ListOf (Var * Scheme)
(define (env? env)
  (listof (lambda (x) (match x
                        [(list a b) (and (symbol? a) (scheme? b))]
                        [_ #f]))))

(define/contract (free-envvars env)
  (-> env? (listof symbol?))
  (map car env))

(define (map-second f l)
  (match l
    ['() '()]
    [(cons (list a b) l) (cons (list a (f b)) l)]))

(define/contract (subst-env sub env)
  (-> subst? env? env?)
;  (displayln (~a "Substing-env " sub " " env))
  (map-second (lambda (s) (subst-scheme sub s)) env))

(define/contract (compose sub1 sub2)
  (-> subst? subst? subst?)
;  (displayln (~a "Calling compose with: " sub1 sub2 (subst? sub1) (subst? sub2)))
  (append (map-second (lambda (t) (subst-type sub1 t)) sub2) sub1))

(define (err? x)
  (match x
    ['terr #t]
    [_     #f]))

(define/contract (unify t1 t2)
  (-> type? type? (or/c subst? err?))
;  (displayln (~a "Calling unify with: " t1 t2 (type? t1) (type? t2)))
  (match* (t1 t2)
    [((TBool) (TBool)) '()]
    [((TVar a) _) (bind a t2)]
    [(_ (TVar a)) (bind a t1)]
    [((TFun l1 r1) (TFun l2 r2))
     (match (unify l1 l2)
       ['terr 'terr]
       [sub1
        (let* ((sr1 (subst-type sub1 r1))
               (sr2 (subst-type sub1 r2))
               (sub2 (unify sr1 sr2)))
          (match sub2
            ['terr 'terr]
            [_ (compose sub2 sub1)]))])]
    [(_ _) 'terr]))

; bind :: TVar -> Type -> Subst
(define/contract (bind a t)
  (-> symbol? type? (or/c subst? err?))
;  (displayln (~a "Calling bind with: " a t (type? t) (subst? (list (list a t)))))
  (cond [(equal? t (TVar a)) '()]
        [(occurs a t) 'terr]
        [else (list (list a t))]))

(define/contract (occurs x t)
  (-> symbol? type? boolean?)
  (member x (free-tvars t)))

(define/contract (instantiate s)
  (-> scheme? type?)
  (match s
    [(Scheme as t) (subst-type (map (lambda (a) (list a (gensym a))) as) t)]))

(define/contract (generalize env t)
  (-> env? type? scheme?)
  (Scheme (remq* (free-envvars env) (free-tvars t)) t))

(define (pairof c1 c2)
  (lambda (x) (match x
                [(list a b) (and (c1 a) (c2 b))]
                [_ #f])))

(define/contract (lookupEnv env x)
  (-> env? symbol? (or/c (pairof list? type?) err?))
  (match env
    ['() 'terr]
    [(cons (list y s) env) (if (equal? x y) (let ((t (instantiate s))) (list '() t)) (lookupEnv env x))]))

(define/contract (infer env e)
  (-> env? term? (or/c (pairof subst? type?) err?))
  (match e
    [(Var x) (lookupEnv env x)]

    [(Abs x e)
     (let* ((tv  (TVar (gensym)))
            (env (cons (list x (Scheme '() tv)) env)))
       (match (infer env e)
         ['terr 'terr]
         [(list sub1 t1) (list sub1 (subst-type sub1 (TFun tv t1)))]))]

    [(App e1 e2)
     (let ((tv (TVar (gensym))))
       (match (infer env e1)
         ['terr 'terr]
         [(list sub1 t1)
          (match (infer (subst-env sub1 env) e2)
            ['terr 'terr]
            [(list sub2 t2)
             (match (unify (subst-type sub2 t1) (TFun t2 tv))
               ['terr 'terr]
               [sub3 (list (compose sub3 (compose sub2 sub1)) (subst-type sub3 tv))])])]))]

    [(If e1 e2 e3)
     (match* ((infer env e1) (infer env e2) (infer env e3))
       [((list sub1 t1) (list sub2 t2) (list sub3 t3))
        (let ((sub4 (unify t1 (TBool)))
              (sub5 (unify t2 t3)))
          (list (compose sub5 (compose sub4 (compose sub3 (compose sub2 sub1)))) (subst-type sub5 t2)))]
       [(_ _ _) 'terr])]

    [(True ) (list '() (TBool))]
    [(False) (list '() (TBool))]
    ))

; Hack - max 26 type variables in result :)
(define lower-letters (map integer->char (sequence->list (in-range 97 (+ 97 26)))))

(define (normalize sc)
  (match sc
    [(Scheme ts body)
     (letrec ((fvs (free-tvars body))
              (ord (map (lambda (v n) (list v (TVar n))) fvs (take lower-letters (length fvs))))
              (norm-type (lambda (t) (match t
                                       [(TFun t1 t2) (TFun (norm-type t1) (norm-type t2))]
                                       [(TBool) (TBool)]
                                       [(TVar a) (cadr (assoc a ord))]))))
       (Scheme (car (map cdr ord)) (norm-type body)))]))

(define (normalize-infer e)
  (match (infer '() e)
    ['terr 'terr]
    [(list sub t) (normalize (generalize '() (subst-type sub t)))]))

(define (pretty-infer e)
;  (displayln (~a "Infer: " (infer '() e)))
;  (displayln (~a "Normalized: " (normalize-infer e)))
  (match (normalize-infer e)
    ['terr 'terr]
    [(Scheme as t) (pretty-scheme as t)]))

(define (parens-if p s)
  (if p (~a "(" s ")") s))

(define (pretty-type t)
;  (displayln t)
  (match t
    [(TVar x) (~a x)]
    [(TBool)  "bool"]
    [(TFun t1 t2) (~a (parens-if (TFun? t1) (pretty-type t1)) " -> " (pretty-type t2))]))

(define (pretty-list-vars as)
  (match as
    ['() " . "]
    [(list a) (~a (pretty-type a) ". ")]
    [(cons a as) (~a (pretty-type a) " " (pretty-list-vars as))]))

(define (pretty-scheme as t)
;  (displayln (~a "Prettying scheme" as " . " t))
  (match as
    ['() (pretty-type t)]
    [_ (~a "forall " (pretty-list-vars as) (pretty-type t))]))

;;; For testing
;(unify (TFun (TVar 'a) (TVar 'b)) (TFun (TFun (TBool) (TVar 'b)) (TVar 'a)))
;(unify (TFun (TVar 'a) (TVar 'b)) (TFun (TFun (TBool) (TBool)) (TVar 'a)))
;(instantiate (Scheme '(a b c d) (TFun (TVar 'a) (TFun (TBool) (TVar 'c)))))
;(generalize (list (list 'a (TBool)) (list 'd (TFun (TBool) (TBool))))
;   (TFun (TVar 'a) (TFun (TVar 'b) (TVar 'c))))
;lower-letters
;(pretty-infer (Abs 'x (Var 'x)))
(pretty-infer (Abs 'y (App (Abs 'x (Var 'x)) (If (True) (Var 'y) (Var 'y)))))
