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
(struct TAnn (e t)    #:prefab)

; Our types
(struct TBool ()      #:prefab)
(struct TFun  (t1 t2) #:prefab)

;;; Operational Semantics

; Substitution
(define (subst x v e)
  (match e
    [(Var y) (if (equal? x y) v e)]
    [(App e1 e2) (App (subst x v e1) (subst x v e2))]
    [(Abs y e) (Abs y (if (equal? x y) e (subst x v e)))]
    [(If e1 e2 e3) (If (subst x v e1) (subst x v e2) (subst x v e3))]
    [(TAnn e t) (TAnn (subst x v e) t)]
    [_ e]))

; Our (big-step) reduction
(define (reduce e)
  (match e
    [(App e1 e2)
     (match (reduce e1)
       [(Abs x e) (subst x (reduce e2) e)]
       [_ (error "Applying not an abstraction")])]
    [(If e1 e2 e3)
     (match (reduce e1)
       [(True ) (reduce e2)]
       [(False) (reduce e3)])]
    [(TAnn e t) (reduce e)]
    [_ e]))

;;; Static Typing 

; Two modes, Checking and Inferring
(define (check G e t)
  (match e
    [(Var x) (equal? t (tlookup x G))]
    [(App e1 e2)
     (match (infer G e1)
       [(TFun t1 t2) (check G e2 t1)]
       [_ #f])]
    [(Abs x e)
     (match t
       [(TFun t1 t2) (check (cons (list x t1) G) e t2)]
       [_ #f])]
    [(True)  (equal? t (TBool))]
    [(False) (equal? t (TBool))]
    [(If e1 e2 e3)
     (and (check G e1 (TBool)) (check G e2 t) (check G e3 t))]
    [(TAnn e t2)
     (if (equal? t t2) (check G e t) #f)]
    ))

(define (infer G e)
  (match e
    [(Var x) (tlookup x G)]
    [(True)  (TBool)]
    [(False) (TBool)]
    [(TAnn e t) (check G e t)]
    [_ (error "Not enough information to infer!")]
    ))

(define (tlookup x G)
  (match G
    ['() 'terr]
    [(cons (list y t) G) (if (equal? x y) t (tlookup x G))]))

;;; For testing
(define l1 (Abs 'x (Var 'x)))
(define l2 (App l1 (True)))
(define l3 (App (App l1 l1) l2))
(define l4 (If l3 (App l1 (False)) (App l1 (True))))
