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

; Term well-formedness
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

;;; Operational Semantics

; Substitution
(define/contract (subst x v e)
  (-> symbol? term? term? term?)
  (match e
    [(Var y)       (if  (equal? x y) v e)]
    [(App e1 e2)   (App (subst x v e1) (subst x v e2))]
    [(Abs y e)     (Abs y (if (equal? x y) e (subst x v e)))]
    [(If e1 e2 e3) (If  (subst x v e1) (subst x v e2) (subst x v e3))]
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

; (reduce (App (Abs 'x (Var 'x)) (True)))

;;; Static typing

; Lambda typing rules:
;
;                (x,t) \in G
;  --------------------------------------
;                G |- x : t
;
;
;   G |- e1 : t1 -> t2         G |- e2 : t1
;  -----------------------------------------
;              G |- (e1 e2) : t2
;
;
;             G, (x, t1) |- e : t2
;  ---------------------------------------
;         G |- lambda (x). e : t1 -> t2 
; 

; Adding polymorphism

;;; Example:
;
; lambda (x). x
;
; What is its type?
; Bool -> Bool?
; Int  -> Int
; (Bool -> Bool) -> (Bool -> Bool)?
;
; let ((f (lambda (x) x))) (if (f #t) (f 42) (f 0))
; 
; ....
;
; forall a. a -> a

; Scheme := forall a1...an. t
(struct Scheme (as t) #:prefab)

(define (scheme? s)
  (match s
    [(Scheme as t) (and ((listof symbol?) as) (type? t))]))

;;; Environments
; Map from Variables to Schemes
;
; Env := ListOf (Var * Scheme)
(define (env? env)
  (listof (lambda (x) (match x
                        [(list a b) (and (symbol? a) (scheme? b))]
                        [_ #f]))))


;;; Hindley-Millner

; General Idea: 
;
; * Typing rules map uniquely onto syntax
; * Run the typing rules 'backwards'
; * When we don't know the type for a subexpression, just make one up!
; * Collect constraints about its usage and solve them as you go.
; * What are constrains? Unifications!

;;; Unification / Substitution, by example
;
; x --- Bool .... [x |-> Bool]
; 
; x -> y --- Bool -> Bool .... [ x |-> Bool , y |-> Bool]
;
; Bool -> y --- x -> Bool .... [ x |-> Bool , y |-> Bool]
;
;
;; Substitutions:
; Map from Variables to Types
;
; subst := ListOf (VAR * TYPE)
(define (subst? s)
  ((listof (lambda (e) (match e
                        [(list x t) (and (symbol? x) (type? t))]
                        [_ #f]))) s))

; Type Errors: 'terr
(define (err? x)
  (match x
    ['terr #t]
    [_     #f]))

;; Unification algorithm:
; Type Type -> Subst
; unify t1 t2 -> sub
; apply sub t1 = apply sub t2
(define/contract (unify t1 t2)
  (-> type? type? (or/c subst? err?))
  (match* (t1 t2)
    
    ; Unifying Bool and Bool -> Empty substitution
    [((TBool) (TBool)) '()]

    ; Unifying a variable with anything -> helper function (a |-> t), but with special cases
    [((TVar a) _) (bind a t2)]
    [(_ (TVar a)) (bind a t1)]

    ; Unifying two functions -> Recursively unify left and right
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

    ; Anything else is a type error!
    [(_ _) 'terr]))

; bind :: TVar Type -> Subst
(define/contract (bind a t)
  (-> symbol? type? (or/c subst? err?))
  (cond ; If the thing we're trying to bind is _the same variable_ -> empty substitution
        [(equal? t (TVar a)) '()] 
        ; If the variable a occurs inside t -> infinite type (error!)
        [(occurs a t) 'terr]
        ; Otherwise, the substitution is literally just [a |-> t]
        [else (list (list a t))]))

; occurs: check if x is a free-variable in t
(define/contract (occurs x t)
  (-> symbol? type? boolean?)
  (member x (free-tvars t)))

; So what's left? compose!

; compose : Subst Subst -> Subst
; sub1 : Map Var Type
; sub2 : Map Var Type
; compose sub1 sub2 := (apply sub1 in sub2) U sub1
(define/contract (compose sub1 sub2)
  (-> subst? subst? subst?)
  (append (map-second (lambda (t) (subst-type sub1 t)) sub2) sub1))

;; Utility functions: substitution/free variables
; Substitution in a type
(define/contract (subst-type sub t)
  (-> subst? type? type?)
  (match t
    [(TVar x) (lookup-with-default sub x t)]
    [(TFun t1 t2) (TFun (subst-type sub t1) (subst-type sub t2))]
    [(TBool)  (TBool)]
    ))

; Substitution in a scheme
; [x |-> bool, y |-> bool]
; (x -> y) ... bool -> bool
; forall x. x -> y ... forall x. x -> bool
(define/contract (subst-scheme sub s)
  (-> subst? scheme? scheme?)
  (match s 
    [(Scheme as t)
     (Scheme as (subst-type (remove-subst as sub) t))]))

; Relies on removing all symbols from a substitution
(define/contract (remove-subst as sub)
  (-> (listof symbol?) subst? subst?)
  (match sub
    ['() '()]
    [(cons (list a t) sub) (if (member a as) (remove-subst as sub) (cons (list a t) (remove-subst as sub)))]))

; Substitution in an environment
(define/contract (subst-env sub env)
  (-> subst? env? env?)
  (map-second (lambda (s) (subst-scheme sub s)) env))

;; Free Variables
; Types
(define/contract (free-tvars t)
  (-> type? (listof symbol?))
  (match t
    [(TVar x) (list x)]
    [(TFun t1 t2) (remove-duplicates (append (free-tvars t1) (free-tvars t2)))]
    [(TBool)  '()]))

; Schemes
(define/contract (free-svars s)
  (-> scheme? (listof symbol?))
  (match s
    [(Scheme as t) (remq* as (free-tvars t))]))

; Envs
(define/contract (free-envvars env)
  (-> env? (listof symbol?))
  (map car env))

;; Utility
; Looks for x in l, returns def if not found
(define (lookup-with-default l x def)
  (match l
    ['() def]
    [(cons (list y v) l) (if (equal? x y) v (lookup-with-default l x def))]))

; Applies f on the second element of each member of l
(define (map-second f l)
  (match l
    ['() '()]
    [(cons (list a b) l) (cons (list a (f b)) l)]))

(define (pairof c1 c2)
  (lambda (x) (match x
                [(list a b) (and (c1 a) (c2 b))]
                [_ #f])))

;;;; Done with unification...
;;;; Let's go back to type inference!
;
; Infer is going to return two things:
;
; - The type that we're trying to infer
; - A _substitution_ that is the solution of all unification constraints encountered
(define/contract (infer env e)
  (-> env? term? (or/c (pairof subst? type?) err?))
  (match e

    ; Variables:
    ; 
    ;                (x,t) \in G
    ;  --------------------------------------
    ;                G |- x : t
    ;
    ; * Look up in the environment
    [(Var x) (lookupEnv env x)]

    ; Abstractions:
    ; 
    ;             G, (x, t1) |- e : t2
    ;  ---------------------------------------
    ;         G |- lambda (x). e : t1 -> t2 
    ;
    ; * Generate a fresh variable t1
    ; * Add it to the context
    ; * Infer the substitution/type of e in the new context
    ; * Apply the substitution to (t1 -> t2) to account for
    ;   any constraints on the fresh variable
    [(Abs x e)
     (let* ((t1  (TVar (gensym)))
            (env (cons (list x (Scheme '() t1)) env)))
       (match (infer env e)
         ['terr 'terr]
         [(list sub1 t2)
          (list sub1 (subst-type sub1 (TFun t1 t2)))]))]

    ; Application:
    ;
    ;   G |- e1 : t1 -> t2         G |- e2 : t1
    ;  -----------------------------------------
    ;              G |- (e1 e2) : t2
    ;
    ; 
    ; * Infer the subst/type of e1
    ; * Infer the subst/type of e2
    ; * Check that the type of e1 is t2 -> tv, where tv is fresh
    ; * Ensure you propage all substitutions as much as possible
    [(App e1 e2)
     (let ((t2 (TVar (gensym))))
       (match (infer env e1)
         ['terr 'terr]
         [(list sub1 t12)
          (match (infer (subst-env sub1 env) e2)
            ['terr 'terr]
            [(list sub2 t1)
             (match (unify (subst-type sub2 t12) (TFun t1 t2))
               ['terr 'terr]
               [sub3 (list (compose sub3 (compose sub2 sub1)) (subst-type sub3 t2))])])]))]

    ; If - easier
    ;
    ; * Infer all three, unify the type of e1 with Bool and e2 with e3
    ; * Compose all substitutions together
    [(If e1 e2 e3)
     (match* ((infer env e1) (infer env e2) (infer env e3))
       [((list sub1 t1) (list sub2 t2) (list sub3 t3))
        (let ((sub4 (unify t1 (TBool)))
              (sub5 (unify t2 t3)))
          (list (compose sub5 (compose sub4 (compose sub3 (compose sub2 sub1)))) (subst-type sub5 t2)))]
       [(_ _ _) 'terr])]

    ; Constants - Boolean, empty substitution
    [(True ) (list '() (TBool))]
    [(False) (list '() (TBool))]
    ))

; Looking up a variable in the environment
(define/contract (lookupEnv env x)
  (-> env? symbol? (or/c (pairof list? type?) err?))
  (match env
    ['() 'terr]
    [(cons (list y s) env)
     (if (equal? x y)
         ; How do we go from a scheme to a type?
         ; .. just generate fresh variables for all foralls!
         (let ((t (instantiate s))) (list '() t))
         (lookupEnv env x))]))

; Instantiate takes a scheme and returns a type with fresh variables
(define/contract (instantiate s)
  (-> scheme? type?)
  (match s
    [(Scheme as t) (subst-type (map (lambda (a) (list a (gensym a))) as) t)]))

; Generalize does the opposite:
; Gather all free vars and make a scheme.
(define/contract (generalize env t)
  (-> env? type? scheme?)
  (Scheme (remq* (free-envvars env) (free-tvars t)) t))

; We're now done!
; ... or are we?

(infer '() (Abs 'x (Var 'x)))









; Let's normalize variable names!

; lower-letters := #\a #\b .... #\z
; Hack! Allows for max 26 type variables in result :) Could play with streams to be correct
(define lower-letters (map integer->char (sequence->list (in-range 97 (+ 97 26)))))

; Take a scheme, and use variables from lower-letters to replace ones by gensym
(define (normalize sc)
  (match sc
    [(Scheme ts body)
     (letrec (; Grab the free variables of the main type
              (fvs (free-tvars body))
              ; Grab as many lower-letters creating a map from the variables to the letters
              (ord (map (lambda (v n) (list v (TVar n))) fvs (take lower-letters (length fvs))))
              ; Recursively replace each variable that appears in the body with its associated letter
              (norm-type (lambda (t) (match t
                                       [(TFun t1 t2) (TFun (norm-type t1) (norm-type t2))]
                                       [(TBool) (TBool)]
                                       [(TVar a) (cadr (assoc a ord))]))))
       (Scheme (car (map cdr ord)) (norm-type body)))]))

; Helper that normalizes what we infer
(define (normalize-infer e)
  (match (infer '() e)
    ['terr 'terr]
    [(list sub t) (normalize (generalize '() (subst-type sub t)))]))

; We're now done!!
; ... or are we?

(normalize-infer (Abs 'x (Var 'x)))

;;; Let's pretty print!
; Pretty print the result of infer
(define (pretty-infer e)
  (match (normalize-infer e)
    ['terr 'terr]
    [(Scheme as t) (pretty-scheme as t)]))

; Pretty print a scheme
(define (pretty-scheme as t)
  (match as
    ['() (pretty-type t)]
    [_ (~a "forall " (pretty-list-vars as) (pretty-type t))]))

; Pretty print a list of variables
(define (pretty-list-vars as)
  (match as
    ['() " . "]
    [(list a) (~a (pretty-type a) ". ")]
    [(cons a as) (~a (pretty-type a) " " (pretty-list-vars as))]))

; Pretty print a type
(define (pretty-type t)
  (match t
    [(TVar x) (~a x)]
    [(TBool)  "bool"]
    [(TFun t1 t2) (~a (parens-if (TFun? t1) (pretty-type t1)) " -> " (pretty-type t2))]))

; Wrap parentheses around a predicate
(define (parens-if p s)
  (if p (~a "(" s ")") s))

;; Are we done NOW?
(pretty-infer (Abs 'x (Var 'x)))
