#lang racket
(provide (all-defined-out))

(require "ast.rkt")

; 'tint
; 'tbool
; 'tchar
; 'tvoid
; (list 'tbox _)

; (define (f x) (box (f x)))
; x : 'a
; f : 'a -> 'b
; f : 'a -> 'box ('b)
; 'a -> 'b =:= 'a -> 'box ('b)
; 'b =:= 'box ('b)

; 
; ------------------------
;   G |- (TInt n) : 'tint

;       |- e : 'tint
; ------------------------
;   G |- (add1 e) : 'tint


(define (check G e te tr)
  (if (equal? (type-of G e) te) tr 'terr))

; Expr -> Type
(define (type-of G e)
  (match e
    [(Int  _) 'tint]
    [(Bool _) 'tbool]
    [(Char _) 'tchar]
    [(Eof)    'tchar]
    [(Prim0 'read-byte) 'tchar]
    [(Prim0 'peek-byte) 'tchar]    
    [(Prim0 'void) 'tvoid]    
    [(Prim1 'add1 e) (check G e 'tint 'tint)]
    [(Prim1 'sub1 e) (check G e 'tint 'tint)]
    [(Prim1 'zero? e) (check G e 'tint 'tbool)]
    [(Prim1 'integer->char e) (check G e 'tint 'tchar)]
    [(Prim1 'char->integer e) (check G e 'tchar 'tint)]        
    [(Prim1 'write-byte  e) (check G e 'tchar 'tvoid)]
    [(Prim1 'eof-object? e) (check G e 'tchar 'tbool)]
    [(Prim1 'box e)
     (match (type-of G e)
       ['terr 'terr]
       [t     (list 'tbox t)])]
    [(Prim1 'unbox e)
     (match (type-of G e)
       [(list 'tbox t) t]
       [_              'terr])]
    [(If e1 e2 e3)
     (match (type-of G e1)
       ['tbool (let ((t2 (type-of G e2))
                     (t3 (type-of G e3)))
                 (if (equal? t2 t3) t2 'terr))]
       [_  'terr])]
    [(Prim2 '+ e1 e2)
     (check G e1 'tint (check G e2 'tint 'tint))]
    [(Prim2 '- e1 e2)
     (check G e1 'tint (check G e2 'tint 'tint))]
    [(Prim2 'eq? e1 e2)
     (check G e1 'tint (check G e2 'tint 'tbool))]
    [(Begin e1 e2)
     (match (type-of G e1)
       ['terr 'terr]
       [_ (type-of G e2)])]
    [(Let x e1 e2)
     (match (type-of G e1)
       ['terr 'terr]
       [t   (type-of (cons (list x t) G) e2)])]
    [(Var x) (tlookup x G)]
    ))

(define (tlookup x G)
  (match G
    ['() 'terr]
    [(cons (list y t) G) (if (equal? x y) t (tlookup x G))]))



         
