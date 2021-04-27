#lang racket
(require "test-runner.rkt"
         "../parse.rkt"
         "../typechecker.rkt"
         "../interp.rkt"
         "../interp-io.rkt")

(test-runner
 (λ (e)
   (let ((e (parse e)))
     (match (type-of '() e)
       ['terr 'err]
       [t (interp e)]))))
;            ['err (error (~a "Something went horribly wrong:" e t))]
;            [v v])]))))

;(test-runner-io (λ (e s) (interp/io (parse e) s)))
; 
;(test-runner-io
;  (λ (e s) 
;    (let ((e (parse e)))
;      (match (type-of '() e)
;        ['terr 'err]
;        [t (interp/io e s)]))))


