#lang racket
(require "test-runner.rkt"
         "../parse.rkt"
         "../typechecker.rkt"
         "../compile.rkt"
         "../unload-bits-asm.rkt"
         a86/interp)

;; link with runtime for IO operations
(unless (file-exists? "../runtime.o")
  (system "make -C .. runtime.o"))
(current-objs
 (list (path->string (normalize-path "../runtime.o"))))

(test-runner
 (λ (e)
   (let ((e (parse e)))
     (match (type-of '() e)
       ['terr 'err]
       [t (unload/free t (asm-interp (compile e)))]))))

(test-runner-io
 (λ (e s)
   (let ((e (parse e)))
     (match (type-of '() e)
       ['terr 'err]
       [t (match (asm-interp/io (compile e) s)
            ['err 'err]
            [(cons r o) (cons (unload/free t r) o)])]))))
