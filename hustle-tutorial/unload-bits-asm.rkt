#lang racket
(provide unload/free unload-value)
(require "types.rkt"
         ffi/unsafe)

;; Answer* -> Answer
(define (unload/free t a)
  (match a
    ['err 'err]
    [(cons h v) (begin0 (unload-value t v)
                        (free h))]))

;; Value* -> Value
(define (unload-value t v)
  (match t
    [(list 'tbox t) (box (unload-value t (heap-ref v)))]
    [_  (bits->value t v)]))
;    
;    [(? imm-bits?) (bits->value v)]
;    [(? box-bits? i)
;     (box (unload-value (heap-ref i)))]
;    [(? cons-bits? i)
;     (cons (unload-value (heap-ref (+ i (arithmetic-shift 1 imm-shift))))
;           (unload-value (heap-ref i)))]))

(define (untag i) i)
;n  (arithmetic-shift (arithmetic-shift i (- (integer-length ptr-mask)))
;                    (integer-length ptr-mask)))

(define (heap-ref i)
  (ptr-ref (cast (untag i) _int64 _pointer) _uint64))
