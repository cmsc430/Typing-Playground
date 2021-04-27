#lang racket
(provide (all-defined-out))

;(define imm-shift          3)
;(define imm-mask       #b111)
;(define ptr-mask       #b111)
;(define type-box       #b001)
;(define type-cons      #b010)
;(define int-shift  (+ 1 imm-shift))
;(define char-shift (+ 2 imm-shift))
;(define type-int      #b0000)
;(define mask-int      #b1111)
;(define type-char    #b01000)
;(define mask-char    #b11111)
(define val-true   #b1)
(define val-false  #b0)
(define val-eof    1114112)
(define val-void   0)

(define (bits->value t b)
  (match t
    ['tint b]
    ['tbool (if (= b val-true) #t #f)]
    ['tchar (if (= b val-eof) eof (integer->char b))]
    ['tvoid (void)]))

(define (imm->bits v)
  (cond [(eof-object? v) val-eof]
        [(eq? v #t) val-true]
        [(eq? v #f) val-false]
        [(void? v)  val-void]
        [(integer? v) v]
        [(char? v)    (char->integer v)]))

;(define (imm-bits? v)
;  (zero? (bitwise-and v imm-mask)))
; 
;(define (int-bits? v)
;  (zero? (bitwise-and v mask-int)))
; 
;(define (char-bits? v)
;  (= type-char (bitwise-and v mask-char)))
; 
;(define (cons-bits? v)
;  (zero? (bitwise-xor (bitwise-and v imm-mask) type-cons)))
; 
;(define (box-bits? v)
;  (zero? (bitwise-xor (bitwise-and v imm-mask) type-box)))
