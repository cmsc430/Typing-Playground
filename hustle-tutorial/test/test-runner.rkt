#lang racket
(provide test-runner test-runner-io)
(require rackunit)

(define (test-runner run) 
  ;; Abscond examples
  (check-equal? (run 7) 7)
  (check-equal? (run -8) -8)

  ;; Blackmail examples
  (check-equal? (run '(add1 (add1 7))) 9)
  (check-equal? (run '(add1 (sub1 7))) 7)

  ;; Con examples
  (check-equal? (run '(if (zero? 0) 1 2)) 1)
  (check-equal? (run '(if (zero? 1) 1 2)) 2)
  (check-equal? (run '(if (zero? -7) 1 2)) 2)
  (check-equal? (run '(if (zero? 0)
                          (if (zero? 1) 1 2)
                          7))
                2)
  (check-equal? (run '(if (zero? (if (zero? 0) 1 0))
                          (if (zero? 1) 1 2)
                          7))
                7)

  ;; Dupe examples
  (check-equal? (run #t) #t)
  (check-equal? (run #f) #f)
  (check-equal? (run (if #t 1 2)) 1)
  (check-equal? (run (if #f 1 2)) 2)
  (check-equal? (run (if 0 1 2)) 1)
  (check-equal? (run '(if #t 3 4)) 3)
  (check-equal? (run '(if #f 3 4)) 4)

; No longer valid!
;  (check-equal? (run '(if  0 3 4)) 4)
  (check-equal? (run '(zero? 4)) #f)
  (check-equal? (run '(zero? 0)) #t)  

  ;; Dodger examples
  (check-equal? (run #\a) #\a)
  (check-equal? (run #\b) #\b)

; No longer supported!
;  (check-equal? (run '(char? #\a)) #t)
;  (check-equal? (run '(char? #t)) #f)  
;  (check-equal? (run '(char? 8)) #f) 
  (check-equal? (run '(char->integer #\a)) (char->integer #\a))
  (check-equal? (run '(integer->char 955)) #\λ)  

  ;; Extort examples
  (check-equal? (run '(add1 #f)) 'err)
  (check-equal? (run '(sub1 #f)) 'err)
  (check-equal? (run '(zero? #f)) 'err)
  (check-equal? (run '(char->integer #f)) 'err)
  (check-equal? (run '(integer->char #f)) 'err)
  (check-equal? (run '(integer->char -1)) 'err)
  (check-equal? (run '(write-char #f)) 'err)
  (check-equal? (run '(write-char -1)) 'err)
  (check-equal? (run '(write-char 256)) 'err)

  ;; Fraud examples
  (check-equal? (run '(let ((x 7)) x)) 7)
  (check-equal? (run '(let ((x 7)) 2)) 2)
  (check-equal? (run '(let ((x 7)) (add1 x))) 8)
  (check-equal? (run '(let ((x (add1 7))) x)) 8)
  (check-equal? (run '(let ((x 7)) (let ((y 2)) x))) 7)
  (check-equal? (run '(let ((x 7)) (let ((x 2)) x))) 2)
  (check-equal? (run '(let ((x 7)) (let ((x (add1 x))) x))) 8)

  (check-equal? (run '(let ((x 0))
                        (if (zero? x) 7 8)))
                7)
  (check-equal? (run '(let ((x 1))
                        (add1 (if (zero? x) 7 8))))
                9)
  (check-equal? (run '(+ 3 4)) 7)
  (check-equal? (run '(- 3 4)) -1)
  (check-equal? (run '(+ (+ 2 1) 4)) 7)
  (check-equal? (run '(+ (+ 2 1) (+ 2 2))) 7)
  (check-equal? (run '(let ((x (+ 1 2)))
                        (let ((z (- 4 x)))
                          (+ (+ x x) z))))
                7)

  ;; Hustle examples (only box)
;  (check-equal? (run ''()) '())  
  (check-equal? (run '(box 1)) (box 1))
;  (check-equal? (run '(cons 1 2)) (cons 1 2))
  (check-equal? (run '(unbox (box 1))) 1)
;  (check-equal? (run '(car (cons 1 2))) 1)
;  (check-equal? (run '(cdr (cons 1 2))) 2)
;  (check-equal? (run '(cons 1 '())) (list 1))
;  (check-equal? (run '(let ((x (cons 1 2)))
;                        (begin (cdr x)
;                               (car x))))
;                1)
;  (check-equal? (run '(let ((x (cons 1 2)))
;                        (let ((y (box 3)))
;                          (unbox y))))
;                3)
  )

; 

(define (test-runner-io run)
  ;; Evildoer examples
  (check-equal? (run 7 "") (cons 7 ""))
  (check-equal? (run '(write-char (integer->char 97)) "") (cons (void) "a"))
  (check-equal? (run '(read-char) "a") (cons #\a ""))
  (check-equal? (run '(begin (write-char #\a) (read-char)) "b")
                (cons #\b "a"))
  (check-equal? (run '(read-char) "") (cons eof ""))
  (check-equal? (run '(eof-object? (read-char)) "") (cons #t ""))
  (check-equal? (run '(eof-object? (read-char)) "a") (cons #f ""))
  (check-equal? (run '(begin (write-char #\a) (write-char #\b)) "")
                (cons (void) "ab"))

  (check-equal? (run '(peek-char) "ab") (cons #\a ""))
  (check-equal? (run '(begin (peek-char) (read-char)) "ab") (cons #\a ""))

  ;; Extort examples
  (check-equal? (run '(write-char #t) "") (cons 'err ""))

  ;; Fraud examples
  (check-equal? (run '(let ((x #\a)) (write-char x)) "") (cons (void) "a"))
  (check-equal? (run '(let ((x #\a))
                        (begin (write-char x)
                               x))
                     "")
                (cons #\a "a"))
  (check-equal? (run '(let ((x #\a)) (begin (read-char) x)) "b")
                (cons #\a ""))
  (check-equal? (run '(let ((x #\a)) (begin (peek-char) x)) "b")
                (cons #\a ""))

  ;; Hustle examples
  (check-equal? (run '(let ((x 1))
                        (begin (write-char #\a)
                               1))
                     "")
                (cons 1 "a"))

  (check-equal? (run '(let ((x 1))
                        (let ((y 2))
                          (begin (write-char #\a)
                                 1)))
                     "")
                (cons 1 "a"))

;  (check-equal? (run '(let ((x (cons 1 2)))
;                        (begin (write-char 97)
;                               (car x)))
;                     "")
;                (cons 1 "a"))
  )
