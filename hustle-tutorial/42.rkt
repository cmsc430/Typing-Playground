#lang racket
(let ((x (box 42))) (let ((y x)) (unbox y)))

