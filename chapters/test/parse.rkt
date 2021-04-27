#lang typed/racket

(require "../utilities.rkt" "../parser.rkt")


(: test [-> S-Exp Boolean])
(define test (λ (code) (equal? code (unparse (parse code)))))


(test '(let ([x 32])
         (+ (let ([x 10]) x) x)))

(test '(let ([x (let ([x 4]) (+ x 1))])
          (+ x 2)))
