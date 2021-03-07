#lang nanopass

(require "concrete-syntax.rkt")


;; Rint
(parse-Rint '123)
(parse-Rint '(read))
(parse-Rint '(+ 21 (- (read))))


;; Rvar
(parse-Rvar 'v)
(parse-Rvar '(let ([v (+ 123)])
               (+ v (- 32))))
