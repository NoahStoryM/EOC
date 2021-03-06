#lang typed/racket

(require "../utilities.rkt")


(define E1 (Int 42))                                ; 42
(define E2 (Prim 'read '()))                        ; (read)
(define E3 (Prim '- (list E1)))                     ; (- 42)
(define E4 (Prim '+ (list E3 (Int 5))))             ; (+ (- 42) 5)
(define E5 (Prim '+ (list E2 (Prim '- (list E2))))) ; (+ (read) (- (read)))


(: list-max [-> (Listof Positive-Integer) Natural])
(define list-max
  (λ (ls)
    (apply max 0 ls)))

(: height [-> Exp Positive-Integer])
(define height
  (λ (e)
    (match e
      [(Int n) 1]
      [(Prim op e*)
       (add1 (list-max (map height e*)))])))


(displayln (height E1))
(displayln (height E2))
(displayln (height E3))
(displayln (height E4))
(displayln (height E5))

(displayln "--------------------")
(write-astln E1)
(write-astln E2)
(write-astln E3)
(write-astln E4)
(write-astln E5)