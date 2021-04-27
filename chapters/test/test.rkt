#lang typed/racket

(require racket/fixnum)
(require "../parser.rkt" "../compiler.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rint examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following compiler pass is just a silly one that doesn't change
;; anything important, but is nevertheless an example of a pass. It
;; flips the arguments of +. -Jeremy

(displayln (unparse (flip-Rint (parse '(+ (- (read)) x)))))


;; Next we have the partial evaluation pass described in the book.
(displayln (unparse (pe-Rint (parse '(+ (+ 20 (- (read))) (+ 32 10))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; uniquify : R1 -> R1
(displayln (unparse (uniquify (parse '(let ([x 32])
                                        (+ (let ([x 10]) x) x))))))

(displayln (unparse (uniquify (parse '(let ([x (let ([x 4]) (+ x 1))])
                                        (+ x 2))))))


;; remove-complex-opera* : R1 -> R1

;; explicate-control : R1 -> C0

;; select-instructions : C0 -> pseudo-x86

;; assign-homes : pseudo-x86 -> pseudo-x86

;; patch-instructions : psuedo-x86 -> x86

;; print-x86 : x86 -> string
