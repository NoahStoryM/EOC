#lang typed/racket

(require "utilities.rkt" "type-check/type-check-Cvar.rkt")

(provide type-checker)


(: type-checker (case-> [-> Program Program]
                        [-> CProgram CProgram]
                        [-> Pseudo-X86Program Pseudo-X86Program]
                        [-> X86Program X86Program]))
(define type-checker
  (λ (p)
    (cond [(Program? p) (type-check-Cvar p)]
          [(CProgram? p) (type-check-Cvar p)]
          [else p])))