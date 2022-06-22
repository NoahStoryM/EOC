#lang nanopass

(require (rename-in racket/base
                    [list?   info?]
                    [fixnum? int?]))

(provide Rint parse-Rint unparse-Rint
         Rvar parse-Rvar unparse-Rvar)


;; Rint
(define prim? (λ (arg) (memq arg '(+ - read))))

(define-language Rint
  (entry Exp)
  (terminals
   (int (n))
   (prim (pr)))
  (Exp (e body)
       n
       pr
       (e0 e* ...)))
(define-parser parse-Rint Rint)


;; Rvar
(define var? (conjoin symbol? (negate prim?)))

(define-language Rvar
  (extends Rint)
  (terminals
   (+ (var (v))))
  (Exp (e body)
       (+ v
          (let ([v* e*] ...) body* ... body))))
(define-parser parse-Rvar Rvar)
