#lang nanopass

(provide Rint parse-Rint unparse-Rint
         Rvar parse-Rvar unparse-Rvar)


;; Rint
(define int? (procedure-rename fixnum? 'int?))
(define prim?
  (λ (arg)
    (case arg
      [(+ - read) #t]
      [else #f])))

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
