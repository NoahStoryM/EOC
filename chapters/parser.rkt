#lang typed/racket

(require "utilities.rkt")

(provide S-Exp s-exp? trans parse untrans unparse)


;; (define-type Literal (U Boolean Real Symbol Char String))
(define-type Literal Fixnum)

(define-type S-List (Listof S-Exp))
(define-type S-Exp (U Symbol Literal S-List))

(define-predicate s-exp?  S-Exp)
(define-predicate s-list? S-List)


(: trans [-> S-Exp Exp])
(define trans
  (λ (code)
    (match code
      [(? fixnum? x) (Int x)]
      [(? symbol? v) (Var v)]
      ['(read) (Prim 'read '())]
      [`(- ,(? s-exp? e)) (Prim '- (list (trans e)))]
      [`(+ ,(? s-exp? e1) ,(? s-exp? e2)) (Prim '+ (list (trans e1) (trans e2)))]
      [`(let ([,(? symbol? v) ,(? s-exp? e)]) ,(? s-exp? body))
       (Let v (trans e) (trans body))])))

(: parse [-> S-Exp Program])
(define parse (λ (code) (Program '() (trans code))))


(: untrans [-> Exp S-Exp])
(define untrans
  (λ (exp)
    (define-values (in out) (make-pipe))

    (parameterize ([port-count-lines-enabled #t])
      (write-ast exp out)
      (assert (read in) s-exp?))))

(: unparse [-> Program S-Exp])
(define unparse (λ (prog) (untrans (Program-body prog))))
