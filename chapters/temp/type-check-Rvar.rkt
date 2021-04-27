#lang racket

(require "../utilities.rkt" "../untyped-utilities.rkt" "type-check-Rint.rkt")

(provide type-check-Rvar-mixin type-check-Rvar% type-check-Rvar)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Variables                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Rvar

(define type-check-Rvar-mixin
  (λ (super%)
    (class super%
      (super-new)
      (inherit operator-types type-check-op type-check-program type-equal? check-type-equal?)

      (define/override (type-check-exp env)
        (lambda (e)
          (debug 'type-check-exp "Rvar ~a" e)
          (match e
            [(Var x)  (values (Var x) (env-ref env x))]
            [_ ((super type-check-exp env) e)]))))))


(define type-check-Rvar% (type-check-Rvar-mixin object%))


(define (type-check-Rvar p)
  (send (new type-check-Rvar%) type-check-program p))
