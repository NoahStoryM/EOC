#lang typed/racket

(require "../utilities.rkt" "type-check-Rint.rkt")

(provide type-check-Rvar-mixin type-check-Rvar% type-check-Rvar)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Variables                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Rvar

(: type-check-Rvar-mixin [-> (Class [operator-types [-> (Env Type)]]
                                    [type-equal? [-> Type Type Boolean]]
                                    [check-type-equal? [-> Type Type AST Void]]
                                    [type-check-op [-> Symbol (Listof Type) Exp Type]]
                                    [type-check-exp [-> (Env Type) [-> Exp (Values Exp Type)]]]
                                    [type-check-program [-> Program Program]])
                             (Class [operator-types [-> (Env Type)]]
                                    [type-equal? [-> Type Type Boolean]]
                                    [check-type-equal? [-> Type Type AST Void]]
                                    [type-check-op [-> Symbol (Listof Type) Exp Type]]
                                    [type-check-exp [-> (Env Type) [-> Exp (Values Exp Type)]]]
                                    [type-check-program [-> Program Program]])])
(define type-check-Rvar-mixin
  (λ (super%)
    (class super%
      (super-new)
      (inherit operator-types type-check-op type-check-program type-equal? check-type-equal?)

      (: type-check-exp [-> (Env Type) [-> Exp (Values Exp Type)]])
      (define/override (type-check-exp env)
        (lambda (e)
          (debug 'type-check-exp "Rvar ~a" e)
          (match e
            [(Var x) (values (Var x) (env-ref env x))]
            [_ ((super type-check-exp env) e)]))))))


(: type-check-Rvar% (Class [operator-types [-> (Env Type)]]
                           [type-equal? [-> Type Type Boolean]]
                           [check-type-equal? [-> Type Type AST Void]]
                           [type-check-op [-> Symbol (Listof Type) Exp Type]]
                           [type-check-exp [-> (Env Type) [-> Exp (Values Exp Type)]]]
                           [type-check-program [-> Program Program]]))
(define type-check-Rvar% (type-check-Rvar-mixin type-check-Rint%))


(: type-check-Rvar [-> Program Program])
(define (type-check-Rvar p)
  (send (new type-check-Rvar%) type-check-program p))
