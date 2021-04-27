#lang typed/racket

(require racket/fixnum)
(require "../utilities.rkt" "../1/interp-Rint.rkt")

(provide interp-Rvar-mixin interp-Rvar% interp-Rvar)


(: interp-Rvar-mixin [-> (Class [interp-exp [-> (Env Fixnum) [-> Exp Fixnum]]]
                                [interp-program [-> Program FinalAnswer]])
                         (Class [interp-exp [-> (Env Fixnum) [-> Exp Fixnum]]]
                                [interp-program [-> Program FinalAnswer]])])
(define interp-Rvar-mixin
  (λ (super%)
    (class interp-Rint%
      (super-new)
      (inherit interp-program)

      (: interp-exp [-> (Env Fixnum) [-> Exp Fixnum]])
      (define/override ((interp-exp env) e)
        (match e
          [(Var x) (env-ref env x)]
          [(Let x e body)
           (define new-env (env-set env x ((interp-exp env) e)))
           ((interp-exp new-env) body)]
          [_ ((super interp-exp env) e)])))))


(: interp-Rvar% (Class [interp-exp [-> (Env Fixnum) [-> Exp Fixnum]]]
                       [interp-program [-> Program FinalAnswer]]))
(define interp-Rvar% (interp-Rvar-mixin interp-Rint%))


(: interp-Rvar [-> Program FinalAnswer])
(define (interp-Rvar p)
  (send (new interp-Rvar%) interp-program p))
