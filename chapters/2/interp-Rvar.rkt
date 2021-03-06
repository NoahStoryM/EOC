#lang typed/racket

(require racket/fixnum)
(require "../utilities.rkt" "../1/interp-Rint.rkt")
(provide interp-Rvar interp-Rvar%)

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code.


(: interp-Rvar% (Class [interp-exp [-> (Env Fixnum) [-> Exp Fixnum]]]
                       [interp-program [-> Program FinalAnswer]]))
(define interp-Rvar%
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
        [_ ((super interp-exp env) e)]))

    ))


(: interp-Rvar [-> Program FinalAnswer])
(define (interp-Rvar p)
  (send (new interp-Rvar%) interp-program p))