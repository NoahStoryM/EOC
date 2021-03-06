#lang typed/racket

(require racket/fixnum)
(require "../utilities.rkt" "interp-Rvar.rkt")
(provide interp-Cvar interp-Cvar-mixin)


(define-type FinalAnswer Fixnum)

(: interp-Cvar-mixin [-> (Class [interp-exp [-> (Env Fixnum) [-> Exp Fixnum]]]
                                [interp-program [-> Program FinalAnswer]])
                         (Class [interp-stmt [-> (Env Fixnum) [-> Assign (Env Fixnum)]]]
                                [interp-tail [-> (Env Fixnum) [-> Tail Fixnum]]]

                                [interp-exp [-> (Env Fixnum) [-> Exp Fixnum]]]
                                [interp-program [-> Program FinalAnswer]])])
(define interp-Cvar-mixin
  (λ (super%)
    (class super%
      (super-new)
      (inherit interp-exp)

      (: interp-stmt [-> (Env Fixnum) [-> Assign (Env Fixnum)]])
      (define/public (interp-stmt env)
        (lambda (s)
          (match s
            [(Assign (Var x) e)
             (env-set env x ((interp-exp env) e))])))

      (: interp-tail [-> (Env Fixnum) [-> Tail Fixnum]])
      (define/public (interp-tail env)
        (lambda (t)
          (match t
            [(Return e)
             ((interp-exp env) e)]
            [(Seq s t2)
             (: new-env (Env Fixnum))
             (define new-env ((interp-stmt env) s))
             ((interp-tail new-env) t2)])))

      (: interp-program [-> Program FinalAnswer])
      (define/override (interp-program p)
        (match p
          [(CProgram _ `((start . ,t1) (,ls . ,ts) ...))
           ((interp-tail '()) t1)])))))


(: interp-Cvar [-> Program FinalAnswer])
(define (interp-Cvar p)
  (send (new (interp-Cvar-mixin interp-Rvar%)) interp-program p))
