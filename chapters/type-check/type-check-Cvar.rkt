#lang typed/racket

(require "../../utilities.rkt" "type-check-Rvar.rkt")

(provide type-check-Cvar-mixin type-check-Cvar% type-check-Cvar)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Cvar

(: type-check-Cvar-mixin [-> (Class [operator-types [-> (Env Type)]]
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
                                    [type-check-stmt [-> (Env Type) [-> Assign Void]]]
                                    [type-check-tail [-> (Env Type) (Env Block) (Listof (Pair Symbol Tail)) [-> Tail Type]]]
                                    [type-check-program (case-> [-> CProgram CProgram]
                                                                [-> Program Program])])])
(define type-check-Cvar-mixin
  (λ (super%)
    (class super%
      (super-new)
      (inherit type-check-exp type-equal? check-type-equal?)

      (: type-check-stmt [-> (Env Type) [-> Assign Void]])
      (define/public ((type-check-stmt env) s)
        (match s
          [(Assign (Var x) e)
           (define-values (e^ t) ((type-check-exp env) e))
           (cond [(env-has-key? env x)
                  (check-type-equal? t (env-ref env x) s)]
                 [else (env-set! env x t)])]))

      (: type-check-tail [-> (Env Type) (Env Block) (Listof (Pair Symbol Tail)) [-> Tail Type]])
      (define/public ((type-check-tail env block-env G) t)
        (match t
          [(Return e)
           (define-values (e^ t) ((type-check-exp env) e))
           t]
          [(Seq s t)
           ((type-check-stmt env) s)
           ((type-check-tail env block-env G) t)]))

      (: type-check-program (case-> [-> CProgram CProgram]
                                    [-> Program Program]))
      (define/override (type-check-program p)
        (if (CProgram? p)
            (match p
              [(CProgram info G)
               (define env ((inst empty-env Type)))
               (define block-env ((inst empty-env Block)))
               (define t ((type-check-tail env block-env G)
                          (let ([k-v (assoc 'start G)])
                            (if (false? k-v)
                                (error "the c program doesn't contain start block: " p)
                                (cdr k-v)))))
               (unless (type-equal? t 'Integer)
                 (error "return type of program must be Integer, not" t))

               (: locals-types (Listof (Pair Symbol Type)))
               (define locals-types (for/list ([(x t) (in-env env)]) (cons x t)))

               (: new-info Info)
               (define new-info (cons (ann (cons 'locals-types locals-types)
                                           (Pair Symbol Any))
                                      info))
               (CProgram new-info G)])
            (super type-check-program p))))))


(: type-check-Cvar% (Class [operator-types [-> (Env Type)]]
                           [type-equal? [-> Type Type Boolean]]
                           [check-type-equal? [-> Type Type AST Void]]
                           [type-check-op [-> Symbol (Listof Type) Exp Type]]
                           [type-check-exp [-> (Env Type) [-> Exp (Values Exp Type)]]]
                           [type-check-stmt [-> (Env Type) [-> Assign Void]]]
                           [type-check-tail [-> (Env Type) (Env Block) (Listof (Pair Symbol Tail)) [-> Tail Type]]]
                           [type-check-program (case-> [-> CProgram CProgram]
                                                       [-> Program Program])]))
(define type-check-Cvar% (type-check-Cvar-mixin type-check-Rvar%))


(: type-check-Cvar [-> CProgram CProgram])
(define (type-check-Cvar p)
  (send (new type-check-Cvar%) type-check-program p))
