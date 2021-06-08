#lang typed/racket

(require "../utilities.rkt")
(require racket/fixnum)

(provide type-check-Rint-mixin type-check-Rint% type-check-Rint)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Integers                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Rint

(: type-check-Rint-mixin [-> (Class) (Class [operator-types [-> (Env Type)]]
                                            [type-equal? [-> Type Type Boolean]]
                                            [check-type-equal? [-> Type Type AST Void]]
                                            [type-check-op [-> Symbol (Listof Type) Exp Type]]
                                            [type-check-exp [-> (Env Type) [-> Exp (Values Exp Type)]]]
                                            [type-check-program [-> Program Program]])])
(define type-check-Rint-mixin
  (λ (super%)
    (class super%
      (super-new)

      (: operator-types [-> (Env Type)])
      (define/public (operator-types)
        (env-set* ((inst empty-env Type))
                  '+ '((Integer Integer) . Integer)
                  '- '((Integer) . Integer)
                  'read '(() . Integer)))

      (: type-equal? [-> Type Type Boolean])
      (define/public (type-equal? t1 t2) (equal? t1 t2))

      (: check-type-equal? [-> Type Type AST Void])
      (define/public (check-type-equal? t1 t2 e)
        (unless (type-equal? t1 t2)
          (error 'type-check "~a != ~a\nin ~v" t1 t2 e)))

      (: type-check-op [-> Symbol (Listof Type) Exp Type])
      (define/public (type-check-op op arg-types e)
        (match (env-ref (operator-types) op)
          [`((,(? type? #{param-types : (Listof Type)}) ...) . ,(? type? return-type))
           (for ([at arg-types] [pt param-types])
             (check-type-equal? at pt e))
           return-type]
          [else (error 'type-check-op "unrecognized ~a" op)]))

      (: type-check-exp [-> (Env Type) [-> Exp (Values Exp Type)]])
      (define/public (type-check-exp env)
        (lambda (e)
          (debug 'type-check-exp "Rint ~a" e)
          (match e
            [(Int n)  (values (Int n) 'Integer)]
            [(Let x e body)
             (define-values (e^ Te) ((type-check-exp env) e))
             (define-values (b Tb) ((type-check-exp (env-set env x Te)) body))
             (values (Let x e^ b) Tb)]
            [(Prim op es)
             (define-values (new-es ts)
               (for/lists ([exprs : (Listof Exp)] [types : (Listof Type)])
                          ([e : Exp es])
                 ((type-check-exp env) e)))
             (values (Prim op new-es) (type-check-op op ts e))]
            [else (error 'type-check-exp "couldn't match ~a" e)])))

      (: type-check-program [-> Program Program])
      (define/public (type-check-program e)
        (match e
          [(Program info body)
           (define-values (body^ Tb) ((type-check-exp ((inst empty-env Type))) body))
           (check-type-equal? Tb 'Integer body)
           (Program info body^)]
          [_ (error 'type-check-Rint "couldn't match ~a" e)])))))


(: type-check-Rint% (Class [operator-types [-> (Env Type)]]
                           [type-equal? [-> Type Type Boolean]]
                           [check-type-equal? [-> Type Type AST Void]]
                           [type-check-op [-> Symbol (Listof Type) Exp Type]]
                           [type-check-exp [-> (Env Type) [-> Exp (Values Exp Type)]]]
                           [type-check-program [-> Program Program]]))
(define type-check-Rint% (type-check-Rint-mixin object%))


(: type-check-Rint [-> Program Program])
(define (type-check-Rint p)
  (send (new type-check-Rint%) type-check-program p))
