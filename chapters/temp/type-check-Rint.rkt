#lang racket

(require "../utilities.rkt" "../untyped-utilities.rkt")

(provide type-check-Rint-mixin type-check-Rint% type-check-Rint)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Integers                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; type-check-Rint

(define type-check-Rint-mixin
  (λ (super%)
    (class super%
      (super-new)

      (define/public (operator-types)
        (env-set* (empty-env)
                  '+ '((Integer Integer) . Integer)
                  '- '((Integer) . Integer)
                  'read '(() . Integer)))

      (define/public (type-equal? t1 t2) (equal? t1 t2))

      (define/public (check-type-equal? t1 t2 e)
        (unless (type-equal? t1 t2)
          (error 'type-check "~a != ~a\nin ~v" t1 t2 e)))

      (define/public (type-check-op op arg-types e)
        (match (env-ref (operator-types) op)
          [`((,param-types ...) . ,return-type)
           (for ([at arg-types] [pt param-types])
             (check-type-equal? at pt e))
           return-type]
          [else (error 'type-check-op "unrecognized ~a" op)]))

      (define/public (type-check-exp env)
        (lambda (e)
          (debug 'type-check-exp "Rint ~a" e)
          (match e
            [(Int n)  (values (Int n) 'Integer)]
            [(Let x e body)
             (define-values (e^ Te) ((type-check-exp env) e))
             (define-values (b Tb) ((type-check-exp (dict-set env x Te)) body))
             (values (Let x e^ b) Tb)]
            [(Prim op es)
             (define-values (new-es ts)
               (for/lists (exprs types) ([e es]) ((type-check-exp env) e)))
             (values (Prim op new-es) (type-check-op op ts e))]
            [else (error 'type-check-exp "couldn't match ~a" e)])))

      (define/public (type-check-program e)
        (match e
          [(Program info body)
           (define-values (body^ Tb) ((type-check-exp '()) body))
           (check-type-equal? Tb 'Integer body)
           (Program info body^)]
          [else (error 'type-check-Rint "couldn't match ~a" e)])))))


(define type-check-Rint% (type-check-Rint-mixin object%))

(define (type-check-Rint p)
  (send (new type-check-Rint%) type-check-program p))
