#lang typed/racket

(require racket/fixnum)
(require "../utilities.rkt")

(provide interp-Rint-mixin interp-Rint% interp-Rint)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreter for Rint: integer arithmetic

;; Note to maintainers of this code:
;;   A copy of this interpreter is in the book and should be
;;   kept in sync with this code. This code does not use
;;   the match 'app' feature because the book doesn't introduce
;;   that until a later.

;; (: interp-exp [-> Exp Fixnum])
;; (define (interp-exp e)
;;   (match e
;;     [(Int n) n]
;;     [(Prim 'read '())
;;      (define r (read))
;;      (if (fixnum? r)
;;          r
;;          (error 'interp-exp "expected an integer" r))]
;;     [(Prim '- (list e))
;;      (define v (interp-exp e))
;;      (fx- 0 v)]
;;     [(Prim '+ (list e1 e2))
;;      (define v1 (interp-exp e1))
;;      (define v2 (interp-exp e2))
;;      (fx+ v1 v2)]
;;     ))

;; (: interp-Rint [-> Program Fixnum])
;; (define (interp-Rint p)
;;   (match p
;;     [(Program '() e) (interp-exp e)]
;;     ))


(: interp-Rint-mixin [-> (Class)
                         (Class [interp-exp [-> (Env Fixnum) [-> Exp Fixnum]]]
                                [interp-program [-> Program FinalAnswer]])])
(define interp-Rint-mixin
  (λ (super%)
    (class super%
      (super-new)

      (: interp-exp [-> (Env Fixnum) [-> Exp Fixnum]])
      (define/public ((interp-exp env) e)
        (match e
          [(Int n) n]
          [(Prim 'read '()) (read-fixnum)]
          [(Prim '- (list e))
           (define v ((interp-exp env) e))
           (fx- 0 v)]
          [(Prim '+ (list e1 e2))
           (define v1 ((interp-exp env) e1))
           (define v2 ((interp-exp env) e2))
           (fx+ v1 v2)]))

      (: interp-program [-> Program FinalAnswer])
      (define/public (interp-program p)
        (match p
          [(Program info e) ((interp-exp (empty-env)) e)])))))


(: interp-Rint% (Class [interp-exp [-> (Env Fixnum) [-> Exp Fixnum]]]
                       [interp-program [-> Program FinalAnswer]]))
(define interp-Rint% (interp-Rint-mixin object%))


(: interp-Rint [-> Program FinalAnswer])
(define (interp-Rint p)
  (send (new interp-Rint%) interp-program p))
