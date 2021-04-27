#lang typed/racket

(require racket/fixnum)
(require "utilities.rkt" "1/interp-Rint.rkt")

(provide (all-defined-out))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rint examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following compiler pass is just a silly one that doesn't change
;; anything important, but is nevertheless an example of a pass. It
;; flips the arguments of +. -Jeremy
(: flip-exp [-> Exp Exp])
(define (flip-exp e)
  (match e
    [(Var x) e]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (Prim '- (list (flip-exp e1)))]
    [(Prim '+ (list e1 e2)) (Prim '+ (list (flip-exp e2) (flip-exp e1)))]))

(: flip-Rint [-> Program Program])
(define (flip-Rint e)
  (match e
    [(Program info e) (Program info (flip-exp e))]))


;; Next we have the partial evaluation pass described in the book.
(: pe-neg [-> Exp Exp])
(define (pe-neg r)
  (match r
    [(Int n) (Int (fx- 0 n))]
    [else (Prim '- (list r))]))

(: pe-add [-> Exp Exp Exp])
(define (pe-add r1 r2)
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))

(: pe-exp [-> Exp Exp])
(define (pe-exp e)
  (match e
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e1)) (pe-neg (pe-exp e1))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]))

(: pe-Rint [-> Program Program])
(define (pe-Rint p)
  (match p
    [(Program info e) (Program info (pe-exp e))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: uniquify-exp [-> (Env Symbol) [-> Exp Exp]])
(define (uniquify-exp env)
  (lambda (e)
    (match e
      [(Var x)
       (Var (env-ref env x))]
      [(Int n) (Int n)]
      [(Let x e body)
       (define new-name (gensym x))
       (define new-env (env-set env x new-name))
       (Let new-name ((uniquify-exp env) e)
            ((uniquify-exp new-env) body))]
      [(Prim op es)
       (Prim op (for/list ([e es]) ((uniquify-exp env) e)))])))

;; uniquify : R1 -> R1
(: uniquify [-> Program Program])
(define (uniquify p)
  (match p
    [(Program info e) (Program info ((uniquify-exp ((inst empty-env Symbol))) e))]))

;; ;; remove-complex-opera* : R1 -> R1
;; (: remove-complex-opera* [-> Program Program])
;; (define (remove-complex-opera* p)
;;   (error "TODO: code goes here (remove-complex-opera*)"))

;; ;; explicate-control : R1 -> C0
;; (: explicate-control [-> Program CProgram])
;; (define (explicate-control p)
;;   (error "TODO: code goes here (explicate-control)"))

;; ;; select-instructions : C0 -> pseudo-x86
;; (: select-instructions [-> CProgram Pseudo-X86Program])
;; (define (select-instructions p)
;;   (error "TODO: code goes here (select-instructions)"))

;; ;; assign-homes : pseudo-x86 -> pseudo-x86
;; (: assign-homes [-> Pseudo-X86Program Pseudo-X86Program])
;; (define (assign-homes p)
;;   (error "TODO: code goes here (assign-homes)"))

;; ;; patch-instructions : psuedo-x86 -> x86
;; (: patch-instructions [-> Pseudo-X86Program X86Program])
;; (define (patch-instructions p)
;;   (error "TODO: code goes here (patch-instructions)"))

;; ;; print-x86 : x86 -> string
;; (: print-x86 [-> X86Program String])
;; (define (print-x86 p)
;;   (error "TODO: code goes here (print-x86)"))
