#lang typed/racket

(require "../parser.rkt"
         "../utilities.rkt"
         "../2/interp-Rvar.rkt"
         "../2/interp-Cvar.rkt"
         "../interp.rkt"
         "../compiler.rkt")


(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))

(: test-init [-> S-Exp Void])
(define test-init
  (λ (code)
    (displayln "\n-----------------------------------")
    (pretty-print code)
    (call-with-values (λ () (eval code eval-ns))
                      (λ args (displayln (car args))))
    (newline)))

(: test-pe-Rvar [-> S-Exp Void])
(define test-pe-Rvar
  (λ (code)
    (define ast
      (pe-Rvar
       (parse code)))

    (test-init code)
    (pretty-print (unparse ast))
    (call-with-values (λ () (eval code eval-ns))
                      (λ args (displayln (car args))))
    (newline)))


(: test-uniquify [-> S-Exp Void])
(define test-uniquify
  (λ (code)
    (define ast
      (uniquify
       (pe-Rvar
        (parse code))))

    (test-pe-Rvar code)
    (pretty-print (unparse ast))
    (displayln (interp-Rvar ast))
    (newline)))

(: test-remove-complex-opera* [-> S-Exp Void])
(define test-remove-complex-opera*
  (λ (code)
    (define ast
      (remove-complex-opera*
       (uniquify
        (pe-Rvar
         (parse code)))))

    (test-uniquify code)
    (pretty-print (unparse ast))
    (displayln (interp-Rvar ast))
    (newline)))

(: test-explicate-control [-> S-Exp Void])
(define test-explicate-control
  (λ (code)
    (define ast
      (explicate-control
       (remove-complex-opera*
        (uniquify
         (pe-Rvar
          (parse code))))))

    (test-remove-complex-opera* code)
    (show-ast ast)
    (displayln (interp-Cvar ast))
    (newline)))

(: test-instruction-selections [-> S-Exp Void])
(define test-instruction-selections
  (λ (code)
    (define ast
      (select-instructions
       (explicate-control
        (remove-complex-opera*
         (uniquify
          (pe-Rvar
           (parse code)))))))

    (test-explicate-control code)
    (show-ast ast)
    (displayln (interp-pseudo-x86-0 ast))
    (newline)))

(: test-assign-homes [-> S-Exp Void])
(define test-assign-homes
  (λ (code)
    (define ast
      (assign-homes
       (select-instructions
        (explicate-control
         (remove-complex-opera*
          (uniquify
           (pe-Rvar
            (parse code))))))))

    (test-instruction-selections code)
    (show-ast ast)
    (displayln (interp-x86-0 ast))
    (newline)))

(: test-patch-instructions [-> S-Exp Void])
(define test-patch-instructions
  (λ (code)
    (define ast
      (patch-instructions
       (assign-homes
        (select-instructions
         (explicate-control
          (remove-complex-opera*
           (uniquify
            (pe-Rvar
             (parse code)))))))))

    (test-assign-homes code)
    (show-ast ast)
    (displayln (interp-x86-0 ast))
    (newline)))

(: test-prepare-x86 [-> S-Exp Void])
(define test-prepare-x86
  (λ (code)
    (define ast
      (prepare-x86
       (patch-instructions
        (assign-homes
         (select-instructions
          (explicate-control
           (remove-complex-opera*
            (uniquify
             (pe-Rvar
              (parse code))))))))))

    (test-assign-homes code)
    (show-ast ast)
    ;; (displayln (interp-x86-0 ast))
    (newline)))


(: test [-> S-Exp Void])
;; (define test test-pe-Rvar)
;; (define test test-uniquify)
;; (define test test-remove-complex-opera*)
;; (define test test-instruction-selections)
;; (define test test-assign-homes)
;; (define test test-patch-instructions)
(define test test-prepare-x86)


(test '24)

(test '(let ([a (+ (+ 19 3) (- 1))])
         (+ (- (+ 4 a))
            7)))

(test '(let ([a 42])
         (let ([b a])
           b)))

(test '(let ([y (let ([x 20])
                  (+ x (let ([x 22]) x)))])
         y))
