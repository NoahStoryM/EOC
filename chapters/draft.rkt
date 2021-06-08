#lang typed/racket

(require "../parser.rkt"
         "../utilities.rkt"
         "../2/interp-Rvar.rkt"
         "../2/interp-Cvar.rkt"
         "../type-check.rkt"
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

(: test-uniquify [-> S-Exp Void])
(define test-uniquify
  (λ (code)
    (define ast
      (type-checker
       (uniquify
        (type-checker
         (parse code)))))

    (test-init code)
    (pretty-print (unparse ast))
    (displayln (interp-Rvar ast))
    (newline)))

(: test-remove-complex-opera* [-> S-Exp Void])
(define test-remove-complex-opera*
  (λ (code)
    (define ast
      (type-checker
       (remove-complex-opera*
        (type-checker
         (uniquify
          (type-checker
           (parse code)))))))

    (test-uniquify code)
    (pretty-print (unparse ast))
    (displayln (interp-Rvar ast))
    (newline)))

(: test-explicate-control [-> S-Exp Void])
(define test-explicate-control
  (λ (code)
    (define ast
      (type-checker
       (explicate-control
        (type-checker
         (remove-complex-opera*
          (type-checker
           (uniquify
            (type-checker
             (parse code)))))))))

    (test-remove-complex-opera* code)
    (show-ast ast)
    (displayln (interp-Cvar ast))
    (newline)))

(: test-instruction-selection [-> S-Exp Void])
(define test-instruction-selection
  (λ (code)
    (define ast
      (type-checker
       (select-instructions
        (type-checker
         (explicate-control
          (type-checker
           (remove-complex-opera*
            (type-checker
             (uniquify
              (type-checker
               (parse code)))))))))))

    (test-explicate-control code)
    (show-ast ast)
    (displayln (interp-pseudo-x86-0 ast))
    (newline)))


(: test [-> S-Exp Void])
(define test test-instruction-selection)


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
