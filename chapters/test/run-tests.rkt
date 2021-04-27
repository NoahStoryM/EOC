#lang typed/racket/no-check

(require "../utilities.rkt")
(require "../untyped-utilities.rkt")
(require "../2/interp-Rvar.rkt")
(require "../2/interp-Cvar.rkt")
(require "../compiler.rkt")
;; (debug-level 1)
;; (AST-output-syntax 'concrete-syntax)

;; Define the passes to be used by interp-tests and the grader
;; Note that your compiler file (the file that defines the passes)
;; should be named "compiler.rkt"
(: passes (List (List String [-> Program Program] [-> Program FinalAnswer])))
(define passes
  `(["uniquify" ,uniquify ,interp-Rvar]
    ;; Uncomment the following passes as you finish them.
    ;; ["remove complex opera*" ,remove-complex-opera* ,interp-Rvar]
    ;; ["explicate control" ,explicate-control ,interp-Cvar]
    ;; ["instruction selection" ,select-instructions ,interp-x86-0]
    ;; ["assign homes" ,assign-homes ,interp-x86-0]
    ;; ["patch instructions" ,patch-instructions ,interp-x86-0]
    ;; ["print x86" ,print-x86 #f]
    ))

;; all the files in the tests/ directory with extension ".rkt".
(: all-tests (Listof String))
(define all-tests
  (map (lambda (p) (car (string-split (path->string p) ".")))
       (filter (lambda (p)
                 (string=? (cadr (string-split (path->string p) ".")) "rkt"))
               (directory-list (build-path (current-directory) "tests")))))

(: tests-for [-> String (Listof String)])
(define (tests-for r)
  (map (lambda (p)
         (caddr (string-split p "_")))
       (filter
        (lambda (p)
          (string=? r (car (string-split p "_"))))
        all-tests)))

(interp-tests "var" #f passes interp-Rvar "var_test" (tests-for "var"))

;; Uncomment the following when all the passes are complete to
;; test the final x86 code.
;; (compiler-tests "var" #f passes "var_test" (tests-for "var"))

