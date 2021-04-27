;;
;;   WARNING!!!!
;;
;;   The original version of this file lives in the course-compiler repository.
;;   There is a copy of this file in the public-student-support-code
;;   repository.
;;
;;   DO NOT EDIT the version of this file in public-student-support-code
;;   because any changes you make will be OBLITERATED the next time
;;   someone edits the version in the course-compiler repository
;;   and then copies it over to public-student-support-code.

#lang racket
(require racket/struct)
(require graph)

;; Version 0.2
;; ---------------------
#|

This file is updated periodically based on the internal, reference
copy of the P423/523 compiler.

Changelog:

0.1: initial release, used 2016-2017.

0.2: update during Fall'17 semester.
We removed these functions:
* interp-test-suite
* compiler-test-suite
We added:
* parallel test running
* various other changes

0.3: updated during Fall'18 semester.
We removed multithreading
We added rackunit integration

0.4: update during Fall'20 semester.
Added structs for AST nodes.

|#

(require racket/pretty racket/match)
(require (for-syntax racket))
; (require racket/async-channel)
(require rackunit rackunit/text-ui rackunit/gui)

(require (except-in "utilities.rkt"
                    debug-level at-debug-level? debug
                    ))


(provide interp-tests
         read-fixnum read-program
         parse-program
         )


;; debug state is a nonnegative integer.
;; The easiest way to increment it is passing the -d option
;; to run-tests.rkt
;; 0 none
;; 1 trace passes in run-test
;; 2 debug macros
;; 3 verbose debugging
;; 4 (copious) absolutely everything
;; The higher the setting the more information is reported.
;; If you want the same functionality as previous incarnation
;; of utilities then uncomment the line after this definition
;; and change the number there.
(define debug-level
  (make-parameter
   0
   (lambda (d)
     (unless (exact-nonnegative-integer? d)
       (error 'debug-state "expected nonnegative-integer in ~a" d))
     d)))
;; (debug-level 2)

;; Check to see if debug state is at least some level
(define (at-debug-level? n)
  (unless (exact-nonnegative-integer? n)
    (error 'at-debug-level? "expected non-negative integer ~a" n))
  (>= (debug-level) n))

(define (test-verbosity)
  (cond [(>= (debug-level) 1) 'verbose]
        [else 'normal]))

;; print-label-and-values prints out the label followed the values
;; and the expression that generated those values
;; The label is formated with the file and line number of the
;; debubing expression for easier location.
(define-syntax (print-label-and-values stx)
  (syntax-case stx ()
    [(_ label value ...)
     (let* ([src (syntax-source stx)]
            [src (if (path? src)
                     (find-relative-path (current-directory) src)
                     src)]
            [lno (syntax-line stx)])
       #`(begin
           (printf "~a @ ~a:~a\n" label #,src #,lno)
        (begin
          (printf "~a:\n" 'value)
          (pretty-print value)
          #;(if (string? value)
              (display value)
              (pretty-display value))
          (newline))
        ...
        (newline)))]))

;; This series of macros are used for debuging purposes
;; and print out
(define-syntax-rule (define-debug-level name level)
    (...
     (define-syntax (name stx)
      (syntax-case stx ()
        [(_ label value ...)
         #`(when (at-debug-level? level)
             #,(syntax/loc stx
                 (print-label-and-values label value ...)))]))))

;; Print out debugging info in a somewhat organized manner
;; (debug "foo" (car '(1 2)) 'foo) should print
;; foo @ utilities.rkt:77
;; (car '(1 2)):
;; 1
;; 'foo:
;; foo
(define-debug-level trace 1)
(define-debug-level debug 2)
(define-debug-level verbose 3)
(define-debug-level copious 4)


(define (read-fixnum)
  (define r (read))
  (cond [(fixnum? r) r]
        [else (error 'read "expected an integer")]))

;; Read an entire .rkt file wrapping the s-expressions in
;; a list whose head is 'program.
(define (read-program path)
  (unless (or (string? path) (path? path))
    (error 'read-program "expected a string in ~s" path))
  (unless (file-exists? path)
    (error 'read-program "file doesn't exist in ~s" path))
  (debug "utilities/read-program" path)
  (define input-prog
    (call-with-input-file path
      (lambda (f)
        `(program () ,@(for/list ([e (in-port read f)]) e)))))
  (define parsed-prog (parse-program input-prog))
  (debug "utilities/read-program" parsed-prog)
  parsed-prog)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing S-expressions into Abstract Syntax Trees (and back)


(define src-primitives
  '(read + - eq? < <= > >= and or not
         vector vector-ref vector-set! vector-length
         procedure-arity
         boolean? integer? vector? procedure? void?
         any-vector-ref any-vector-set! any-vector-length))

(define (parse-exp e)
  (match e
    [(? fixnum? x) (Int x)]
    [(? symbol? v) (Var v)]
    ['(read) (Prim 'read '())]
    [`(- ,e) (Prim '- (list (parse-exp e)))]
    [`(+ ,e1 ,e2) (Prim '+ (list (parse-exp e1) (parse-exp e2)))]
    [`(let ([,(? symbol? v) ,rhs]) ,body)
     (Let v (parse-exp rhs) (parse-exp body))]))

;; (define (parse-def d)
;;   (match d
;;     [`(define (,f ,ps ...) : ,rty ,body)
;;      (Def f ps rty '() (parse-exp body))]
;;     [`(define (,f ,xs ...) ,body) ;; dynamically typed definition
;;      (Def f xs 'Any '() (parse-exp body))]
;;     [`(: ,name ,type)
;;      (Decl name type)]
;;     ))

(define (parse-program p)
  (match p
    [`(program ,info ,body)
     (Program info (parse-exp body))]
    ;; [`(program ,info ,def* ... ,body)
    ;;  (ProgramDefsExp info
    ;;               (for/list ([d def*]) (parse-def d))
    ;;               (parse-exp body))]
    ))

(define (unparse-exp e)
  (match e
    [(Var x) x]
    [(Int n) n]
    ;; [(Bool b) b]
    ;; [(Void) '(void)]
    [(Let x rhs body)
     `(let ([,x ,(unparse-exp rhs)]) ,(unparse-exp body))]
    ;; [(Lambda ps rt body)
    ;;  `(lambda: ,ps ,rt ,(unparse-exp body))]
    [(Prim op es)
     `(,op ,@(map unparse-exp es))]
    ;; [(Apply e es)
    ;;  `(,(unparse-exp e) ,@(map unparse-exp es))]
    ))


;; The check-passes function takes a compiler name (a string), a
;; typechecker (see below), a description of the passes (see below),
;; and an initial interpreter to apply to the initial expression, and
;; returns a function that takes a test name and runs the passes and
;; the appropriate interpreters to test the correctness of all the
;; passes. This function assumes there is a "tests" subdirectory and a
;; file in that directory whose name is the test name followed by
;; ".rkt". Also, there should be a matching file with the ending ".in"
;; that provides the input for the Scheme program. If any program
;; should not pass typechecking, then there is a file with the name
;; number (whose contents are ignored) that ends in ".tyerr".
;;
;; The description of the passes is a list with one entry per pass.
;; An entry is a list with three things: a string giving the name of
;; the pass, the function that implements the pass (a translator from
;; AST to AST), and a function that implements the interpreter (a
;; function from AST to result value).
;;
;; The typechecker is a function of exactly one argument that EITHER
;; raises an error using the (error) function when it encounters a
;; type error, or returns #f when it encounters a type error.

(define (strip-has-type e)
  e)

#;(define (strip-has-type e)
  (match e
    [`(has-type ,e ,T)
     (strip-has-type e)]
    [`(,(app strip-has-type e*) ...)
      `(,@e*)]
    [else
     e]))

(define ((check-exception name test-name error-expected) fn)
  (with-handlers
    ([exn:fail?
      (lambda (exn)
        (cond [error-expected 'expected-error]
              [else
               (displayln (format "encountered exception while testing `~a`, case ~a" name test-name))
               (raise exn)]))])
    (let ([res (fn)])
      (when (and (not (string? res)) (not (pair? res)) (not (eq? res #f)))
        (check-false error-expected (format "in check-exception, expected exception, not ~a" res)))
      res)))

(define ((check-passes-suite name typechecker passes initial-interp) test-name)
  (test-suite
   test-name
   (let* ([input-file-name (format "tests/~a.in" test-name)]
          [result-file-name (format "tests/~a.res" test-name)]
          [program-name (format "tests/~a.rkt" test-name)]
          [sexp (read-program program-name)]
          [type-error-expected (file-exists? (format "tests/~a.tyerr" test-name))]
          [tsexp ((check-exception name test-name type-error-expected)
                  (thunk (test-typecheck typechecker sexp)))]
          [error-expected (file-exists? (format "tests/~a.err" test-name))]
          [checker (check-exception name test-name error-expected)])
     (test-case
       "typecheck"
       (if type-error-expected
           (check-false
            tsexp
            (format "expected type error in compiler '~a', case ~a, but no error raised by typechecker" name test-name))
           (check-not-false
            tsexp
            (format "expected no type error in compiler '~a', case ~a, but received error ~a" name test-name tsexp))))
     (trace "type checker output:" (strip-has-type tsexp))
     (unless type-error-expected
       (make-test-suite
        "passes"
        (let ([expected-result (cond [initial-interp
                                      (if (file-exists? input-file-name)
                                          (with-input-from-file input-file-name
                                            (lambda () (checker (thunk (initial-interp tsexp)))))
                                          (checker (thunk (initial-interp tsexp))))]
                                     [else
                                      (if (file-exists? result-file-name)
                                          (call-with-input-file result-file-name
                                            (lambda (f) (string->number (read-line f))))
                                          42)])])
        (let loop ([passes passes]
                   [p tsexp]
                   [tests '()])
          (trace "testing" test-name expected-result)
          (cond [(null? passes) (reverse tests)]
                [else
                 (define pass-info (car passes))
                 (define pass-name (list-ref pass-info 0))
                 (define pass      (list-ref pass-info 1))
                 (define interp    (list-ref pass-info 2))
                 (define type-checker
                   (cond [(>= (length pass-info) 4)
                          (list-ref pass-info 3)]
                         [else #f]))
                 (trace (string-append "running pass: " pass-name))
                 (define input p)
                 (define new-p^ ((check-exception name test-name #f) (thunk (pass p))))
                 (trace "pass output: " (strip-has-type new-p^))
                 (define new-p (cond [type-checker
                                      (trace "type checking...")
                                      (type-checker new-p^)]
                                     [else new-p^]))
                 (trace "type-check output: " (strip-has-type new-p))
                 (cond [interp
                        (define result
                          (if (file-exists? input-file-name)
                              (with-input-from-file input-file-name
                                (lambda () (checker (thunk (interp new-p)))))
                              (checker (thunk (interp new-p)))))
                        (trace "output: " result)
                        (cond [expected-result
                               (loop (cdr passes) new-p
                                     (cons (test-suite
                                            (string-append "pass " pass-name)
                                            (check-equal?
                                             result expected-result
                                             (format "differing results in compiler '~a' on test '~a' pass '~a', expected ~a, not ~a" name test-name pass-name expected-result result)))
                                           tests))]
                              [else
                               (loop (cdr passes) new-p tests)]
                              );; cond result
                          ]
                       [else
                        (loop (cdr passes) new-p tests)]
                       ) ;; cond interp
                 ]
                ))))))))

(define (compile passes)
  (let ([prog-file-name (vector-ref (current-command-line-arguments) 0)])
    ((compile-file passes) prog-file-name)))

;; The compile-file function takes a typechecker and a description of
;; the compiler passes (see the comment for check-passes) and returns
;; a function that, given a program file name (a string ending in
;; ".rkt") that passes typechecking, applies all of the passes and
;; writes the output to a file whose name is the same as the proram
;; file name but with ".rkt" replaced with ".s". The function then
;; returns #t. If the program does not typecheck, the returned
;; function will return #f.
(define (compile-file typechecker passes)
  (lambda (prog-file-name)
    (define file-base (string-trim prog-file-name ".rkt"))
    (define out-file-name (string-append file-base ".s"))
    (trace "testing" prog-file-name)
    (call-with-output-file
      out-file-name
      #:exists 'replace
      (lambda (out-file)
        (define sexp (read-program prog-file-name))
        (define tsexp (test-typecheck typechecker sexp))
        (trace "compile-file: output of type check" tsexp)
        (if tsexp
            (let ([x86 (let loop ([passes passes] [p tsexp])
                         (cond [(null? passes) p]
                               [else
                                (define pass-info (car passes))
                                (define name      (list-ref pass-info 0))
                                (define pass      (list-ref pass-info 1))
                                (define interp    (list-ref pass-info 2))
                                (define type-checker
                                  (cond [(>= (length pass-info) 4)
                                         (list-ref pass-info 3)]
                                        [else #f]))
                                (trace (string-append "compiling, running pass: " name))
                                (define new-p^
                                  ((check-exception name file-base #f)
                                   (thunk (pass p))))
                                (trace (string-append name " output: ") (strip-has-type new-p^))
                                (define new-p (cond [type-checker
                                                     (trace "type checking...")
                                                     (type-checker new-p^)]
                                                    [else new-p^]))
                                (loop (cdr passes) new-p)
                                ]))])
              (cond [(string? x86)
                     (write-string x86 out-file)
                     (newline out-file)
                     (flush-output out-file)
                     #t]
                    [else
                     (error "compiler did not produce x86 output")])
              )
            #f)
        ))))



;; The interp-tests function takes a compiler name (a string), a
;; typechecker (see the comment for check-passes) a description of the
;; passes (ditto) a test family name (a string), and a list of test
;; numbers, and runs the compiler passes and the interpreters to check
;; whether the passes correct.
;;
;; This function assumes that the subdirectory "tests" has a bunch of
;; Scheme programs whose names all start with the family name,
;; followed by an underscore and then the test number, ending in
;; ".rkt". Also, for each Scheme program there is a file with the same
;; number except that it ends with ".in" that provides the input for
;; the Scheme program. If any program should not pass typechecking,
;; then there is a file with the name number (whose contents are
;; ignored) that ends in ".tyerr".

;; (define (interp-tests name typechecker passes initial-interp test-family test-nums)
;;   (define checker (check-passes name typechecker passes initial-interp))
;;   (for ([test-number (in-list test-nums)])
;;     (let ([test-name (format "~a_~a" test-family test-number)])
;;       (debug "utilities/interp-test" test-name)
;;       (checker test-name))))

(define (interp-tests name typechecker passes initial-interp test-family test-nums)
  (run-tests (interp-tests-suite name typechecker passes initial-interp test-family test-nums) (test-verbosity)))

(define (interp-tests-suite name typechecker passes initial-interp test-family test-nums)
  (define checker-suite (check-passes-suite name typechecker passes initial-interp))
  (make-test-suite
   "interpreter tests"
   (for/list ([test-number (in-list test-nums)])
     (let ([test-name (format "~a_~a" test-family test-number)])
       (checker-suite test-name)))))


;; POLICY: we could use raw string comparison here, but it is nice to
;; be whitespace insensitive as long as we are creating valid S-exprs
;; as output.
(define (result-check res expected)
  (or
   (equal? res (string->number expected))
   (if (string? res)
       (string=? res expected)
       (string=? (number->string res) expected))
   (equal? (with-input-from-string (number->string res) read)
           (with-input-from-string expected read))))

;; Use exponential backoff to poll/sleep until a timeout is reached.
;; Takes: a control function as produced by "process".
;; Returns: status: 'done-ok, 'done-error, 'timed-out
(define (wait-or-timeout control-fun maxsecs)
  (define (poll)
    (match (control-fun 'status)
      ['running    #f]
      ['done-ok    'done-ok]
      ['done-error 'done-error]))
  (let loop ((slept 0) (delta 0.001)) ;; Start at 1ms
    (define remain (- maxsecs slept))
    (debug (format "Polling subprocess, ~a elapsed" slept))
    (cond
      [(>= slept maxsecs) 'timed-out]
      [(< remain delta)
       (sleep remain)
       (or (poll) 'timed-out)]
      [else
       (define slept2 (+ slept delta))
       (sleep delta)
       (or (poll) (loop slept2 (* 2 delta)))])))


;; The compiler-tests function takes a compiler name (a string), a
;; typechecker (see the comment for check-passes) a description of the
;; passes (ditto), a test family name (a string), and a list of test
;; numbers (see the comment for interp-tests), and runs the compiler
;; to generate x86 (a ".s" file) and then runs gcc to generate machine
;; code, unless a type error is detected. It runs the machine code and
;; stores the result. If the test file has a corresponding .res file,
;; the result is compared against its contents; otherwise, the result
;; is compared against 42. If a type error is detected, it will check
;; if a .tyerr file exists, and report an error if not. It will do the
;; same if a .tyerr file exists but the typechecker does not report an
;; error.

(define (get-value-or-fail command output)
  (match (process command)
    [`(,in1 ,out ,_ ,inErr ,control-fun)
     (let* ([timeout 3.0]
      [res (wait-or-timeout control-fun timeout)]
      [result (cond [(symbol=? res 'timed-out)
                           `(error timed-out ,timeout)]
        [(symbol=? res 'done-error)
                           `(error done-error ,(control-fun 'exit-code))]
        [else `(result done ,(read-line in1))])])
       (close-input-port in1)
       (close-input-port inErr)
       (close-output-port out)
       result)]))

(define (compiler-tests-suite name typechecker passes test-family test-nums)
  (let ([compiler (compile-file typechecker passes)])
    (make-test-suite
     "compiler tests"
     (for/list ([test-number (in-list test-nums)])
       (let* ([test-name (format "~a_~a" test-family test-number)]
              [type-error-expected (file-exists? (format "tests/~a.tyerr" test-name))]
              [typechecks (compiler (format "tests/~a.rkt" test-name))])
         (test-suite
          test-name
          (if type-error-expected
              (test-case "typecheck" (check-false typechecks "Expected expression to fail typechecking"))
        (if (not typechecks) (fail "Expected expression to typecheck")
      (test-case "code generation"
           (let ([gcc-output (system (format "gcc -g -march=x86-64 -std=c99 runtime.o tests/~a.s -o tests/~a.out" test-name test-name))])
             (if (not gcc-output) (fail "Failed during assembly")
           (let ([input (if (file-exists? (format "tests/~a.in" test-name))
                (format " < tests/~a.in" test-name)
                "")]
           [output (if (file-exists? (format "tests/~a.res" test-name))
                 (call-with-input-file
               (format "tests/~a.res" test-name)
                   (lambda (f) (read-line f)))
                 "42")]
           [error-expected (file-exists? (format "tests/~a.err" test-name))])
             (let* ([command (format "./tests/~a.out ~a" test-name input)]
              [result (get-value-or-fail command output)])
               (check-not-false gcc-output "Unable to run program, gcc reported assembly failure")
               (check-not-equal? (cadr result) 'timed-out (format "x86 execution timed out after ~a seconds" (caddr result)))
               (cond [error-expected
                (check-equal? (cadr result) 'done-error (format "expected error, not: ~a" (caddr result)))
                (check-equal? (caddr result) 255 (format "expected error, not: ~a" (caddr result)))]
               [else
                (check-not-eq? (cadr result) eof "x86 execution did not produce output")
                (check result-check (caddr result) output "Mismatched output from x86 execution")]))))))))))))))

(define (compiler-tests name typechecker passes test-family test-nums)
  (run-tests (compiler-tests-suite name typechecker passes test-family test-nums) (test-verbosity)))

(define (compiler-tests-gui name typechecker passes test-family test-nums)
  (test/gui (compiler-tests-suite name typechecker passes test-family test-nums)))


;; Takes a function of 1 argument (or #f) and Racket expression, and
;; returns whether the expression is well-typed. If the first argument
;; is #f, that means we aren't providing a typechecker so we simply
;; return true. If not, we apply the typechecker to the expression. We
;; require that a typechecker will EITHER raise an error using the
;; (error) function when it encounters a type error, or that it
;; returns #f when it encounters a type error. This function then
;; returns whether a type error was encountered.

;; TODO: when a type checking error is not expected, we should
;;   always print the error message. The following instead prints
;;   the error message when debug level >= 1. -Jeremy
(define (test-typecheck tcer exp)
  (define (handler e)
    (copious "test-typecheck" tcer exp e)
    (when (at-debug-level? 1)
    (display (exn-message e))
    (newline)(newline))
    #f)
  (if (eq? tcer #f)
      exp
      (let ([res (with-handlers ([exn:fail? handler])
                   (tcer exp))])
        (match res
          [#f #f]
          ;[(Program info body) res]
          [else res]
          ;[else exp]
          ))))

(define assert
  (lambda (msg b)
    (if (not b)
  (begin
    (display "ERROR: ")
    (display msg)
    (newline))
  (void))))

;; (case-> (symbol . -> . symbol) (string . -> . string))
(define (racket-id->c-id x)
  (define (->c-id-char c)
    (if (or (char<=? #\A c #\Z)
            (char<=? #\a c #\z)
            (char<=? #\0 c #\9))
        c
        #\_))
  (cond
    [(symbol? x) (string->symbol (racket-id->c-id (symbol->string x)))]
    [(string? x) (list->string (map ->c-id-char (string->list x)))]
    [else (error 'racket-id->c-id "expected string or symbol: ~v" x)]))
