#lang typed/racket

(require typed/rackunit typed/rackunit/text-ui typed/rackunit/gui)
(require (for-syntax typed/racket))


(define-type FinalAnswer Fixnum)

(provide FinalAnswer Info

         ;; eval
         ns-anchor eval-ns

         ;; type
         Type type? Op-Type op-type?

         ;; debug
         debug-level at-debug-level? debug trace verbose copious

         ;; ;; tests
         ;; interp-tests

         ;; tools
         read-fixnum integer->fixnum
         dim-fixnum-nullary dim-fixnum-unary dim-fixnum-binary
         goto-label get-CFG
         pseudo-goto-label pseudo-get-CFG
         symbol-append any-tag


         ;; env
         Env env? empty-env empty-env? env-ref
         env-remove env-set env-set*
         env-remove! env-set! env-set*!
         in-env env-has-key?


         ;; AST
         ast-name write-ast write-astln show-ast show-astln
         (except-out (struct-out AST) make-ast)


         ;; x86
         X86Reg X86Reg?
         atm->x86arg
         (struct-out X86Program) (struct-out Block)

         (except-out (struct-out X86Arg) make-x86arg)
         (struct-out Imm) (struct-out Reg) (struct-out Deref)

         (except-out (struct-out X86Instr) make-x86instr)
         (struct-out Instr) (struct-out Callq) (struct-out Retq)
         (struct-out Pushq) (struct-out Popq) (struct-out Jmp)

         ;; pseudo x86
         (struct-out Pseudo-X86Program) (struct-out Pseudo-Block)

         atm->pseudo-x86arg
         (except-out (struct-out Pseudo-X86Arg) make-pseudo-x86arg)
         (struct-out Pseudo-Var) (struct-out Pseudo-Imm) (struct-out Pseudo-Reg) (struct-out Pseudo-Deref)

         (except-out (struct-out Pseudo-X86Instr) make-pseudo-x86instr)
         (struct-out Pseudo-Instr) (struct-out Pseudo-Callq) (struct-out Pseudo-Retq)
         (struct-out Pseudo-Pushq) (struct-out Pseudo-Popq) (struct-out Pseudo-Jmp)


         ;; racket
         (struct-out Program)
         (except-out (struct-out Exp) make-exp)

         (except-out (struct-out Atm) make-atm)
         (struct-out Int) (struct-out Var)

         (struct-out Prim)
         (struct-out Let)


         ;; C
         (struct-out CProgram)

         (except-out (struct-out Tail) make-tail)
         (struct-out Return) (struct-out Seq)

         (struct-out Assign)

         )

;;; eval
(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))


(define-type Op-Type (Pair (Listof Type) Type))
(define-type Type (U Op-Type Symbol))

(define-predicate op-type? Op-Type)
(define-predicate type? Type)


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
(: debug-level (Parameter Natural))
(define debug-level (make-parameter 0))
;; (debug-level 2)

;; Check to see if debug state is at least some level
(: at-debug-level? [-> Natural Boolean])
(define (at-debug-level? n) (>= (debug-level) n))

(: test-verbosity [-> (U 'verbose 'normal)])
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


;;; Env

;; (define-type (Env A) (Listof (Pair Symbol A)))
;; (define-predicate env? Env)

;; (: env-ref (All (A) [-> (Env A) Symbol A]))
;; (define env-ref (λ (env var) (cdr (assoc env var symbol=?))))

;; (: env-remove (All (A) [-> (Env A) Symbol (Env A)]))
;; (define env-remove
;;   (λ (env var)
;;     (remove var env (ann (λ (ele) (symbol=? var (car ele)))
;;                          [-> (Pair Symbol A) Boolean]))))

;; (: env-set (All (A) [-> (Env A) Symbol A (Env A)]))
;; (define env-set (λ (env var val) (cons (cons var val) (env-remove env var))))

;; (: in-env (All (A) [-> (Env A) (Listof (Values Symbol A))]))
;; (define in-env
;;   (λ (env)
;;     (map (ann (λ (p) (values (car p) (cdr p)))
;;               [-> (Pair Symbol A) (Values Symbol A)])
;;          env)))


(define-type (Env A) (Mutable-HashTable Symbol A))
;; (define-predicate env? (Mutable-HashTable Symbol Any))
(define-predicate env? HashTableTop)

(: empty-env (All (A) [-> (Env A)]))
(define empty-env (λ () (make-hasheq)))

(: empty-env? (All (A) [-> (Env A) Boolean]))
(define empty-env? (λ (env) (hash-empty? env)))

(: env-ref (All (A) [-> (Env A) Symbol A]))
(define env-ref (λ (env var) (hash-ref env var)))

(: env-remove (All (A) [-> (Env A) Symbol (Env A)]))
(define env-remove
  (λ (env var)
    (: new-env (Env A))
    (define new-env (hash-copy env))
    (hash-remove! new-env var)
    new-env))

(: env-remove! (All (A) [-> (Env A) Symbol Void]))
(define env-remove! (λ (env var) (hash-remove! env var)))

(: env-set (All (A) [-> (Env A) Symbol A (Env A)]))
(define env-set
  (λ (env var val)
    (: new-env (Env A))
    (define new-env (hash-copy env))
    (hash-set! new-env var val)
    new-env))

(: env-set! (All (A) [-> (Env A) Symbol A Void]))
(define env-set! (λ (env var val) (hash-set! env var val)))

(: env-set* (All (A) [->* ((Env A)) #:rest-star (Symbol A) (Env A)]))
(define env-set*
  (λ (env . k-v)
    (: new-env (Env A))
    (define new-env (hash-copy env))
    (apply hash-set*! new-env k-v)
    new-env))

(: env-set*! (All (A) [->* ((Env A)) #:rest-star (Symbol A) Void]))
(define env-set*! (λ (env . k-v) (apply hash-set*! env k-v)))

(: in-env (All (A) [-> (Env A) (Env A)]))
(define in-env (λ (env) env))

(: env-has-key? (All (A) [-> (Env A) Symbol Boolean]))
(define env-has-key? (λ (env key) (hash-has-key? env key)))


;;; Dict
(define-type Dict (Listof (Pair Symbol Any)))
(define-predicate dict? Dict)


;; read
(: read-fixnum [-> Fixnum])
(define read-fixnum
  (λ ()
    (define res (read))
    (if (fixnum? res) res (error 'read "expected a fixnum"))))

(: integer->fixnum [-> Integer Fixnum])
(define integer->fixnum
  (λ (n)
    (if (fixnum? n)
        n
        (error 'integer->fixnum "expected a fixnum"))))

(: dim-fixnum-nullary [-> [-> Fixnum] [-> Fixnum * Fixnum]])
(define dim-fixnum-nullary (λ (op) (λ args (op))))

(: dim-fixnum-unary [-> [-> Fixnum Fixnum] [-> Fixnum * Fixnum]])
(define dim-fixnum-unary (λ (op) (λ args (match args [(list n) (op n)]))))

(: dim-fixnum-binary [-> [-> Fixnum Fixnum Fixnum] [-> Fixnum * Fixnum]])
(define dim-fixnum-binary (λ (op) (λ args (match args [(list r s) (op r s)]))))


;; write to out
(define-type Mode (U Boolean 0 1))

(: make-recur [-> Output-Port Mode
                  (U [-> Any Output-Port Void]
                     [->* (Any) (Output-Port) Void])])
(define make-recur
  (λ (out mode)
    (case mode
      [(#t) write]
      [(#f) display]
      [else (ann (lambda (arg out) (print arg out mode))
                 [-> Any Output-Port Void])])))

(: make-recurln [-> Output-Port Mode
                    (U [-> Any Output-Port Void]
                       [->* (Any) (Output-Port) Void])])
(define make-recurln
  (λ (out mode)
    (case mode
      [(#t) writeln]
      [(#f) displayln]
      [else (ann (lambda (arg out) (println arg out mode))
                 [-> Any Output-Port Void])])))

(: indent-width (Parameter Natural))
(define indent-width (make-parameter 4))

(: newline-and-indent [-> Output-Port (Option Integer) Void])
(define newline-and-indent
  (λ (out col)
    (void
     (let ([lead (if (false? col) "" (make-string col #\space))])
       (newline out)
       (write-string lead out)))))


(define-type Info (Listof (Pair Symbol Any)))

(: print-info [-> Info Output-Port Mode Void])
(define print-info
  (λ (info out mode)
    (define recur (make-recur out mode))

    (for ([i info])
      (define label (car i))
      (define data  (cdr i))
      (match label
        ['locals-types
         (write-string "locals-types:" out)
         (newline out)
         (cond [(dict? data)
                (for ([datum data])
                  (define var  (car datum))
                  (define type (cdr datum))
                  (write-string (make-string (indent-width) #\space) out)
                  (write-string (symbol->string var) out)
                  (write-string " : " out)
                  (recur type out)
                  (newline out))
                (newline out)]
               [else
                (recur data out)
                (newline out)])]
        [_
         (write-string (symbol->string label) out)
         (write-string ":" out)
         (newline out)
         (recur data out)
         (newline out)]))))


;; AST
(struct AST () #:constructor-name make-ast #:transparent) ; #:abstract

(: ast-name [-> (U Procedure AST) Symbol])
(define ast-name (λ (ast) (assert (object-name ast) symbol?)))

(: write-ast [->* (AST) (Output-Port Mode) Void])
(define write-ast (λ (ast [out (current-output-port)] [mode #f]) ((hash-ref AST-format-table (ast-name ast)) ast out mode)))

(: write-astln [->* (AST) (Output-Port Mode) Void])
(define write-astln (λ (ast [out (current-output-port)] [mode #f]) (write-ast ast out mode) (newline out)))

(: AST-format-table (Mutable-HashTable Symbol [-> AST Output-Port Mode Void]))
(define AST-format-table (make-hasheq))

(: add-AST-format! [-> Symbol [-> AST Output-Port Mode Void] Void])
(define add-AST-format! (λ (name op) (hash-set! AST-format-table name op)))


(: show-ast [-> AST Void])
(define show-ast
  (λ (ast)
    (parameterize ([port-count-lines-enabled #t])
      (define out (open-output-string))
      (write-ast ast out)
      (write-string (get-output-string out))
      (write-string "" out)
      (void))))

(: show-astln [-> AST Void])
(define show-astln (λ (ast) (show-ast ast) (newline)))

;;; x86 Language:
(define-type X86Reg (U 'rsp 'rbp 'rax 'rbx 'rcx 'rdx 'rsi 'rdi
                       'r8 'r9 'r10 'r11 'r12 'r13 'r14 'r15))
(define-predicate X86Reg? X86Reg)

(: atm->x86arg [-> Atm X86Arg])
(define atm->x86arg
  (λ (atm)
    (match atm
      [(Int x) (Imm x)])))

(struct X86Arg AST () #:constructor-name make-x86arg #:transparent) ; #:abstract
(struct Imm X86Arg ([value : Fixnum]) #:transparent)
(add-AST-format! 'Imm
                 (ann (λ (ast out mode)
                        (match ast [(Imm n) (write-string "$" out) (write n out)]))
                      [-> AST Output-Port Mode Void]))

(struct Reg X86Arg ([name : X86Reg]) #:transparent)
(add-AST-format! 'Reg
                 (ann (λ (ast out mode) (match ast [(Reg r) (write-string "%" out) (write r out)]))
                      [-> AST Output-Port Mode Void]))

(struct Deref X86Arg ([reg : X86Reg] [offset : Fixnum]) #:transparent)
(add-AST-format! 'Deref
                 (ann (λ (ast out mode)
                        (match ast
                          [(Deref reg offset)
                           (void
                            (write offset out)
                            (write-string "(" out)
                            (write-string "%" out)
                            (write reg out)
                            (write-string ")" out))]))
                      [-> AST Output-Port Mode Void]))


(struct X86Instr AST () #:constructor-name make-x86instr #:transparent) ; #:abstract
(struct Instr X86Instr ([name : Symbol] [arg* : (Listof X86Arg)]) #:transparent)
(add-AST-format! 'Instr
                 (ann (λ (ast out mode)
                        (match ast
                          [(Instr name arg*)
                           (let-values ([(line col pos) (port-next-location out)])
                             (write name out)
                             (for ([arg arg*]
                                   [i (in-naturals)])
                               (unless (zero? i) (write-string "," out))
                               (write-string " " out)
                               (write-ast arg out))
                             (newline-and-indent out col))]))
                      [-> AST Output-Port Mode Void]))

(struct Callq X86Instr ([target : Symbol] [arity : Fixnum]) #:transparent)
(add-AST-format! 'Callq
                 (ann (λ (ast out mode)
                        (match ast
                          [(Callq target arity)
                           (let-values ([(line col pos) (port-next-location out)])
                             (void (write-string "callq" out)
                                   (write-string " " out)
                                   (write target out)
                                   (newline-and-indent out col)))]))
                      [-> AST Output-Port Mode Void]))

(struct Retq X86Instr () #:transparent)
(add-AST-format! 'Retq
                 (ann (λ (ast out mode)
                        (match ast
                          [(Retq)
                           (let-values ([(line col pos) (port-next-location out)])
                             (void (write-string "retq" out)
                                   (newline-and-indent out col)))]))
                      [-> AST Output-Port Mode Void]))

(struct Pushq X86Instr ([arg : X86Arg]) #:transparent)
(add-AST-format! 'Pushq
                 (ann (λ (ast out mode)
                        (match ast
                          [(Pushq arg)
                           (let-values ([(line col pos) (port-next-location out)])
                             (void (write-string "pushq" out)
                                   (write-string " " out)
                                   (write-ast arg out)
                                   (newline-and-indent out col)))]))
                      [-> AST Output-Port Mode Void]))

(struct Popq X86Instr ([arg : X86Arg]) #:transparent)
(add-AST-format! 'Popq
                 (ann (λ (ast out mode)
                        (match ast
                          [(Popq arg)
                           (let-values ([(line col pos) (port-next-location out)])
                             (void (write-string "popq" out)
                                   (write-string " " out)
                                   (write-ast arg out)
                                   (newline-and-indent out col)))]))
                      [-> AST Output-Port Mode Void]))

(struct Jmp X86Instr ([target : Symbol]) #:transparent)
(add-AST-format! 'Jmp
                 (ann (λ (ast out mode)
                        (match ast
                          [(Jmp target)
                           (let-values ([(line col pos) (port-next-location out)])
                             (void (write-string "jmp" out)
                                   (write-string " " out)
                                   (write target out)
                                   (newline-and-indent out col)))]))
                      [-> AST Output-Port Mode Void]))


(struct Block AST ([info : Info] [instr* : (Listof X86Instr)]))
(add-AST-format! 'Block
                 (ann (λ (ast out mode)
                        (match ast
                          [(Block info instr*)
                           (print-info info out mode)
                           (write-string (make-string (indent-width) #\space) out)
                           (for ([instr instr*])
                             (write-ast instr out))
                           (newline out)]))
                      [-> AST Output-Port Mode Void]))


(struct X86Program AST
  ([info : Info]
   ;; [body : (Pair (Pair (U '_main 'main '_start 'start) Block) (Listof (Pair Symbol Block)))]
   [body : (Listof (Pair Symbol Block))])
  #:transparent)
(add-AST-format! 'X86Program
                 (ann (λ (ast out mode)
                        (match ast
                          [(X86Program info body)
                           ;; (write-string "x86 program:" out)
                           (newline out)
                           (print-info info out mode)
                           (write-string ".global " out)
                           (writeln (caar body) out)
                           (for ([seg body])
                             (write (car seg) out)
                             (write-string ":\n" out)
                             (write-ast (cdr seg) out))]))
                      [-> AST Output-Port Mode Void]))


;;; Racket Language:
(struct Exp AST () #:constructor-name make-exp #:transparent) ; #:abstract

(struct Program AST ([info : Info] [body : Exp]) #:transparent)
(add-AST-format! 'Program
                 (ann (λ (ast out mode)
                        (match ast
                          [(Program info body)
                           ;; (write-string "Racket program:" out)
                           (newline out)
                           (print-info info out mode)
                           (if (list? body)
                               (for ([def body])
                                 (write-ast def out))
                               (write-ast body out))]))
                      [-> AST Output-Port Mode Void]))


(struct Atm Exp () #:constructor-name make-atm #:transparent) ; #:abstract

(struct Int Atm ([value : Fixnum]))
(add-AST-format! 'Int
                 (ann (λ (ast out mode) (match ast [(Int n) (write n out)]))
                      [-> AST Output-Port Mode Void]))

(struct Var Atm ([name : Symbol]))
(add-AST-format! 'Var
                 (ann (λ (ast out mode) (match ast [(Var v) (write v out)]))
                      [-> AST Output-Port Mode Void]))

(struct Prim Exp ([op : Symbol] [arg* : (Listof Exp)]))
(add-AST-format! 'Prim
                 (ann (λ (ast out mode)
                        (match ast
                          [(Prim op arg*)
                           (void (write-string "(" out)
                                 (write-string (symbol->string op) out)
                                 (for ([arg arg*])
                                   (write-string " " out)
                                   (write-ast arg out))
                                 (write-string ")" out))]))
                      [-> AST Output-Port Mode Void]))

(struct Let Exp ([var : Symbol] [rhs : Exp] [body : Exp]))
(add-AST-format! 'Let
                 (ann (λ (ast out mode)
                        (match ast
                          [(Let x rhs body)
                           (void (let-values ([(line col pos) (port-next-location out)])
                                   (write-string "(let ([" out)
                                   (write-string (symbol->string x) out)
                                   (write-string " " out)
                                   (write-ast rhs out)
                                   (write-string "])" out)
                                   (newline-and-indent out col)
                                   (write-string "   " out) ;; indent body
                                   (write-ast body out)
                                   (write-string ")" out)
                                   (newline-and-indent out col)))]))
                      [-> AST Output-Port Mode Void]))

;; Pseudo x86 language:
(: atm->pseudo-x86arg [-> Atm Pseudo-X86Arg])
(define atm->pseudo-x86arg
  (λ (atm)
    (match atm
      [(Int x) (Pseudo-Imm x)]
      [(Var x) (Pseudo-Var x)])))

(struct Pseudo-X86Arg AST () #:constructor-name make-pseudo-x86arg #:transparent) ; #:abstract
(struct Pseudo-Var Pseudo-X86Arg ([value : Symbol]) #:transparent)
(add-AST-format! 'Pseudo-Var
                 (ann (λ (ast out mode)
                        (match ast [(Pseudo-Var n) (write-string "@" out) (write n out)]))
                      [-> AST Output-Port Mode Void]))
(struct Pseudo-Imm Pseudo-X86Arg ([value : Fixnum]) #:transparent)
(add-AST-format! 'Pseudo-Imm
                 (ann (λ (ast out mode)
                        (match ast [(Pseudo-Imm n) (write-string "$" out) (write n out)]))
                      [-> AST Output-Port Mode Void]))

(struct Pseudo-Reg Pseudo-X86Arg ([name : X86Reg]) #:transparent)
(add-AST-format! 'Pseudo-Reg
                 (ann (λ (ast out mode) (match ast [(Pseudo-Reg r) (write-string "%" out) (write r out)]))
                      [-> AST Output-Port Mode Void]))

(struct Pseudo-Deref Pseudo-X86Arg ([reg : X86Reg] [offset : Fixnum]) #:transparent)
(add-AST-format! 'Pseudo-Deref
                 (ann (λ (ast out mode)
                        (match ast
                          [(Pseudo-Deref reg offset)
                           (void
                            (write offset out)
                            (write-string "(" out)
                            (write-string "%" out)
                            (write reg out)
                            (write-string ")" out))]))
                      [-> AST Output-Port Mode Void]))


(struct Pseudo-X86Instr AST () #:constructor-name make-pseudo-x86instr #:transparent) ; #:abstract
(struct Pseudo-Instr Pseudo-X86Instr ([name : Symbol] [arg* : (Listof Pseudo-X86Arg)]) #:transparent)
(add-AST-format! 'Pseudo-Instr
                 (ann (λ (ast out mode)
                        (match ast
                          [(Pseudo-Instr name arg*)
                           (let-values ([(line col pos) (port-next-location out)])
                             (write name out)
                             (for ([arg arg*]
                                   [i (in-naturals)])
                               (unless (zero? i) (write-string "," out))
                               (write-string " " out)
                               (write-ast arg out))
                             (newline-and-indent out col))]))
                      [-> AST Output-Port Mode Void]))

(struct Pseudo-Callq Pseudo-X86Instr ([target : Symbol] [arity : Fixnum]) #:transparent)
(add-AST-format! 'Pseudo-Callq
                 (ann (λ (ast out mode)
                        (match ast
                          [(Pseudo-Callq target arity)
                           (let-values ([(line col pos) (port-next-location out)])
                             (void (write-string "callq" out)
                                   (write-string " " out)
                                   (write target out)
                                   (newline-and-indent out col)))]))
                      [-> AST Output-Port Mode Void]))

(struct Pseudo-Retq Pseudo-X86Instr () #:transparent)
(add-AST-format! 'Pseudo-Retq
                 (ann (λ (ast out mode)
                        (match ast
                          [(Pseudo-Retq)
                           (let-values ([(line col pos) (port-next-location out)])
                             (void (write-string "retq" out)
                                   (newline-and-indent out col)))]))
                      [-> AST Output-Port Mode Void]))

(struct Pseudo-Pushq Pseudo-X86Instr ([arg : Pseudo-X86Arg]) #:transparent)
(add-AST-format! 'Pseudo-Pushq
                 (ann (λ (ast out mode)
                        (match ast
                          [(Pseudo-Pushq arg)
                           (let-values ([(line col pos) (port-next-location out)])
                             (void (write-string "pushq" out)
                                   (write-string " " out)
                                   (write-ast arg out)
                                   (newline-and-indent out col)))]))
                      [-> AST Output-Port Mode Void]))

(struct Pseudo-Popq Pseudo-X86Instr ([arg : Pseudo-X86Arg]) #:transparent)
(add-AST-format! 'Pseudo-Popq
                 (ann (λ (ast out mode)
                        (match ast
                          [(Pseudo-Popq arg)
                           (let-values ([(line col pos) (port-next-location out)])
                             (void (write-string "popq" out)
                                   (write-string " " out)
                                   (write-ast arg out)
                                   (newline-and-indent out col)))]))
                      [-> AST Output-Port Mode Void]))

(struct Pseudo-Jmp Pseudo-X86Instr ([target : Symbol]) #:transparent)
(add-AST-format! 'Pseudo-Jmp
                 (ann (λ (ast out mode)
                        (match ast
                          [(Pseudo-Jmp target)
                           (let-values ([(line col pos) (port-next-location out)])
                             (void (write-string "jmp" out)
                                   (write-string " " out)
                                   (write target out)
                                   (newline-and-indent out col)))]))
                      [-> AST Output-Port Mode Void]))


(struct Pseudo-Block AST ([info : Info] [instr* : (Listof Pseudo-X86Instr)]))
(add-AST-format! 'Pseudo-Block
                 (ann (λ (ast out mode)
                        (match ast
                          [(Pseudo-Block info instr*)
                           (print-info info out mode)
                           (write-string (make-string (indent-width) #\space) out)
                           (for ([instr instr*])
                             (write-ast instr out))
                           (newline out)]))
                      [-> AST Output-Port Mode Void]))


(struct Pseudo-X86Program AST
  ([info : Info]
   ;; [body : (Pair (Pair (U '_main 'main '_start 'start) Pseudo-Block) (Listof (Pair Symbol Pseudo-Block)))]
   [body : (Listof (Pair Symbol Pseudo-Block))])
  #:transparent)
(add-AST-format! 'Pseudo-X86Program
                 (ann (λ (ast out mode)
                        (match ast
                          [(Pseudo-X86Program info body)
                           ;; (write-string "pseudo x86 program:" out)
                           (newline out)

                           (write-string ".global " out)
                           (writeln (caar body) out)

                           (print-info info out mode)

                           (for ([seg body])
                             (write (car seg) out)
                             (write-string ":\n" out)
                             (write-ast (cdr seg) out))]))
                      [-> AST Output-Port Mode Void]))


;; C Language:
(struct CProgram AST
  ([info : Info]
   [body : (Pair (Pair (U '_main 'main '_start 'start) Tail)
                 (Listof (Pair Symbol Tail)))])
  #:transparent)
(add-AST-format! 'CProgram
                 (ann (λ (ast out mode)
                        (match ast
                          [(CProgram info body)
                           ;; (write-string "C program:" out)
                           (newline out)

                           (print-info info out mode)

                           (for ([seg body])
                             (define label (car seg))
                             (define tail  (cdr seg))

                             (write-string (symbol->string label) out)
                             (write-string ":" out)
                             (newline out)
                             (write-string (make-string (indent-width) #\space) out)
                             (write-astln tail out))]))
                      [-> AST Output-Port Mode Void]))

(struct Tail AST () #:constructor-name make-tail #:transparent) ; abstract
(struct Return Tail ([exp : (U Atm Prim)]) #:transparent)
(add-AST-format! 'Return
                 (ann (λ (ast out mode)
                        (match ast
                          [(Return exp)
                           (void (write-string "return " out)
                                 (write-ast exp out)
                                 (write-string ";\n" out))]))
                      [-> AST Output-Port Mode Void]))

(struct Seq Tail ([stmt : Assign] [tail : Tail]) #:transparent)
(add-AST-format! 'Seq
                 (ann (λ (ast out mode)
                        (match ast
                          [(Seq stmt tail)
                           (write-ast stmt out)
                           (write-ast tail out)]))
                      [-> AST Output-Port Mode Void]))


(struct Assign AST ([var : Var] [exp : Exp]) #:transparent)
(add-AST-format! 'Assign
                 (ann (λ (ast out mode)
                        (match ast
                          [(Assign var exp)
                           (let-values ([(line col pos) (port-next-location out)])
                             (void (write-ast var out)
                                   (write-string " = " out)
                                   (write-ast exp out)
                                   (write-string ";" out)
                                   (newline-and-indent out col)))]))
                      [-> AST Output-Port Mode Void]))


;; ;; The check-passes function takes a compiler name (a string), a
;; ;; typechecker (see below), a description of the passes (see below),
;; ;; and an initial interpreter to apply to the initial expression, and
;; ;; returns a function that takes a test name and runs the passes and
;; ;; the appropriate interpreters to test the correctness of all the
;; ;; passes. This function assumes there is a "tests" subdirectory and a
;; ;; file in that directory whose name is the test name followed by
;; ;; ".rkt". Also, there should be a matching file with the ending ".in"
;; ;; that provides the input for the Scheme program. If any program
;; ;; should not pass typechecking, then there is a file with the name
;; ;; number (whose contents are ignored) that ends in ".tyerr".
;; ;;
;; ;; The description of the passes is a list with one entry per pass.
;; ;; An entry is a list with three things: a string giving the name of
;; ;; the pass, the function that implements the pass (a translator from
;; ;; AST to AST), and a function that implements the interpreter (a
;; ;; function from AST to result value).
;; ;;
;; ;; The typechecker is a function of exactly one argument that EITHER
;; ;; raises an error using the (error) function when it encounters a
;; ;; type error, or returns #f when it encounters a type error.

;; ;; (define (strip-has-type e)
;; ;;   e)

;; #;(define (strip-has-type e)
;;     (match e
;;       [`(has-type ,e ,T)
;;        (strip-has-type e)]
;;       [`(,(app strip-has-type e*) ...)
;;        `(,@e*)]
;;       [else
;;        e]))

;; (define ((check-exception name test-name error-expected) fn)
;;   (with-handlers
;;       ([exn:fail?
;;         (lambda (exn)
;;           (cond [error-expected 'expected-error]
;;                 [else
;;                  (displayln (format "encountered exception while testing '~a`, case ~a" name test-name))
;;                  (raise exn)]))])
;;     (let ([res (fn)])
;;       (when (and (not (string? res)) (not (pair? res)) (not (eq? res #f)))
;;         (check-false error-expected (format "in check-exception, expected exception, not ~a" res)))
;;       res)))

;; (define ((check-passes-suite name typechecker passes initial-interp) test-name)
;;   (test-suite
;;    test-name
;;    (let* ([input-file-name (format "tests/~a.in" test-name)]
;;           [result-file-name (format "tests/~a.res" test-name)]
;;           [program-name (format "tests/~a.rkt" test-name)]
;;           [sexp (read-program program-name)]
;;           [type-error-expected (file-exists? (format "tests/~a.tyerr" test-name))]
;;           [tsexp ((check-exception name test-name type-error-expected)
;;                   (thunk (test-typecheck typechecker sexp)))]
;;           [error-expected (file-exists? (format "tests/~a.err" test-name))]
;;           [checker (check-exception name test-name error-expected)])
;;      (test-case
;;          "typecheck"
;;        (if type-error-expected
;;            (check-false
;;             tsexp
;;             (format "expected type error in compiler '~a', case ~a, but no error raised by typechecker" name test-name))
;;            (check-not-false
;;             tsexp
;;             (format "expected no type error in compiler '~a', case ~a, but received error ~a" name test-name tsexp))))
;;      (trace "type checker output:" (strip-has-type tsexp))
;;      (unless type-error-expected
;;        (make-test-suite
;;         "passes"
;;         (let ([expected-result (cond [initial-interp
;;                                       (if (file-exists? input-file-name)
;;                                           (with-input-from-file input-file-name
;;                                             (lambda () (checker (thunk (initial-interp tsexp)))))
;;                                           (checker (thunk (initial-interp tsexp))))]
;;                                      [else
;;                                       (if (file-exists? result-file-name)
;;                                           (call-with-input-file result-file-name
;;                                             (lambda (f) (string->number (read-line f))))
;;                                           42)])])
;;           (let loop ([passes passes]
;;                      [p tsexp]
;;                      [tests '()])
;;             (trace "testing" test-name expected-result)
;;             (cond [(null? passes) (reverse tests)]
;;                   [else
;;                    (define pass-info (car passes))
;;                    (define pass-name (list-ref pass-info 0))
;;                    (define pass      (list-ref pass-info 1))
;;                    (define interp    (list-ref pass-info 2))
;;                    (define type-checker
;;                      (cond [(>= (length pass-info) 4)
;;                             (list-ref pass-info 3)]
;;                            [else #f]))
;;                    (trace (string-append "running pass: " pass-name))
;;                    (define input p)
;;                    (define new-p^ ((check-exception name test-name #f) (thunk (pass p))))
;;                    (trace "pass output: " (strip-has-type new-p^))
;;                    (define new-p (cond [type-checker
;;                                         (trace "type checking...")
;;                                         (type-checker new-p^)]
;;                                        [else new-p^]))
;;                    (trace "type-check output: " (strip-has-type new-p))
;;                    (cond [interp
;;                           (define result
;;                             (if (file-exists? input-file-name)
;;                                 (with-input-from-file input-file-name
;;                                   (lambda () (checker (thunk (interp new-p)))))
;;                                 (checker (thunk (interp new-p)))))
;;                           (trace "output: " result)
;;                           (cond [expected-result
;;                                  (loop (cdr passes) new-p
;;                                        (cons (test-suite
;;                                               (string-append "pass " pass-name)
;;                                               (check-equal?
;;                                                result expected-result
;;                                                (format "differing results in compiler '~a' on test '~a' pass '~a', expected ~a, not ~a" name test-name pass-name expected-result result)))
;;                                              tests))]
;;                                 [else
;;                                  (loop (cdr passes) new-p tests)]
;;                                 );; cond result
;;                           ]
;;                          [else
;;                           (loop (cdr passes) new-p tests)]
;;                          ) ;; cond interp
;;                    ]
;;                   ))))))))


;; ;; The interp-tests function takes a compiler name (a string), a
;; ;; typechecker (see the comment for check-passes) a description of the
;; ;; passes (ditto) a test family name (a string), and a list of test
;; ;; numbers, and runs the compiler passes and the interpreters to check
;; ;; whether the passes correct.
;; ;;
;; ;; This function assumes that the subdirectory "tests" has a bunch of
;; ;; Scheme programs whose names all start with the family name,
;; ;; followed by an underscore and then the test number, ending in
;; ;; ".rkt". Also, for each Scheme program there is a file with the same
;; ;; number except that it ends with ".in" that provides the input for
;; ;; the Scheme program. If any program should not pass typechecking,
;; ;; then there is a file with the name number (whose contents are
;; ;; ignored) that ends in ".tyerr".
;; ;;
;; ;; (define (interp-tests name typechecker passes initial-interp test-family test-nums)
;; ;;   (define checker (check-passes name typechecker passes initial-interp))
;; ;;   (for ([test-number (in-list test-nums)])
;; ;;     (let ([test-name (format "~a_~a" test-family test-number)])
;; ;;       (debug "utilities/interp-test" test-name)
;; ;;       (checker test-name))))

;; (: interp-tests (All (A) [-> String (Option [-> A (Option A)])
;;                              (Listof )

;;                              Natural]))
;; (define (interp-tests name typechecker passes initial-interp test-family test-nums)
;;   (run-tests (interp-tests-suite name typechecker passes initial-interp test-family test-nums) (test-verbosity)))

;; (define (interp-tests-suite name typechecker passes initial-interp test-family test-nums)
;;   (define checker-suite (check-passes-suite name typechecker passes initial-interp))
;;   (make-test-suite
;;    "interpreter tests"
;;    (for/list ([test-number (in-list test-nums)])
;;      (let ([test-name (format "~a_~a" test-family test-number)])
;;        (checker-suite test-name)))))


;; This parameter (dynamically scoped thingy) is used for goto.
(: pseudo-get-CFG (Parameter (Listof (Pair Symbol Pseudo-Block))))
(define pseudo-get-CFG (make-parameter '()))

(: pseudo-goto-label [-> Symbol (Option Pseudo-Block)])
(define (pseudo-goto-label label)
  (: lb (Option (Pair Symbol Pseudo-Block)))
  (define lb (assq label (pseudo-get-CFG)))
  (if (false? lb) #f (cdr lb)))

(: get-CFG (Parameter (Listof (Pair Symbol Block))))
(define get-CFG (make-parameter '()))

(: goto-label [-> Symbol Block])
(define (goto-label label)
  (: lb (Option (Pair Symbol Block)))
  (define lb (assq label (get-CFG)))
  (if (false? lb)
      (error "There isn't a block label called ~a!" label)
      (cdr lb)))

(: symbol-append [-> Symbol Symbol Symbol])
(define (symbol-append s1 s2)
  (string->symbol (string-append (symbol->string s1) (symbol->string s2))))

(: any-tag [-> Any Fixnum])
(define (any-tag ty)
  (match ty
    ['Integer 1]		;; 001
    ['Boolean 4]		;; 100
    ['Void 5]                   ;; 101
    [`(Vector ,ts ...) 2]	;; 010
    [`(PVector ,ts ...) 2]
    [`(,ts ... -> ,rt) 3]	;; 011
    [else (error "in any-tag, unrecognized type" ty)]
    ))
