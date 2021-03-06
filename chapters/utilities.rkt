#lang typed/racket

;; (require racket/struct)
;; (require graph)

(define-type FinalAnswer Fixnum)

(provide FinalAnswer

         ;; env
         Env env? empty-env empty-env? env-ref env-remove env-set in-env


         ;; AST
         ast-name write-ast write-astln show-ast show-astln
         (except-out (struct-out AST) make-ast)


         ;; x86
         X86Reg X86Reg?
         (struct-out X86Program) (struct-out Block)

         (except-out (struct-out X86Arg) make-x86arg)
         (struct-out Imm) (struct-out Reg) (struct-out Deref)

         (except-out (struct-out X86Instr) make-x86instr)
         (struct-out Instr) (struct-out Callq) (struct-out Retq)
         (struct-out Pushq) (struct-out Popq) (struct-out Jmp)


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


(define-type (Env A) (Immutable-HashTable Symbol A))
(define-predicate env? (Immutable-HashTable Symbol Any))

(: empty-env (All (A) [-> (Env A)]))
(define empty-env (λ () (make-immutable-hasheq)))

(: empty-env? (All (A) [-> (Env A) Boolean]))
(define empty-env? (λ (env) (hash-empty? env)))

(: env-ref (All (A) [-> (Env A) Symbol A]))
(define env-ref (λ (env var) (hash-ref env var)))

(: env-remove (All (A) [-> (Env A) Symbol (Env A)]))
(define env-remove (λ (env var) (hash-remove env var)))

(: env-set (All (A) [-> (Env A) Symbol A (Env A)]))
(define env-set (λ (env var val) (hash-set env var val)))

(: in-env (All (A) [-> (Env A) (Env A)]))
(define in-env (λ (env) env))


;; write to port
(: make-recur [-> Output-Port (U Boolean 0 1)
                  (U [-> Any Output-Port Void]
                     [->* (Any) (Output-Port) Void])])
(define make-recur
  (λ (out mode)
    (case mode
      [(#t) write]
      [(#f) display]
      [else (ann (lambda (arg out) (print arg out mode))
                 [-> Any Output-Port Void])])))

(: make-recurln [-> Output-Port (U Boolean 0 1)
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


;; AST
(struct AST () #:constructor-name make-ast #:transparent) ; #:abstract

(: ast-name [-> (U Procedure AST) Symbol])
(define ast-name (λ (ast) (assert (object-name ast) symbol?)))

(: write-ast [->* (AST) (Output-Port (U Boolean 0 1)) Void])
(define write-ast (λ (ast [out (current-output-port)] [mode #f]) ((hash-ref AST-format-table (ast-name ast)) ast out mode)))

(: write-astln [->* (AST) (Output-Port (U Boolean 0 1)) Void])
(define write-astln (λ (ast [out (current-output-port)] [mode #f]) (write-ast ast out mode) (newline out)))

(: AST-format-table (Mutable-HashTable Symbol [-> AST Output-Port (U Boolean 0 1) Void]))
(define AST-format-table (make-hasheq))

(: add-AST-format! [-> Symbol [-> AST Output-Port (U Boolean 0 1) Void] Void])
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

(struct X86Arg AST () #:constructor-name make-x86arg #:transparent) ; #:abstract
(struct Imm X86Arg ([value : Fixnum]) #:transparent)
(add-AST-format! 'Imm
                 (ann (λ (ast out mode)
                        (match ast [(Imm n) (write-string "$" out) (write n out)]))
                      [-> AST Output-Port (U Boolean 0 1) Void]))

(struct Reg X86Arg ([name : X86Reg]) #:transparent)
(add-AST-format! 'Reg
                 (ann (λ (ast out mode) (match ast [(Reg r) (write-string "%" out) (write r out)]))
                      [-> AST Output-Port (U Boolean 0 1) Void]))

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
                      [-> AST Output-Port (U Boolean 0 1) Void]))


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
                      [-> AST Output-Port (U Boolean 0 1) Void]))

(struct Callq X86Instr ([target : Symbol]) #:transparent)
(add-AST-format! 'Callq
                 (ann (λ (ast out mode)
                        (match ast
                          [(Callq target)
                           (let-values ([(line col pos) (port-next-location out)])
                             (void (write-string "callq" out)
                                   (write-string " " out)
                                   (write target out)
                                   (newline-and-indent out col)))]))
                      [-> AST Output-Port (U Boolean 0 1) Void]))

(struct Retq X86Instr () #:transparent)
(add-AST-format! 'Retq
                 (ann (λ (ast out mode)
                        (match ast
                          [(Retq)
                           (let-values ([(line col pos) (port-next-location out)])
                             (void (write-string "retq" out)
                                   (newline-and-indent out col)))]))
                      [-> AST Output-Port (U Boolean 0 1) Void]))

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
                      [-> AST Output-Port (U Boolean 0 1) Void]))

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
                      [-> AST Output-Port (U Boolean 0 1) Void]))

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
                      [-> AST Output-Port (U Boolean 0 1) Void]))


(struct Block AST ([info : (Listof Any)] [instr* : (Listof X86Instr)]))
(add-AST-format! 'Block
                 (ann (λ (ast out mode)
                        (match ast
                          [(Block info instr*)
                           (write-string (make-string (indent-width) #\space) out)
                           (for ([instr instr*])
                             (write-ast instr out))
                           (newline out)]))
                      [-> AST Output-Port (U Boolean 0 1) Void]))


(struct X86Program AST
  ([info : (Listof Any)]
   [body : (Pair (Pair (U '_main 'main '_start 'start) Block) (Listof (Pair Symbol Block)))])
  #:transparent)
(add-AST-format! 'X86Program
                 (ann (λ (ast out mode)
                        (match ast
                          [(X86Program info body)
                           (write-string ".global " out)
                           (writeln (caar body) out)
                           (for ([seg body])
                             (write (car seg) out)
                             (write-string ":\n" out)
                             (write-ast (cdr seg) out))]))
                      [-> AST Output-Port (U Boolean 0 1) Void]))


;;; Racket Language:
(struct Exp AST () #:constructor-name make-exp #:transparent) ; #:abstract

(struct Program AST ([info : (Listof Any)] [body : Exp]) #:transparent)
(add-AST-format! 'Program
                 (ann (λ (ast out mode)
                        (match ast
                          [(Program info body)
                           (writeln "program:" out)
                           (if (list? body)
                               (for ([def body])
                                 (write-ast def out))
                               (write-ast body out))]))
                      [-> AST Output-Port (U Boolean 0 1) Void]))


(struct Atm Exp () #:constructor-name make-atm #:transparent) ; #:abstract

(struct Int Atm ([value : Fixnum]))
(add-AST-format! 'Int
                 (ann (λ (ast out mode) (match ast [(Int n) (write n out)]))
                      [-> AST Output-Port (U Boolean 0 1) Void]))

(struct Var Atm ([name : Symbol]))
(add-AST-format! 'Var
                 (ann (λ (ast out mode) (match ast [(Var v) (write v out)]))
                      [-> AST Output-Port (U Boolean 0 1) Void]))

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
                      [-> AST Output-Port (U Boolean 0 1) Void]))

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
                      [-> AST Output-Port (U Boolean 0 1) Void]))

;; C Language:
(struct CProgram AST
  ([info : (Listof Any)]
   [body : (Pair (Pair (U '_main 'main '_start 'start) Tail) (Listof (Pair Symbol Tail)))])
  #:transparent)
(add-AST-format! 'CProgram
                 (ann (λ (ast out mode)
                        (match ast
                          [(CProgram info body)
                           (for ([seg body])
                             (write-string "(" out)
                             (write (car seg) out)
                             (write-string ": " out)
                             (write-ast (cdr seg) out)
                             (write-string ")" out))]))
                      [-> AST Output-Port (U Boolean 0 1) Void]))

(struct Tail AST () #:constructor-name make-tail #:transparent) ; abstract
(struct Return Tail ([exp : Exp]) #:transparent)
(struct Seq Tail ([stmt : Assign] [tail : Tail]) #:transparent)

(struct Assign ([var : Var] [exp : Exp]) #:transparent)