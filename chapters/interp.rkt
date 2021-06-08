#lang typed/racket

(require racket/fixnum)
(require "utilities.rkt"
         "untyped-utilities.rkt"
         (prefix-in runtime-config: "runtime-config.rkt"))

(provide interp-pseudo-x86-0 interp-x86-0)

;; The interpreters in this file are for the intermediate languages
;; produced by the various passes of the compiler.
;;
;; The interpreters for the source languages (Rvar, Rif, ...)
;; and the C intermediate languages Cvar and Cif
;; are in separate files, e.g., interp-Rvar.rkt.
(: interp-pseudo-x86-0 [-> Pseudo-X86Program FinalAnswer])
(define interp-pseudo-x86-0
  (lambda (p)
    ((send (new interp-R1-class) interp-pseudo-x86 ((inst empty-env Fixnum))) p)))

(: interp-x86-0 [-> X86Program FinalAnswer])
(define interp-x86-0
  (lambda (p)
    ((send (new interp-R1-class) interp-x86 ((inst empty-env Fixnum))) p)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpreters for R1: fixnum arithmetic and 'let'

(: interp-R1-class (Class (field [result Symbol]
                                 [x86-ops (Immutable-HashTable Symbol (List Natural [-> Fixnum * Fixnum]))])

                          [observe-value [-> Fixnum Fixnum]]
                          [return-from-tail [-> Fixnum (Env Fixnum) (Pair (Pair Symbol Fixnum) (Env Fixnum))]]
                          [is-return? [-> Any Boolean]]
                          [primitives [-> (List '+ '- 'read)]]

                          ;; R1
                          [interp-op [-> Symbol [-> Fixnum * Fixnum]]]
                          [interp-scheme-exp [-> (Env Fixnum) [-> Exp Fixnum]]]
                          [interp-scheme [-> (Env Fixnum) [-> Program FinalAnswer]]]

                          ;; C0
                          [interp-C-exp [-> (Env Fixnum) [-> Exp Fixnum]]]
                          [interp-C-tail [-> (Env Fixnum) [-> Tail Fixnum]]]
                          [interp-C-stmt [-> (Env Fixnum) [-> Assign (Env Fixnum)]]]
                          [interp-C [-> CProgram FinalAnswer]]

                          ;; x86
                          [get-name [-> AST Symbol]]
                          [interp-x86-op [-> Symbol [-> Fixnum * Fixnum]]]
                          [interp-x86-exp [-> (Env Fixnum) [-> AST Fixnum]]]
                          [interp-x86-instr [-> (Env Fixnum) [-> (Listof X86Instr) (Env Fixnum)]]]
                          [interp-x86-block [-> (Env Fixnum) [-> Block (Env Fixnum)]]]
                          [interp-x86 [-> (Env Fixnum) [-> X86Program FinalAnswer]]]

                          ;; psuedo-x86
                          [pseudo-get-name [-> AST Symbol]]
                          [interp-pseudo-x86-op [-> Symbol [-> Fixnum * Fixnum]]]
                          [interp-pseudo-x86-exp [-> (Env Fixnum) [-> AST Fixnum]]]
                          [interp-pseudo-x86-instr [-> (Env Fixnum) [-> (Listof Pseudo-X86Instr) (Env Fixnum)]]]
                          [interp-pseudo-x86-block [-> (Env Fixnum) [-> Pseudo-Block (Env Fixnum)]]]
                          [interp-pseudo-x86 [-> (Env Fixnum) [-> Pseudo-X86Program FinalAnswer]]]

                          ))
(define interp-R1-class
  (class object%
    (super-new)

    (field [result : Symbol (gensym 'result)])

    ;; Hide details for debug output.
    (: observe-value [-> Fixnum Fixnum])
    (define/public (observe-value v) v)

    (: return-from-tail [-> Fixnum (Env Fixnum) (Pair (Pair Symbol Fixnum) (Env Fixnum))])
    (define/public (return-from-tail v env)
      (cons (cons result v) env))

    (: is-return? [-> Any Boolean])
    (define/public (is-return? e)
      (match e
        [(cons (cons res v) env) (equal? res result)]
        [_ #f]))

    (: primitives [-> (List '+ '- 'read)])
    (define/public (primitives) '(+ - read))

    (: interp-op [-> Symbol [-> Fixnum * Fixnum]])
    (define/public (interp-op op)
      (match op
        ['+ (dim-fixnum-binary fx+)]
        ['- (dim-fixnum-binary fx-)]
        ['read (dim-fixnum-nullary read-fixnum)]
        [_ (error "in interp-op R1, unmatched" op)]))

    (: interp-scheme-exp [-> (Env Fixnum) [-> Exp Fixnum]])
    (define/public (interp-scheme-exp env)
      (lambda (ast)
        (define recur (interp-scheme-exp env))
        (verbose "R1/interp-scheme-exp" ast)
        (match ast
          [(Var x)
           (env-ref env x)]
          [(Int n) n]
          [(Let x e body)
           (define v (recur e))
           ((interp-scheme-exp (env-set env x v)) body)]
          [(Prim op args)
           (apply (interp-op op)
                  (for/list : (Listof Fixnum) ([e args]) (recur e)))]
          [_ (error (format "R1/no match in interp-scheme-exp for ~a" ast))])))

    (: interp-scheme [-> (Env Fixnum) [-> Program FinalAnswer]])
    (define/public (interp-scheme env)
      (lambda (ast)
        (verbose "R1/interp-scheme" ast)
        (match ast
          [(Program _ e)
           ((interp-scheme-exp (empty-env)) e)]
          [_ (error (format "R1/no match in interp-scheme for ~a" ast))])))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; C0

    (: interp-C-exp [-> (Env Fixnum) [-> Exp Fixnum]])
    (define/public (interp-C-exp env)
      (lambda (ast)
        (define recur (interp-C-exp env))
        (define result
          (match ast
            [(Var x) (env-ref env x)]
            [(Int n) n]
            [(Prim op args)
             (apply (interp-op op) (map (interp-C-exp env) args))]
            [_ (error "C0/interp-C-exp unhandled" ast)]))
        (verbose "C0/interp-C-exp" ast result)
        result))

    (: interp-C-tail [-> (Env Fixnum) [-> Tail Fixnum]])
    (define/public (interp-C-tail env)
      (lambda (ast)
        (match ast
          [(Return e)
           ((interp-C-exp env) e)]
          ;; (return-from-tail v env)  hmm -Jeremy
          [(Seq s t)
           (: new-env (Env Fixnum))
           (define new-env ((interp-C-stmt env) s))
           ((interp-C-tail new-env) t)]
          [_ (error "interp-C-tail unhandled" ast)])))

    (: interp-C-stmt [-> (Env Fixnum) [-> Assign (Env Fixnum)]])
    (define/public (interp-C-stmt env)
      (lambda (ast)
        (verbose "C0/interp-C-stmt" ast)
        (match ast
          [(Assign (Var x) e)
           (env-set env x ((interp-C-exp env) e))]
          [_ (error "interp-C-stmt unhandled" ast)])))

    (: interp-C [-> CProgram FinalAnswer])
    (define/public (interp-C ast)
      (debug "R1/interp-C" ast)
      (match ast
        [(CProgram _ `((start . ,t1) (,ls . ,ts) ...))
         ((interp-C-tail ((inst empty-env Fixnum))) t1)]
        [_ (error "no match in interp-C for " ast)]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; psuedo-x86 and x86
    ;; s,d ::= (var x) | (int n) | (reg r) | (deref r n)
    ;; i   ::= (movq s d) | (addq s d) | (subq s d) | (imulq s d)
    ;;       | (negq d) | (callq f)
    ;; psuedo-x86 ::= (program info i ...)

    ;; x86
    (: get-name [-> AST Symbol])
    (define/public (get-name ast)
      (match ast
        [(Var x) x]
        [(Reg x) x]
        [(Deref r n) (string->symbol (format "~a:~a" r n))]
        [_ (error 'interp-R1-class/get-name "doesn't have a name: ~a" ast)]))

    (field [x86-ops : (Immutable-HashTable Symbol (List Natural [-> Fixnum * Fixnum]))
                    ((inst hash Symbol (List Natural [-> Fixnum * Fixnum]))
                     'addq  (list 2 (dim-fixnum-binary fx+))
                     'imulq (list 2 (dim-fixnum-binary fx*))
                     'subq  (list 2 (dim-fixnum-binary fx-))
                     'negq  (list 1 (dim-fixnum-unary (curry (ann fx- (-> Zero Fixnum Fixnum)) 0))))])

    (: interp-x86-op [-> Symbol [-> Fixnum * Fixnum]])
    (define/public (interp-x86-op op)
      (cadr (hash-ref x86-ops op
                      (λ () (error 'interp-R1-class/interp-x86-op "unmatched ~a" op)))))


    (: interp-x86-exp [-> (Env Fixnum) [-> AST Fixnum]])
    (define/public (interp-x86-exp env)
      (lambda (ast)
        (copious "interp-x86-exp" ast)
        (define result
          (match ast
            [(Reg x)
             (env-ref env (get-name ast))]
            [(Deref r n)
             (env-ref env (get-name ast))]
            [(Imm n) n]
            [_ (error 'interp-R1-class/interp-x86-exp "unhandled ~a" ast)]))
        (copious "R1/interp-x86-exp" (observe-value result))
        result))

    (: interp-x86-instr [-> (Env Fixnum) [-> (Listof X86Instr) (Env Fixnum)]])
    (define/public (interp-x86-instr env)
      (lambda (ast)
        (when (pair? ast)
          (copious "R1/interp-x86-instr" (car ast)))
        (match ast
          ['() env]
          [(cons (Callq 'read_int _) ss)
           (let ([v (read-fixnum)])
             (copious "read " v)
             ((interp-x86-instr (env-set env 'rax v)) ss))]
          [(cons (Instr 'movq (list s d)) ss)
           (define x (get-name d))
           (define v ((interp-x86-exp env) s))
           (copious "move " (observe-value v))
           ((interp-x86-instr (env-set env x v)) ss)]
          [(cons (Jmp conclusion) ss)
           #:when (string-suffix? (symbol->string conclusion) "conclusion")
           env]
          [(cons (Jmp label) ss)
           ((interp-x86-block env) (goto-label label))]
          [(cons (Instr binary-op (list s d)) ss)
           (let ([s ((interp-x86-exp env) s)]
                 [d ((interp-x86-exp env) d)]
                 [x (get-name d)]
                 [f (interp-x86-op binary-op)])
             (let ([v (f d s)])
               (copious "binary-op result " (observe-value v))
               ((interp-x86-instr (env-set env x v)) ss)))]
          [(cons (Instr unary-op (list d)) ss)
           (let ([d ((interp-x86-exp env) d)]
                 [x (get-name d)]
                 [f (interp-x86-op unary-op)])
             (let ([v (f d)])
               (copious "unary-op result " (observe-value v))
               ((interp-x86-instr (env-set env x v)) ss)))]
          [_ (error "R1/interp-x86-instr no match for" ast)])))

    (: interp-x86-block [-> (Env Fixnum) [-> Block (Env Fixnum)]])
    (define/public (interp-x86-block env)
      (lambda (ast)
        (match ast
          [(Block info ss)
           ((interp-x86-instr env) ss)]
          [_ (error "R1/interp-x86-block unhandled" ast)])))

    (: interp-x86 [-> (Env Fixnum) [-> X86Program FinalAnswer]])
    (define/public (interp-x86 env)
      (lambda (ast)
        (when (pair? ast)
          (copious "R1/interp-x86" (car ast)))
        (match ast
          [(X86Program info G)
           (parameterize ([get-CFG G])
             (define start-block (cdar G))
             (define result-env ((interp-x86-block ((inst empty-env Fixnum))) start-block))
             (env-ref result-env 'rax))]
          [_ (error "R1/interp-x86 no match in for" ast)])))

    ;; pseudo x86
    (: pseudo-get-name [-> AST Symbol])
    (define/public (pseudo-get-name ast)
      (match ast
        [(Pseudo-Var x) x]
        [(Pseudo-Reg x) x]
        [(Pseudo-Deref r n) (string->symbol (format "~a:~a" r n))]
        [_ (error 'interp-R1-class/pseudo-get-name "doesn't have a name: ~a" ast)]))

    (: interp-pseudo-x86-op [-> Symbol [-> Fixnum * Fixnum]])
    (define/public (interp-pseudo-x86-op op)
      (cadr (hash-ref x86-ops op
                      (λ () (error 'interp-R1-class/interp-x86-op "unmatched ~a" op)))))

    (: interp-pseudo-x86-exp [-> (Env Fixnum) [-> AST Fixnum]])
    (define/public (interp-pseudo-x86-exp env)
      (lambda (ast)
        (copious "interp-pseudo-x86-exp" ast)
        (define result
          (match ast
            [(Pseudo-Var x)
             (env-ref env (pseudo-get-name ast))]
            [(Pseudo-Reg x)
             (env-ref env (pseudo-get-name ast))]
            [(Pseudo-Deref r n)
             (env-ref env (pseudo-get-name ast))]
            [(Pseudo-Imm n) n]
            [_ (error 'interp-R1-class/interp-pseudo-x86-exp "unhandled ~a" ast)]))
        (copious "R1/interp-pseudo-x86-exp" (observe-value result))
        result))

    (: interp-pseudo-x86-instr [-> (Env Fixnum) [-> (Listof Pseudo-X86Instr) (Env Fixnum)]])
    (define/public (interp-pseudo-x86-instr env)
      (lambda (ast)
        (when (pair? ast)
          (copious "R1/interp-pseudo-x86-instr" (car ast)))
        (match ast
          ['() env]
          [(cons (Pseudo-Callq 'read_int _) ss)
           (let ([v (read-fixnum)])
             (copious "read " v)
             ((interp-pseudo-x86-instr (env-set env 'rax v)) ss))]
          [(cons (Pseudo-Instr 'movq (list s d)) ss)
           (define x (pseudo-get-name d))
           (define v ((interp-pseudo-x86-exp env) s))
           (copious "move " (observe-value v))
           ((interp-pseudo-x86-instr (env-set env x v)) ss)]
          ;; [(cons (Pseudo-Jmp conclusion) ss)
          ;;  #:when (string-suffix? (symbol->string conclusion) "conclusion")
          ;;  env]
          ;; [(cons (Pseudo-Jmp label) ss)
          ;;  ((interp-pseudo-x86-block env) (pseudo-goto-label label))]
          [(cons (Pseudo-Jmp label) ss)
           (define block (pseudo-goto-label label))
           (if (false? block)
               env
               ((interp-pseudo-x86-block env) block))]
          [(cons (Pseudo-Instr binary-op (list s d)) ss)
           (let ([s ((interp-pseudo-x86-exp env) s)]
                 [d ((interp-pseudo-x86-exp env) d)]
                 [x (pseudo-get-name d)]
                 [f (interp-pseudo-x86-op binary-op)])
             (let ([v (f d s)])
               (copious "binary-op result " (observe-value v))
               ((interp-pseudo-x86-instr (env-set env x v)) ss)))]
          [(cons (Pseudo-Instr unary-op (list d)) ss)
           (let ([d ((interp-pseudo-x86-exp env) d)]
                 [x (pseudo-get-name d)]
                 [f (interp-pseudo-x86-op unary-op)])
             (let ([v (f d)])
               (copious "unary-op result " (observe-value v))
               ((interp-pseudo-x86-instr (env-set env x v)) ss)))]
          [_ (error "R1/interp-pseudo-x86-instr no match for" ast)])))

    (: interp-pseudo-x86-block [-> (Env Fixnum) [-> Pseudo-Block (Env Fixnum)]])
    (define/public (interp-pseudo-x86-block env)
      (lambda (ast)
        (match ast
          [(Pseudo-Block info ss)
           ((interp-pseudo-x86-instr env) ss)]
          [_ (error "R1/interp-pseudo-x86-block unhandled" ast)])))

    (: interp-pseudo-x86 [-> (Env Fixnum) [-> Pseudo-X86Program FinalAnswer]])
    (define/public (interp-pseudo-x86 env)
      (lambda (ast)
        (when (pair? ast)
          (copious "R1/interp-pseudo-x86" (car ast)))
        (match ast
          [(Pseudo-X86Program info G)
           (parameterize ([pseudo-get-CFG G])
             (define start-block (cdar G))
             (define result-env ((interp-pseudo-x86-block ((inst empty-env Fixnum))) start-block))
             (env-ref result-env 'rax))]
          [_ (error "R1/interp-pseudo-x86 no match in for" ast)])))


    )) ;; class interp-R1-class
