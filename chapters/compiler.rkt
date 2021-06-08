#lang typed/racket

(require racket/fixnum)
(require "utilities.rkt"
         "type-check.rkt"
         "2/interp-Rvar.rkt")

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
    [(Prim 'read '()) e]
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
    [(Int n) e]
    [(Var n) e]
    [(Prim 'read '()) e]
    [(Prim '- (list e1)) (pe-neg (pe-exp e1))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]
    [(Let x e body) (Let x (pe-exp e) (pe-exp body))]))

(: pe-Rvar [-> Program Program])
(define (pe-Rvar p)
  (match p
    [(Program info e) (type-checker (Program info (pe-exp e)))]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HW1 Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; uniquify : R1 -> R1
(: uniquify [-> Program Program])
(define (uniquify p)
  (: uniquify-exp [-> (Env Symbol) [-> Exp Exp]])
  (define (uniquify-exp env)
    (lambda (e)
      (match e
        [(Var x) (Var (env-ref env x))]
        [(Int n) (Int n)]
        [(Let x e body)
         (define new-name (gensym x))
         (define new-env (env-set env x new-name))
         (Let new-name ((uniquify-exp env) e)
              ((uniquify-exp new-env) body))]
        [(Prim op es)
         (Prim op (for/list ([e es]) ((uniquify-exp env) e)))])))

  (match p
    [(Program info e) (type-checker (Program info ((uniquify-exp ((inst empty-env Symbol))) e)))]))

;; remove-complex-opera* : R1 -> R1
(: remove-complex-opera* [-> Program Program])
(define (remove-complex-opera* p)
  (define-type Exp-Cont [-> Exp Exp])
  (define-type Args-Cont [-> (Listof Exp) Exp])

  (: id Exp-Cont)
  (define id (λ (arg) arg))

  (: rco-args [-> (Listof Exp) Args-Cont Exp])
  (define rco-args
    (λ (es cont)
      (cond [(null? es) (cont es)]
            [else
             (define e (car es))
             (if (Atm? e)
                 (rco-args (cdr es)
                           (ann (λ (es1) (cont (cons e es1)))
                                Args-Cont))
                 (rco-exp e
                          (ann (λ (e0)
                                 (define new-name (gensym))
                                 (match e0
                                   [(Let ex ee (Var ex)) (set! e0 ee)]
                                   [_ (void)])

                                 (Let new-name e0
                                      (rco-args (cdr es)
                                                (ann (λ (es1) (cont (cons (Var new-name) es1)))
                                                     Args-Cont))))
                               Exp-Cont)))])))

  (: rco-exp [-> Exp Exp-Cont Exp])
  (define rco-exp
    (λ (exp cont)
      (match exp
        [(Int x) (cont exp)]
        [(Var v) (cont exp)]

        [(Let x e body)
         (match e
           [(? Atm?) (Let x e (rco-exp body (ann (λ (b0) (cont b0)) Exp-Cont)))]
           [(Let ex ee (Var ex)) (rco-exp (Let x ee body) cont)]
           [_ (rco-exp e (ann (λ (e0) (Let x e0 (rco-exp body cont))) Exp-Cont))])]
        [(Prim op es)
         (if (andmap Atm? es)
             (cont exp)
             (rco-args es (ann (λ (es0) (cont (Prim op es0))) Args-Cont)))])))

  (match p
    [(Program info e) (type-checker (Program info (rco-exp e id)))]))


;; explicate-control : R1 -> C0
(: explicate-control [-> Program CProgram])
(define (explicate-control p)
  (: insert-cont [-> Symbol Tail Tail Tail])
  (define insert-cont
    (λ (x rt cont)
      (match rt
        [(Return exp) (Seq (Assign (Var x) exp) cont)]
        [(Seq stmt tail) (Seq stmt (insert-cont x tail cont))])))

  (: explicate-assign [-> Exp Symbol Tail Tail])
  (define explicate-assign
    (λ (e x cont)
      (match e
        [(Let y rhs body) (explicate-assign rhs y (insert-cont x (explicate-tail body) cont))]
        [_ (Seq (Assign (Var x) e) cont)])))

  (: explicate-tail [-> Exp Tail])
  (define explicate-tail
    (λ (e)
      (match e
        [(Let x rhs body) (explicate-assign rhs x (explicate-tail body))]
        [_ #:when (or (Atm? e) (Prim? e)) (Return e)])))


  (match p
    [(Program info body)
     (type-checker (CProgram info (list (cons 'start (explicate-tail body)))))]))


;; select-instructions : C0 -> pseudo-x86
(: select-instructions [-> CProgram Pseudo-X86Program])
(define (select-instructions p)
  (: select-instr-atm [-> Atm (U Pseudo-Imm Pseudo-X86Arg)])
  (define select-instr-atm
    (λ (atm)
      (match atm
        [(Int x) (Pseudo-Imm x)]
        [(Var v) (if (X86Reg? v) (Pseudo-Reg v) (Pseudo-Var v))])))

  (: select-instr-stmt [-> Assign (Listof Pseudo-X86Instr)])
  (define select-instr-stmt
    (λ (stmt)
      (define var (Assign-var stmt))
      (define exp (Assign-exp stmt))

      (match exp
        [(Int x) (list (Pseudo-Instr 'movq (list (select-instr-atm exp) (select-instr-atm var))))]
        [(Var x) (list (Pseudo-Instr 'movq (list (select-instr-atm exp) (select-instr-atm var))))]
        [(Prim '+ arg*)
         (match arg*
           [(list (? Var? arg1) (? Atm? arg2))
            #:when (eq? (Var-name arg1) (Var-name var))
            (list (Pseudo-Instr 'addq (list (atm->pseudo-x86arg arg2) (select-instr-atm var))))]
           [(list (? Atm? arg1) (? Var? arg2))
            #:when (eq? (Var-name arg2) (Var-name var))
            (list (Pseudo-Instr 'addq (list (atm->pseudo-x86arg arg1) (select-instr-atm var))))]
           [(list (? Atm? arg1) (? Atm? arg2))
            (list (Pseudo-Instr 'movq (list (atm->pseudo-x86arg arg1) (select-instr-atm var)))
                  (Pseudo-Instr 'addq (list (atm->pseudo-x86arg arg2) (select-instr-atm var))))])]
        [(Prim '- arg*)
         (match arg*
           [(list (? Var? arg1))
            #:when (eq? (Var-name arg1) (Var-name var))
            (list (Pseudo-Instr 'negq (list (select-instr-atm var))))]
           [(list (? Atm? arg1))
            (list (Pseudo-Instr 'movq (list (atm->pseudo-x86arg arg1) (select-instr-atm var)))
                  (Pseudo-Instr 'negq (list (select-instr-atm var))))])]
        [(Prim 'read arg*)
         (list (Pseudo-Callq 'read_int 1)
               (Pseudo-Instr 'movq (list (Pseudo-Reg 'rax) (select-instr-atm var))))])))

  (: select-instr-tail [-> Tail (Listof Pseudo-X86Instr)])
  (define select-instr-tail
    (λ (tail)
      (match tail
        [(Seq stmt tail) (append (select-instr-stmt stmt) (select-instr-tail tail))]
        [(Return (Prim 'read '())) (list (Pseudo-Callq 'read_int 1) (Pseudo-Jmp 'conclusion))]
        [(Return exp) (append (select-instr-stmt (Assign (Var 'rax) exp)) (list (Pseudo-Jmp 'conclusion)))])))

  (match p
    [(CProgram info body)
     (type-checker
      (Pseudo-X86Program info
                         (map (ann (λ (p) (cons (car p) (Pseudo-Block '() (select-instr-tail (cdr p)))))
                                   [-> (Pair Symbol Tail) (Pair Symbol Pseudo-Block)])
                              body)))]))

;; assign-homes : pseudo-x86 -> x86
(: assign-homes [-> Pseudo-X86Program X86Program])
(define (assign-homes p)
  (: type-stack-env (Env Fixnum))
  (define type-stack-env
    (env-set* ((inst empty-env Fixnum))
              'Integer 8))


  (: assign-homes-arg [-> Pseudo-X86Arg (Listof (Pair Symbol Fixnum)) X86Arg])
  (define assign-homes-arg
    (λ (arg pos-info)
      (match arg
        [(Pseudo-Var v)
         (Deref 'rbp
                 (cdr (let ([val (assq v pos-info)])
                        (if (false? val)
                            (error 'assign-homes-arg "invalid variable")
                            val))))]
        [(Pseudo-Imm i) (Imm i)]
        [(Pseudo-Reg r) (Reg r)]
        [(Pseudo-Deref r offset) (Deref r offset)])))

  (: assign-homes-instr [-> Pseudo-X86Instr (Listof (Pair Symbol Fixnum)) X86Instr])
  (define assign-homes-instr
    (λ (instr pos-info)
      (match instr
        [(Pseudo-Instr name arg*) (Instr name (for/list ([arg arg*]) (assign-homes-arg arg pos-info)))]
        [(Pseudo-Pushq arg) (Pushq (assign-homes-arg arg pos-info))]
        [(Pseudo-Popq arg) (Popq (assign-homes-arg arg pos-info))]
        [(Pseudo-Callq target arity) (Callq target arity)]
        [(Pseudo-Retq) (Retq)]
        [(Pseudo-Jmp target) (Jmp target)])))

  (: assign-homes-block [-> Pseudo-Block (Listof (Pair Symbol Fixnum)) Block])
  (define assign-homes-block
    (λ (block pos-info)
      (match block
        [(Pseudo-Block info instr*)
         (Block info (for/list ([instr instr*]) (assign-homes-instr instr pos-info)))])))

  (match p
    [(Pseudo-X86Program info body)
     (define-predicate types-info? (Pair 'locals-types (Listof (Pair Symbol Symbol))))

     (: types-info (Pair 'locals-types (Listof (Pair Symbol Symbol))))
     (define types-info
       (let ([val (assq 'locals-types info)])
         (if (types-info? val)
             val
             (error 'assign-homes "expected a info: locals-types"))))

     (: top Fixnum)
     (define top 0)

     (: pos-info (Listof (Pair Symbol Fixnum)))
     (define pos-info
       (for/list ([p (cdr types-info)])
         (define var  (car p))
         (define type (cdr p))

         (set! top (fx- top (env-ref type-stack-env type)))
         (cons var top)))

     (X86Program (cons (cons 'stack-space (abs top))
                       (filter (ann (λ (i) (not (eq? (car i) 'locals-types))) [-> (Pair Symbol Any) Boolean])
                               info))
                 (for/list ([b body]) (cons (car b) (assign-homes-block (cdr b) pos-info))))]))

;; patch-instructions : x86 -> x86
(: patch-instructions [-> X86Program X86Program])
(define (patch-instructions p)
  (: patch-instr [-> X86Instr (Listof X86Instr)])
  (define patch-instr
    (λ (instr)
      (match instr
        [(Instr name (list (? Deref? d1) (? Deref? d2)))
         (list (Instr 'movq (list d1 (Reg 'rax)))
               (Instr name  (list (Reg 'rax) d2)))]
        [_ (list instr)])))

  (: patch-block [-> Block Block])
  (define patch-block
    (λ (block)
      (match block
        [(Block info instr*)
         (Block info (append-map patch-instr instr*))])))

  (match p
    [(X86Program info body)
     (X86Program info (map (ann (λ (p) (cons (car p) (patch-block (cdr p))))
                                [-> (Pair Symbol Block) (Pair Symbol Block)])
                           body))]))

;; prepare-x86 : x86 -> x86
(: prepare-x86 [-> X86Program X86Program])
(define (prepare-x86 p)
  (match p
    [(X86Program info body)
     (define-predicate stack-info? (Pair 'stack-space Fixnum))

     (: stack-space Fixnum)
     (define stack-space
       (let ([val (assq 'stack-space info)])
         (if (stack-info? val)
             (cdr val)
             (error 'print-x86 "expected a info: stack-space"))))

     (X86Program '()
                  (list* (cons 'main
                                (Block '()
                                        (list (Pushq (Reg 'rbx))
                                              (Instr 'movq (list (Reg 'rsp) (Reg 'rbp)))
                                              (Instr 'subq (list (Imm stack-space) (Reg 'rsp)))
                                              (Jmp 'start))))
                         (cons 'conclusion
                                (Block '()
                                        (list (Instr 'addq (list (Imm stack-space) (Reg 'rsp)))
                                              (Popq (Reg 'rbp))
                                              (Retq)
                                              ;; (Jmp 'exit)
                                              )))
                         (cons 'exit
                                (Block '()
                                        (list (Instr 'movq (list (Imm 0) (Reg 'rbx)))
                                              (Instr 'movq (list (Imm 1) (Reg 'rax)))
                                              (Instr 'int (list (Imm #x80))))))
                         body))]))

;; print-x86 : x86 -> string
(: print-x86 [-> X86Program String])
(define (print-x86 p)
  (parameterize ([port-count-lines-enabled #t])
    (define out (open-output-string))
    (write-ast p out)
    (get-output-string out)))
