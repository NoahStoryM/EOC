#lang nanopass

(require (rename-in racket/base
                    [list?   info?]
                    [fixnum? int?]))

(provide Rint parse-Rint unparse-Rint
         Rvar parse-Rvar unparse-Rvar
         Cvar parse-Cvar unparse-Cvar
         X86Int parse-X86Int unparse-X86Int)


;; Rint
(define list? (λ (arg) (eq? arg 'list)))

(define Int?  (λ (arg) (eq? arg 'Int)))
(define Prim? (λ (arg) (eq? arg 'Prim)))

(define read? (λ (arg) (equal? arg ''read)))
(define -?    (λ (arg) (equal? arg ''-)))
(define +?    (λ (arg) (equal? arg ''+)))

(define-language Rint
  (entry LP)
  (terminals
   (info (info_))
   (int  (int_))
   (list (list_))

   (Int  (Int_))
   (Prim (Prim_))

   (read (read_))
   (- (-_))
   (+ (+_)))

  (Exp (exp body)
       (Int int_)
       (Prim read_ (list_))
       (Prim -_ (list_ exp))
       (Prim +_ (list_ exp1 exp2)))
  (LP (prog)
      (Program info_ body)))
(define-parser parse-Rint Rint)


;; Rvar
(define prim? (λ (arg) (memq arg '(+ - read))))
(define var?
  (λ (arg)
    (match arg
      [`(quote ,(? (conjoin (negate prim?) symbol?))) #t]
      [_ #f])))
(define Var? (λ (arg) (eq? arg 'Var)))
(define Let? (λ (arg) (eq? arg 'Let)))

(define-language Rvar
  (extends Rint)
  (entry LP)
  (terminals
   (+ (var (var_))
      (Var (Var_))
      (Let (Let_))))
  (Exp (exp body)
       (+ (Let var_ exp body)
          (Var var_))))
(define-parser parse-Rvar Rvar)


;; Cvar
(define label?
  (λ (arg)
    (match arg
      [`(quote ,(? symbol?)) #t]
      [_ #f])))

(define-language Cvar
  (extends Rvar)
  (entry LP)

  (terminals (+ (label (label_))))

  (Atm (atm)
       (+ (Int int_)
          (Var var_)))

  (Exp (exp body)
       (- (Prim -_ (list_ exp))
          (Prim +_ (list_ exp1 exp2))
          (Let var_ exp body))
       (+ atm
          (Prim -_ (list_ atm))
          (Prim +_ (list_ atm1 atm2))))

  (Assign (stmt) (+ (Assign (Var_ var_) exp)))

  (Tail (tail)
        (+ (Return exp)
           (Seq stmt tail)))

  (LP (prog)
      (- (Program info_ body))
      (+ (CProgram info_ (list_ [label_ . tail] [label_* . tail*] ...)))))
(define-parser parse-Cvar Cvar)


;; X86Int
(define reg?
  (λ (arg)
    (member arg '('rsp 'rbp 'rax 'rbx 'rcx 'rdx 'rsi 'rdi
                       'r8 'r9 'r10 'r11 'r12 'r13 'r14 'r15))))

(define Imm?   (λ (arg) (eq? arg 'Imm)))
(define Reg?   (λ (arg) (eq? arg 'Reg)))
(define Deref? (λ (arg) (eq? arg 'Deref)))
(define Instr? (λ (arg) (eq? arg 'Instr)))

(define nullary?   (λ (arg) (member arg '())))
(define unary?  (λ (arg) (member arg '('negq))))
(define binary? (λ (arg) (member arg '('addq 'subq 'movq))))
(define n-ary?  (λ (arg) (member arg '())))

(define Callq? (λ (arg) (eq? arg 'Callq)))
(define Retq?  (λ (arg) (eq? arg 'Retq)))
(define Pushq? (λ (arg) (eq? arg 'Pushq)))
(define Popq?  (λ (arg) (eq? arg 'Popq)))
(define Jmp?   (λ (arg) (eq? arg 'Jmp)))

(define-language X86Int
  (entry LP)
  (terminals
   (info (info_))
   (int  (int_))
   (reg  (reg_))
   (list (list_))
   (label (label_))

   (Imm   (Imm_))
   (Reg   (Reg_))
   (Deref (Deref_))

   (Instr   (Instr_))
   (nullary (nullary_))
   (unary   (unary_))
   (binary  (binary_))
   (n-ary   (n-ary_))

   (Callq (Callq_))
   (Retq  (Retq_))
   (Pushq (Pushq_))
   (Popq  (Popq_))
   (Jmp   (Jmp_)))

  (X86Arg (arg)
          (Imm int_)
          (Reg reg_)
          (Deref reg_ int_))
  (X86Instr (instr)
            (Instr nullary_ (list_))
            (Instr unary_   (list_ arg))
            (Instr binary_  (list_ arg1 arg2))
            (Instr n-ary_   (list_ arg1 arg2 arg* ...))

            (Callq label_ int_)
            (Retq)
            (Pushq arg)
            (Popq arg)
            (Jmp label_))

  (X86Block (block)
            (Block info_ (list_ instr instr* ...)))

  (LP (prog)
      (X86Program info_ (list_ [label_ . block] [label_* . block*] ...))))
(define-parser parse-X86Int X86Int)
