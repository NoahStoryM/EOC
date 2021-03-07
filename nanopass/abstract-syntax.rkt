#lang nanopass

(provide Rint parse-Rint unparse-Rint
         Rvar parse-Rvar unparse-Rvar
         Cvar parse-Cvar unparse-Cvar
         X86Int parse-X86Int unparse-X86Int)


;; Rint
(define _info? (procedure-rename list? '_info?))
(define _int? (procedure-rename fixnum? '_int?))

(define _list? (λ (arg) (eq? arg 'list)))

(define _Int?     (λ (arg) (eq? arg 'Int)))
(define _Prim?    (λ (arg) (eq? arg 'Prim)))
(define _Program? (λ (arg) (eq? arg 'Program)))

(define _read? (λ (arg) (equal? arg ''read)))
(define _-?    (λ (arg) (equal? arg ''-)))
(define _+?    (λ (arg) (equal? arg ''+)))

(define-language Rint
  (entry LP)
  (terminals
   (_info (info_))
   (_int  (int_))
   (_list (list_))

   (_Int  (Int_))
   (_Prim (Prim_))

   (_read (read_))
   (_- (-_))
   (_+ (+_)))

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
(define _var?
  (λ (arg)
    (match arg
      [`(quote ,(? (conjoin (negate prim?) symbol?))) #t]
      [_ #f])))
(define _Var? (λ (arg) (eq? arg 'Var)))
(define _Let? (λ (arg) (eq? arg 'Let)))

(define-language Rvar
  (extends Rint)
  (entry LP)
  (terminals
   (+ (_var (var_))
      (_Var (Var_))
      (_Let (Let_))))
  (Exp (exp body)
       (+ (Let var_ exp body)
          (Var var_))))
(define-parser parse-Rvar Rvar)


;; Cvar
(define _label?
  (λ (arg)
    (match arg
      [`(quote ,(? symbol?)) #t]
      [_ #f])))

(define-language Cvar
  (extends Rvar)
  (entry LP)

  (terminals (+ (_label (label_))))

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
(define _reg?
  (λ (arg)
    (member arg '('rsp 'rbp 'rax 'rbx 'rcx 'rdx 'rsi 'rdi
                       'r8 'r9 'r10 'r11 'r12 'r13 'r14 'r15))))

(define _Imm?   (λ (arg) (eq? arg 'Imm)))
(define _Reg?   (λ (arg) (eq? arg 'Reg)))
(define _Deref? (λ (arg) (eq? arg 'Deref)))
(define _Instr? (λ (arg) (eq? arg 'Instr)))

(define _nullary?   (λ (arg) (member arg '())))
(define _unary?  (λ (arg) (member arg '('negq))))
(define _binary? (λ (arg) (member arg '('addq 'subq 'movq))))
(define _n-ary?  (λ (arg) (member arg '())))

(define _Callq? (λ (arg) (eq? arg 'Callq)))
(define _Retq?  (λ (arg) (eq? arg 'Retq)))
(define _Pushq? (λ (arg) (eq? arg 'Pushq)))
(define _Popq?  (λ (arg) (eq? arg 'Popq)))
(define _Jmp?   (λ (arg) (eq? arg 'Jmp)))

(define-language X86Int
  (entry LP)
  (terminals
   (_info (info_))
   (_int  (int_))
   (_reg  (reg_))
   (_list (list_))
   (_label (label_))

   (_Imm   (Imm_))
   (_Reg   (Reg_))
   (_Deref (Deref_))

   (_Instr   (Instr_))
   (_nullary (nullary_))
   (_unary   (unary_))
   (_binary  (binary_))
   (_n-ary   (n-ary_))

   (_Callq (Callq_))
   (_Retq  (Retq_))
   (_Pushq (Pushq_))
   (_Popq  (Popq_))
   (_Jmp   (Jmp_)))

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
