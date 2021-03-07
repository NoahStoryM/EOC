#lang nanopass

(require "abstract-syntax.rkt")


;; Rint
(parse-Rint '(Program '() (Int 123)))
(parse-Rint '(Program (list) (Int 123)))
(parse-Rint '(Program '() (Prim '- (list (Int 123)))))


;; Rvar
(parse-Rvar '(Program '() (Int 123)))
(parse-Rvar '(Program '() (Var 'v)))
(parse-Rvar '(Program '()
                      (Let 'x (Int 22)
                           (Let 'y (Int 11)
                                (Prim '+ (list (Int 432) (Int 123)))))))


;; Cvar
(parse-Cvar '(CProgram '()
                       (list ['_main . (Return (Int 123))]
                             ['_last . (Seq (Assign (Var 'v) (Int 12))
                                            (Return (Prim '+ (list (Int 31) (Var 'v)))))])))


;; X86Int
(parse-X86Int '(X86Program '()
                           (list ['_main . (Block '()
                                                  (list (Pushq (Reg 'rbx))
                                                        (Instr 'movq (list (Reg 'rsp) (Reg 'rbp)))
                                                        (Instr 'addq (list (Imm 16) (Reg 'rsp)))
                                                        (Jmp '_start)))]
                                 ['_start . (Block '()
                                                   (list (Instr 'movq (list (Imm 10) (Deref 'rbp -8)))
                                                         (Instr 'negq (list (Deref 'rbp -8)))
                                                         (Instr 'movq (list (Deref 'rbp -8) (Reg 'rax)))
                                                         (Instr 'addq (list (Imm 52) (Reg 'rax)))
                                                         (Jmp '_conclusion)))]
                                 ['_conclusion . (Block '()
                                                        (list (Instr 'addq (list (Imm 16) (Reg 'rsp)))
                                                              (Popq (Reg 'rbp))
                                                              (Retq)))])))
