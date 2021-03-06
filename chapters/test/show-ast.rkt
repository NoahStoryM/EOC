#lang typed/racket

(require "../utilities.rkt" "../1/interp-Rint.rkt")


(show-ast (X86Program '()
                      `([_main . ,(Block '()
                                        (list (Pushq (Reg 'rbx))
                                              (Instr 'movq (list (Reg 'rsp) (Reg 'rbp)))
                                              (Instr 'subq (list (Imm 16) (Reg 'rsp)))
                                              (Jmp '_start)))]
                        [_start . ,(Block '()
                                         (list (Instr 'movq (list (Imm 10) (Deref 'rbp -8)))
                                               (Instr 'negq (list (Deref 'rbp -8)))
                                               (Instr 'movq (list (Deref 'rbp -8) (Reg 'rax)))
                                               (Instr 'addq (list (Imm 52) (Reg 'rax)))
                                               (Jmp '_conclusion)))]
                        [_conclusion . ,(Block '()
                                              (list (Instr 'addq (list (Imm 16) (Reg 'rsp)))
                                                    (Popq (Reg 'rbp))
                                                    (Retq)))])))
