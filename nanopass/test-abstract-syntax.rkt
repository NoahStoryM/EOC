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
                                            (Return (Var 'v)))])))