#lang nanopass

(provide Rint parse-Rint unparse-Rint
         Rvar parse-Rvar unparse-Rvar
         Cvar parse-Cvar unparse-Cvar)


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
   (_+ (+_))

   (_Program (Program_)))

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
