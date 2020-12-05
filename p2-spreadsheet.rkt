#lang racket #| CSC324 Fall 2020: Project 2 |#

; If you would like us to test this file with our correct implementation of
; "typeof" and "typeo", change the import to "p2-soln.rkt" before submitting
; your code.
(require "p2-base.rkt")
;(require "p2-soln.rkt")

(require "mk.rkt")
; (require racket/pretty) ; you might find the prettyprint function useful

(provide type-check-spreadsheet fill-in)

;-------------------------------------------------------------------------------
; * Task 3: Type Checking a Spreadsheet *
;-------------------------------------------------------------------------------

#|
(type-check-spreadsheet spreadsheet)
  spreadsheet: An spreadsheet AST described in the project handout.

  Returns a list of booleans, representing whether the types of each column
  is correctly annotated.
|#
(define (type-check-spreadsheet spreadsheet)
  (let* ([defs (second spreadsheet)]
         [cols (third spreadsheet)]
         [typeenv (get-env (rest defs))]
         )
    (type-check-cols (rest cols) typeenv '())))


#|
(get-env defs)
  defs: A list of definitions

  Returns an association list representing the type environment
  after all definitions have been type checked.
|#
(define (get-env defs)
  (if (null? defs)
      '()
      (foldl (lambda (def acc)
               (append acc (list (cons (first def) (first (run 1 (out) (typeo (second def) acc out)))))))
             '()
             defs)))


#|
(type-check-cols cols env acc)
  cols: A list of columns in the spreadsheet.
  env: An association list representing the type environment.
  acc: A list.

  Type checking each column in the speadsheet and returning
  returns a list of booleans representing whether the annotated
  type of each column is correct.
|#
(define (type-check-cols cols env acc)
  (if (null? cols)
      acc
      (let* ([fcol (first cols)]
             [fcol-name (first fcol)]
             [fcol-typ (second fcol)]
             [rcols (rest cols)]
             [check-fcol (type-check-col (third fcol) fcol-typ env)]
             )
        (if check-fcol
            (type-check-cols rcols (append env (list (cons fcol-name fcol-typ))) (append acc (list check-fcol)))
            (type-check-cols rcols (append env (list (cons fcol-name 'error))) (append acc (list check-fcol)))))))


#|
(type-check-col col exp-typ env)
  col: A list of columns in the spreadsheet.
  exp-typ: The annotated type of col.
  env: An association list representing the type environment.

  Check annotated of given column is correct. 
|#
(define/match (type-check-col col exp-col-typ env)
  [((list 'values vals ...) exp-col-typ env)  ; If col is a value column
   (if (null? vals)
       #f
       (check-type vals exp-col-typ env))]
  [((list 'computed expr) exp-col-typ env)  ; If col is a computed column
   (let ([expr-typ (safe-first (run 1 (out) (typeo expr env out)))])
     (if (equal? expr-typ exp-col-typ)
         #t
         #f))])


#|
(check-type args exp-typ env)
  col: A value column.
  exp-typ: The annotated type of col.
  env: An association list representing the type environment.

  Checks if the type of each value in col matches the
  annoted type of col. 
|#
(define (check-type args exp-col-typ env)
  (if (null? args)
      #t
      (let* ([f-arg (first args)]
             [r-arg (rest args)]
             [f-arg-typ (safe-first (run 1 (out) (typeo f-arg env out)))]
             )
        (if (equal? f-arg-typ exp-col-typ)
            (check-type r-arg exp-col-typ env)
            #f))))


#|
(safe-first args lst)
  lst: A list.

  A safe version of built-in function `first`
|#
(define (safe-first lst)
  (if (null? lst)
      lst
      (first lst)))
                
;-------------------------------------------------------------------------------
; * Task 4: Synthesizing Programs *
;-------------------------------------------------------------------------------

#|
(fill-in lvar expr type n)
  lvar: The logic variable to be filled in
  expr: An expression following the spreadsheet grammar, with a logic variable
        somewhere in the expression to be filled in.
  type: The desired type of the expression after BLANK is filled in.
  n:    The maximum number of results to return.

  Macro that runs a miniKanren query that will replace the symbol `BLANK`
  in the spreadsheet expression `expr` so that the type of the expression
  is consistent with `type`. The query returns at most `n` results.
|#

(define-syntax fill-in
  (syntax-rules ()
    [(fill-in lvar expr type n)
     (run n (lvar) (typeo expr '() type))]))


