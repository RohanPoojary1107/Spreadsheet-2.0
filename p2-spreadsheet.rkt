#lang racket #| CSC324 Fall 2020: Project 2 |#

; If you would like us to test this file with our correct implementation of
; "typeof" and "typeo", change the import to "p2-soln.rkt" before submitting
; your code.
(require "p2-base.rkt") ; (require "p2-soln.rkt")

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
         [env (getEnv (rest defs))]
         )
    env
    ;(type-check-cols (rest cols) env '())
    ))

(define (getEnv defs)
  (foldl (lambda (def acc) (append acc (list (cons (first def) (first (run 1 (out) (typeo (second def) acc out))))))) '() defs))

(define (type-check-cols cols env acc)
  (if (null? cols)
      acc
      (let* ([f-col (first cols)]
             [f-col-name (first f-col)]
             [f-col-typ (second f-col)]
             [r-col (rest cols)]
             [check-f-col (type-check-col (third f-col) f-col-typ env)]
             )
        (if check-f-col
            (type-check-cols r-col (append env (list (cons f-col-name f-col-typ))) (append acc (list check-f-col)))
            (type-check-cols r-col (append env (list (cons f-col-name 'error))) (append acc (list check-f-col))))
        )
      ))

(define/match (type-check-col col exp-typ env)
  [((list 'values vals ...) exp-typ env)
   (if (null? vals)
       #f
       (check-type vals exp-typ env))]
  [((list 'computed expr) exp-typ env)
   (let ([expr-typ (safe-first (run 1 (out) (typeo expr env out)))])
     (if (equal? expr-typ exp-typ)
         #t
         #f))]
  )

(define (safe-first lst)
  (if (null? lst)
      lst
      (first lst)))

(define (check-type args exp-typ env)
  (if (null? args)
      #t
      (let* ([f-arg (first args)]
             [r-arg (rest args)]
             [f-typ (safe-first (run 1 (out) (typeo f-arg env out)))]
             )
        (if (equal? f-typ exp-typ)
            (check-type r-arg exp-typ env)
            #f)))
  )
                

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
     (run n (lvar) (typeo expr '() type))
     ]))


