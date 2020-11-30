#lang racket #| CSC324 Fall 2020: Project 2 |#

(require "mk.rkt")
; (require racket/pretty) ; you might find the prettyprint function useful

(provide typeof typeo)

;-------------------------------------------------------------------------------
; * Task 1: A Simple Type Inferencer *
;-------------------------------------------------------------------------------

#|
(typeof expr typeenv)
  expr: An expression following the spreadsheet grammar
  typeenv: An association list containing the mapping of identifiers to types

  Returns the type of the expression expr: 'num, 'str, 'bool, 'error
|#
(define/match (typeof expr typeenv)
  
  
  ; Builtins
  [((list '+ e1 e2) typeenv) (if (and (equal? (typeof e1 typeenv) 'num) (equal? (typeof e2 typeenv) 'num)) 'num 'error)]
  [((list '- e1 e2) typeenv) (if (and (equal? (typeof e1 typeenv) 'num) (equal? (typeof e2 typeenv) 'num)) 'num 'error)]
  [((list '* e1 e2) typeenv) (if (and (equal? (typeof e1 typeenv) 'num) (equal? (typeof e2 typeenv) 'num)) 'num 'error)]
  [((list '/ e1 e2) typeenv) (if (and (equal? (typeof e1 typeenv) 'num) (equal? (typeof e2 typeenv) 'num)) 'num 'error)]
  [((list '> e1 e2) typeenv) (if (and (equal? (typeof e1 typeenv) 'num) (equal? (typeof e2 typeenv) 'num)) 'bool 'error)]
  [((list '= e1 e2) typeenv) (if (and (equal? (typeof e1 typeenv) 'num) (equal? (typeof e2 typeenv) 'num)) 'bool 'error)]
  [((list '>= e1 e2) typeenv) (if (and (equal? (typeof e1 typeenv) 'num) (equal? (typeof e2 typeenv) 'num)) 'bool 'error)]
  [((list '++ e1 e2) typeenv) (if (and (equal? (typeof e1 typeenv) 'str) (equal? (typeof e2 typeenv) 'str)) 'str 'error)]
  [((list '! e1) typeenv) (if (equal? (typeof e1 typeenv) 'bool) 'bool 'error)]
  [((list 'num->str e1) typeenv) (if (equal? (typeof e1 typeenv) 'num) 'str 'error)]
  [((list 'len e1) typeenv) (if (equal? (typeof e1 typeenv) 'str) 'num 'error)]
    
    
    

    ; Function Calls
  [((list func arg ...) typeenv) expr]
  
  [(expr typeenv) (cond
                    
    ; Constants
    [(number? expr) 'num]
    [(string? expr) 'str]
    [(boolean? expr) 'bool]
    ;[(list? expr) expr]    
    ; Identifiers
    [(symbol? expr) (let ([val (lookup expr typeenv)])
                          (if (equal? val #f) 'error val))])]
  )

; Helper functions for Task 1

#|
(lookup key alst)
  elem: A key in the association list
  alst: An association list 

  Returns the value corresponding to the first time the key appears in the
  association list, or #f if the key does not exist.

  Examples:
  > (lookup 'b '((a . 3) (b . 4)))
  4
  > (lookup 'b '((a . 3) (b . 4) (b . 5)))
  4
  > (lookup 'c '((a . 3) (b . 4) (b . 5)))
  #f
|#
(define (lookup key alst)
  (if (null? alst) #f
      (let* ([first (first alst)]
            [rest (rest alst)]
            [first-key (car first)]
            [first-value (cdr first)])
        (if (equal? first-key key) first-value (lookup key rest))
        ) 
      ))

; Add your helper functions here

;-------------------------------------------------------------------------------
; * Task 2: A Type Inferencer Relation in miniKanren
;-------------------------------------------------------------------------------

#|
(typeo expr typeenv type)
  expr: An expression following the spreadsheet grammar
  typeenv: An association list containing the mapping of identifiers to types
  type: The type of the expression

  The relational form of the `typeof` function
|#
(define (typeo expr env type)
  (conde
    ; constants: numbero, stringo, and boolo are miniKanren builtin relations
    ((numbero expr)
     (== type 'num))
    ; TODO

    ; identifier: symbolo is a miniKanren builtin relation
    ; TODO

    ; builtins
    ; TODO

    ; function calls
    ; TODO

    ; function definitions
    ; TODO
    ))


; Helper functions for Task 2

#|
(lookupo key alst value)
  elem: A key in the association list
  alst: An association list 
  value: The corresponding value in the association list

  The relational form of the `lookup` function
|#
(define (lookupo key alst value)
  (void))


; Add your helper functions here

