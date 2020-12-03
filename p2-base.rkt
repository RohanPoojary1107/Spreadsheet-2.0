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
  [((list fn expr ...) typeenv) (let* ([fn-type (lookup fn typeenv)]
                                       [exp-arg-type (first fn-type)]
                                       [fn-return-type (second fn-type)])
                                  (if (check-func-valid exp-arg-type expr typeenv)
                                      fn-return-type
                                      'error))]
  
  [(expr typeenv) (cond             
                    ; Constants and Identifiers
                    [(number? expr) 'num]
                    [(string? expr) 'str]
                    [(boolean? expr) 'bool]   
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
(define (check-func-valid exp-arg-type args typeenv)
  (let ([actual-arg-type (foldl (lambda (arg acc) (append acc (list (typeof arg typeenv)))) '() args)])
    (equal? actual-arg-type exp-arg-type)))
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
   ((stringo expr)
    (== type 'str))
   ((boolo expr)
    (== type 'bool))
    
   ; identifier: symbolo is a miniKanren builtin relation
   ((symbolo expr)
    (lookupo expr env type))

   ; builtins

   ((fresh (builtin args rest e1 e2)
           (== expr (cons builtin args))
           (== args (cons e1 rest))
           
           (conde ((== rest '())
                   (conde ( (== builtin '!)
                            (typeo e1 env 'bool)
                            (== type 'bool) )
                          ((== builtin 'num->str)
                           (typeo e1 env 'num)
                           (== type 'str) )
                          ((== builtin 'len)
                           (typeo e1 env 'str)
                           (== type 'num)))
                   )
                  ((=/= rest '()) (== rest (cons e2 '()))
                                  (conde ((conde ((== builtin '+)) ((== builtin '-)) ((== builtin '*)) ((== builtin '/)))
                                          (typeo e1 env 'num)
                                          (typeo e2 env 'num)
                                          (== type 'num) )
                                         ((conde ((== builtin '>)) ((== builtin '=)) ((== builtin '>=)))
                                          (typeo e1 env 'num)
                                          (typeo e2 env 'num)
                                          (== type 'bool) )
                                         ((== builtin '++)
                                          (typeo e1 env 'str)
                                          (typeo e2 env 'str)
                                          (== type 'str))
                                         )))))
   ; Function calls
   ((fresh (fn args fn-typ fn-arg-type)
           (== expr (cons fn args))
           (typeo fn env fn-typ)
           (== fn-typ (list (cons fn-arg-type (cons type '()))))
           (type-listo args env type)))

   ; Function expression
   ((fresh (ids lmb args body arg-types zip-list newenv)
           (== expr (cons lmb args))
           (== lmb (list 'lambda ids body))
           (list-symbolo ids)
           (equal-lengtho ids args)
           (mapo args env arg-types)
           (zippo ids arg-types zip-list)
           (appendo env zip-list newenv)
           (typeo body newenv type)
           ))     
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
  (fresh (fkey fval rest)
         (== (cons (cons fkey fval) rest) alst)
         (conde ((== key fkey)
                 (== value fval))
                ((=/= key fkey)
                 (lookupo key rest value)))))


; Add your helper functions here
(define (boolo expr)
  (conde ((== expr #t))
         ((== expr #f))))

(define (type-listo args typeenv exp-types)
  (conde ((== args '()) (== exp-types '()))
         ((=/= args '()) (=/= exp-types '())
                         (fresh (farg rest-arg ftyp rest-typ)
                                (== args (cons farg rest-arg))
                                (== exp-types (cons ftyp rest-typ))
                                (== (list ftyp) (run 1 (out) (typeo farg typeenv out)))
                                (type-listo rest-arg typeenv rest-typ)))))

(define (appendo xs ys xsys)
  (conde ((== xs '())
          (== ys xsys))
         ((fresh (x xs^ xsys^)
                 (== xs (cons x xs^))
                 (== xsys (cons x xsys^))
                 (appendo xs^ ys xsys^)))))

(define (equal-lengtho l1 l2)
  (conde ((== l1 '())
          (== l2 '()))
         ((=/= l1 '())
          (=/= l2 '())
          (fresh (f-l1 r-l1 f-l2 r-l2 zip-list)
                 (== (cons f-l1 r-l1) l1)
                 (== (cons f-l2 r-l2) l2)
                 (equal-lengtho r-l1 r-l2)))))


(define (zippo l1 l2 output)
  (conde ((== l1 '())
          (== l2 '())
          (== output '()))
         ((fresh (f-l1 r-l1 f-l2 r-l2 r-output f-pair)
                 (== l1 (cons f-l1 r-l1))
                 (== l2 (cons f-l2 r-l2))
                 (== f-pair (cons f-l1 f-l2))
                 (== output (cons f-pair r-output))
                 (zippo r-l1 r-l2 r-output)))))

(define (mapo lst env flst)
  (conde ((== lst '())
          (== flst '()))
         ((fresh (f-lst r-lst r-flst f-lst-typ)
                 (== (cons f-lst r-lst) lst)
                 (typeo f-lst env f-lst-typ)
                 (== (cons f-lst-typ r-flst) flst)
                 (mapo r-lst env r-flst)
                 ))))

(define (list-symbolo lst)
  (conde ((== lst '()))
         ((fresh (f-lst r-lst)
                 (== lst (cons f-lst r-lst))
                 (symbolo f-lst)
                 (list-symbolo r-lst)))))











         











