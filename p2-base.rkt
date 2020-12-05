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
  [((list '+ args ...) typeenv) (if (and (equal? 2 (length args))
                                         (equal? (typeof (first args) typeenv) 'num)
                                         (equal? (typeof (second args) typeenv) 'num))
                                    'num
                                    'error)]
  [((list '- args ...) typeenv) (if (and (equal? 2 (length args))
                                         (equal? (typeof (first args) typeenv) 'num)
                                         (equal? (typeof (second args) typeenv) 'num))
                                    'num
                                    'error)]
  [((list '* args ...) typeenv) (if (and (equal? 2 (length args))
                                         (equal? (typeof (first args) typeenv) 'num)
                                         (equal? (typeof (second args) typeenv) 'num))
                                    'num
                                    'error)]
  [((list '/ args ...) typeenv) (if (and (equal? 2 (length args))
                                         (equal? (typeof (first args) typeenv) 'num)
                                         (equal? (typeof (second args) typeenv) 'num))
                                    'num
                                    'error)]
  [((list '> args ...) typeenv) (if (and (equal? 2 (length args))
                                         (equal? (typeof (first args) typeenv) 'num)
                                         (equal? (typeof (second args) typeenv) 'num))
                                    'bool
                                    'error)]
  [((list '= args ...) typeenv) (if (and (equal? 2 (length args))
                                         (equal? (typeof (first args) typeenv) 'num)
                                         (equal? (typeof (second args) typeenv) 'num))
                                    'bool
                                    'error)]
  [((list '>= args ...) typeenv) (if (and (equal? 2 (length args))
                                          (equal? (typeof (first args) typeenv) 'num)
                                          (equal? (typeof (second args) typeenv) 'num))
                                     'bool
                                     'error)]
  [((list '++ args ...) typeenv) (if (and (equal? 2 (length args))
                                          (equal? (typeof (first args) typeenv) 'str)
                                          (equal? (typeof (second args) typeenv) 'str))
                                     'str
                                     'error)]
  [((list '! args ...) typeenv) (if (and (equal? 1 (length args))
                                         (equal? (typeof (first args) typeenv) 'bool))
                                    'bool
                                    'error)]
  [((list 'num->str args ...) typeenv) (if (and (equal? 1 (length args))
                                                (equal? (typeof (first args) typeenv) 'num))
                                           'str
                                           'error)]
  [((list 'len args ...) typeenv) (if (and (equal? 1 (length args))
                                           (equal? (typeof (first args) typeenv) 'str))
                                      'num 'error)]
    
  ; Function Calls
  [((list fn args ...) typeenv) (let* ([fn-type (lookup fn typeenv)]
                                       [exp-arg-types (first fn-type)]
                                       [fn-return-type (second fn-type)])
                                  (if (check-func-valid exp-arg-types args typeenv)
                                      fn-return-type
                                      'error))]
  ; Constants and Identifiers
  [(expr typeenv) (cond             
                    [(number? expr) 'num]
                    [(string? expr) 'str]
                    [(boolean? expr) 'bool]   
                    [(symbol? expr) (let ([expr-typ (lookup expr typeenv)])
                                      (if (equal? expr-typ #f)
                                          'error
                                          expr-typ))])]
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
  (if (null? alst)
      #f
      (let* ([first (first alst)]
             [rest (rest alst)]
             [first-key (car first)]
             [first-value (cdr first)])
        (if (equal? first-key key)
            first-value
            (lookup key rest)))))

#|
(check-func-valid exp-arg-types args typeenv)
  exp-arg-types: A list
  args: A list
  typeenv: An association list 

  Checks if type of every element in args is equal to the type
  expected.
|#
(define (check-func-valid exp-arg-types args typeenv)
  (let ([actual-arg-types (foldl (lambda (arg acc)
                                   (append acc (list (typeof arg typeenv))))
                                 '()
                                 args)])
    (equal? actual-arg-types exp-arg-types)))

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
   ((fresh (builtin args rest arg1 arg2)
           (== expr (cons builtin args))
           (== args (cons arg1 rest))
           (conde ((== rest '())                  ; If only one argument was passed.
                   (conde ((== builtin '!)
                           (typeo arg1 env 'bool)
                           (== type 'bool) )
                          ((== builtin 'num->str)
                           (typeo arg1 env 'num)
                           (== type 'str) )
                          ((== builtin 'len)
                           (typeo arg1 env 'str)
                           (== type 'num)))
                   )
                  ((=/= rest '())                
                   (== rest (cons arg2 '()))     ; If two arguments were passed.
                   (conde ((conde ((== builtin '+))
                                  ((== builtin '-))
                                  ((== builtin '*))
                                  ((== builtin '/)))
                           (typeo arg1 env 'num)
                           (typeo arg2 env 'num)
                           (== type 'num))
                          ((conde ((== builtin '>))
                                  ((== builtin '=))
                                  ((== builtin '>=)))
                           (typeo arg1 env 'num)
                           (typeo arg2 env 'num)
                           (== type 'bool))
                          ((== builtin '++)
                           (typeo arg1 env 'str)
                           (typeo arg2 env 'str)
                           (== type 'str)))))))
   
   ; Function expression
   ((fresh (fn args fn-typ fn-arg-typ exp-fn-arg-typ fn-rtn-typ)
           (== expr (cons fn args))
           (not-builtino fn)
           (=/= fn 'lambda)
           (typeo fn env fn-typ)
           (== fn-typ (cons exp-fn-arg-typ (cons fn-rtn-typ '())))
           (type-listo args env fn-arg-typ)
           (== exp-fn-arg-typ fn-arg-typ)
           (== type fn-rtn-typ)
           ))

   ((fresh (ids lmb args body arg-types zip-list newenv) ;handling lambda function calls
           (== expr (cons lmb args))
           (== lmb (list 'lambda ids body))
           (list-symbolo ids)
           (equal-lengtho ids args)
           (type-listo args env arg-types)
           (zippo ids arg-types zip-list)
           (appendo zip-list env newenv)
           (typeo body newenv type)
           ))
   
   ; function definition
   ((fresh (ids list-types body typeenv out newenv output)
           (== expr (list 'lambda ids body))
           (typeo body typeenv out)
           (appendo env typeenv newenv)
           (typeo body newenv output)
           (type-listo ids newenv list-types)
           (== type (cons list-types (list output)))
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


#|
(boolo expr)
  expr: A key in the association list

  The relational holds true if expr is of
  boolean (#t or #f) type.
|#

(define (boolo expr)
  (conde ((== expr #t))
         ((== expr #f))))

#|
(type-listo args typeenv exp-types)
  args: A list of arguments
  typeenv: An association list
  exp-types: A list of expected types of the arguments

  This relation holds if exp-types is the list of types
of the arguments.
|#
(define (type-listo args typeenv exp-types)
  (conde ((== args '()) (== exp-types '()))
         ((=/= args '()) (=/= exp-types '())
                         (fresh (farg rest-arg ftyp rest-typ)
                                (== args (cons farg rest-arg))
                                (typeo farg typeenv ftyp)
                                (== exp-types (cons ftyp rest-typ))
                                (type-listo rest-arg typeenv rest-typ)))))

#|
(appendo xs ys xsys)
  xs: A list.
  ys: A list.
  xsys: A list.

  This relation holds if xsys is equal to list ys
  appended to list xs.
|#
(define (appendo xs ys xsys)
  (conde ((== xs '())
          (== ys xsys))
         ((fresh (x xs^ xsys^)
                 (== xs (cons x xs^))
                 (== xsys (cons x xsys^))
                 (appendo xs^ ys xsys^)))))

#|
(equal-lengtho lst1 lst2)
  lst1: A list.
  lst2: A list.

  This relation holds if lst1 and lst2 have the same
  number of elements.
|#
(define (equal-lengtho lst1 lst2)
  (conde ((== lst1 '())
          (== lst2 '()))
         ((=/= lst1 '())
          (=/= lst2 '())
          (fresh (f-lst1 r-lst1 f-lst2 r-lst2 zip-list)
                 (== (cons f-lst1 r-lst1) lst1)
                 (== (cons f-lst2 r-lst2) lst2)
                 (equal-lengtho r-lst1 r-lst2)))))

#|
(zippo lst1 lst2 output)
  lst1: A list.
  lst2: A list.
  output: A list of pairs

  This relation holds if 'output' is the result of elements
  in lst1 zipped with elements in lst2.
|#
(define (zippo lst1 lst2 zip-list)
  (conde ((== lst1 '())
          (== lst2 '())
          (== zip-list '()))
         ((fresh (f-lst1 r-lst1 f-lst2 r-lst2 r-zip-list f-pair)
                 (== lst1 (cons f-lst1 r-lst1))
                 (== lst2 (cons f-lst2 r-lst2))
                 (== f-pair (cons f-lst1 f-lst2))
                 (== zip-list (cons f-pair r-zip-list))
                 (zippo r-lst1 r-lst2 r-zip-list)))))

#|
(list-symbolo lst)
  lst: A list.

  This relation holds if all elements in lst are symbols.
|#
(define (list-symbolo lst)
  (conde ((== lst '()))
         ((fresh (first rest)
                 (== lst (cons first rest))
                 (symbolo first)
                 (list-symbolo rest)))))


#|
(not-builtino fn)
  fn: A symbol.

  This relation holds if fn is not one of the
  built-in operations.
|#
(define (not-builtino fn)
  (conde((=/= fn '+)
         (=/= fn '-)
         (=/= fn '*)
         (=/= fn '/)
         (=/= fn '>)
         (=/= fn '=)
         (=/= fn '>=)
         (=/= fn '++)
         (=/= fn '!)
         (=/= fn 'num->str)
         (=/= fn 'len))))






         











  