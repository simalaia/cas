#! /usr/bin/env scheme --script

(load "dmatch.scm")

;; An attempt at implementing a CAS system, not overly successful.  Turns out one requires a hygienic variable substitution implementation for it to work.  Currently I am trying to wrap my head around nomimal algebra and closure calculus to help with this.  Was still fun and interesting though.

(define ops  '())
(define vars '())

(define (add-op o f) (set! ops (cons `(,o . ,f) ops)) )
(define (add-var v e) (set! vars (cons `(,v . e) vars)) )
(define (op e) (cdr (assoc e ops)) )
(define (op? e) (if (assoc e ops) #t #f) )
(define (var e) (cdr (assoc e vars)) )
(define (var? e) (if (assoc e vars) #t #f) )

(define (reduce e)
  (dmatch
   e
   ((,o . ,e)
    (if (op? o) ((op o) (reduce e)) (cons (reduce o) (reduce e))) )
   (,e (guard (number? e)) e )
   (,e (guard (symbol? e)) (if (var? e) (reduce (var e)) e) )
   (() '() )) )

#|
(add-op
 '- (case-lambda
      ((x)       `(* -1 ,(car x))  )
      ((x y)     `(+ ,x (* -1 ,y)) )
      ((x y . o) `(- ,x ,y ,o) )) )
|#
(add-op
 '- (lambda (e)
      (dmatch
       e
       ((,x ,y ,o) (reduce `(- ,x (- ,y ,o))) )
       ((,x ,y)    (reduce `(+ ,x (* -1 ,y))) )
       ((,x)       (reduce `(* -1 ,x)) )) ) )

(add-op
 '+ (lambda (e)
      (dmatch
       e add
       (((+ ,x ,y) ,o)    `(+ ,x ,y ,o) )
       ((,x (+ ,y ,o))    `(+ ,x ,y ,o) )
       ((,x ,y)           `(+ ,x ,y)  ) 
       ((,x)              `(,x))) ) )

(add-var 'foot '(* 12 (* 2.54 (* (/ 1 100) meter))))
(add-var 'mile '(* 5280 foot))

(display (reduce 32)) (newline)
(display (reduce 'n)) (newline)
(display (reduce '(- mile))) (newline)
(display (reduce '(- a b c))) (newline)
(display (reduce '(- a b))) (newline)
(display (reduce '(+ a (+ b c)))) (newline)
