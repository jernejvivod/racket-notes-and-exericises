#lang racket

; ####### IV. LOCAL ENVIRONMENTS #######

; let ... the expressions are evaluated in the environment before the let expression.
; let* ... the expressions are evaluated as the result of previous declarations (let in SML).
; letrec ... the expressiosn are evaluated in an environment that includes all the given
; declarations (mutual recursion).
; define ... the semantic is equivalent to letrec (different syntax)

; The following function using letrec is equivalent to the below function
; using define (different syntax).
(define (test1 x)
  (letrec ([a 1]
           [b (lambda (x) (+ x c))]
           [c 3])
    (+ a (b x) c x)))

(define (test2 x)
  (define a 1)
  (define b (lambda (x) (+ x c)))
  (define c 3)
  (+ a (b x) c x))


; Example of a problem that can occur due to lazy evaluation.
(define (test-letrec1 a)
  (letrec ([b 3]
           [c (lambda (x) (+ a b d x))] ; Note binding of d (remember and in SML).
           [d (+ a 1)])
    (c a)))

(define (test-letrec2 a)
  (letrec ([b 3]
           [c (+ d 1)] ; Issue - lazy evaluation
           [d (+ a 1)])
    (+ a d)))


; Note differences in output for the following functions.
(define (test-let a)
  (let ([a 3]
        [b (+ a 2)]) ; Here, 'a' refers to the function argument!
    (+ a b)))

(define (test-let* a)
  (let* ([a 3]
         [b (+ a 2)]) ; Here 'a' refers to the 'a' in previous [] (LIKE let in SML)!
    (+ a b)))

