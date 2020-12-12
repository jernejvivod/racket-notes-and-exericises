#lang racket

; Define map function that takes a function and a list and applies
; f to each element of l.
; (map is already built-in)
(define (map f l)
  (if (null? l) null
      (cons (f (car l)) (map f (cdr l)))))

; Define a function that takes an integer n and produces a function
; that maps each integer in a list to its n-th power.
(define (raise-l-to-pow n)
  (curry map (lambda (x) (expt x n))))

; Use raise-l-to-pow to get two functions.
(define f_res2 (raise-l-to-pow 2))
(define f_res3 (raise-l-to-pow 3))


; Simple unit tests
(require rackunit)

(check-equal? (f_res2 '(1 2 3 4 5)) '(1 4 9 16 25))
(check-equal? (f_res3 '(1 2 3 4 5)) '(1 8 27 64 125))
