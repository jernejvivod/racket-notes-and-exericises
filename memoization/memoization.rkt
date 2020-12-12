#lang racket

; ####### V. MEMOIZATION #######

; If the function has no side effects, we can save the answers for later use.
; Storing the value may be faster than computing it again.

; Implementation
; ** Use a list of pairs of found solutions.
;   - '((arg_1, ans_1), ..., (arg_n, ans_n))
;   - We do not want it to be globally available.
;

; ** If the solution exists, parse it from the list.
;   - We can use the built-in function assoc

; ** If the solution does not exist, we compute it and augment the list of solutions.
;   - We need to use mutation (set!) to augment the list.

; Example - improving the naive implementation of a function for computing
; Fibonacci numbers.

(define fib3
  (letrec ([solutions null] ; Define empty list for storing solutions.
           [aux (lambda (x) ; Auxiliary function for performing the computations.
                  (let ([ans (assoc x solutions)]) ; Locate the first element of solutions whose car is equal to x.
                    (if ans ; If solution found ...
                        (cdr ans) ; return it. Else ...
                        (let ([new (cond [(= x 1) 1] ; Set new value with help of recursive call. 
                                         [(= x 2) 1]
                                         [else (+ (aux (- x 1))
                                                  (aux (- x 2)))])])
                          (begin
                            (set! solutions (cons (cons x new) solutions)) ; Put computed solution at beginning of solutions list.
                            new)))))]) ; Return computed value.
    aux))

