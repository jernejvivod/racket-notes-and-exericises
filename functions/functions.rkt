#lang racket

; ####### III. FUNCTIONS #######

; Note the two versions of binding a value.
; (define id expr)
; (define (id args) body ...+)

; The SECOND FORM is a shorthand for function definitions and is shorthand for
; (define id (lambda (arg ...) body ...+))

; ### Examples ###

; Define a salutation ... EXAMPLE OF A VALUE DEFINITION
; list-ref is a function that takes a list and an index and returns the value at that index.
(define salutation (list-ref '("Hi" "Hello") (random 2)))

; Define greeting function (shorthand and lambda)

; SHORTHAND ... THINK fun IN SML!
(define (greet name)
  (string-append salutation ", " name))

; LAMBDA ... THINK val IN SML!
(define greet2 (lambda (name)
                 (string-append salutation ", " name "!")))

; CALL THE TWO FUNCTIONS!
(greet "John")
(greet2 "Julia")

; A greeting function with two optional arguments and a keyword argument.
(define (greet3 first [surname "Smith"] #:hi [hi "hi"])
  (string-append hi ", " first " " surname))

; EXAMPLE OF CALL
(greet3 "John")
(greet3 "John" "Doe")
(greet3 "John" #:hi "Hey")
(greet3 "John" "Doe" #:hi "Hey")

; EXERCISE - Same greeting using a lambda function binding.
(define greet4 (lambda (first [surname "Smith"] #:hi [hi salutation])
                 (string-append hi ", " first " " surname)))


; The function shorthand via define also supports a rest argument (i.e., a final argument to collect extra arguments in a list).
; (define (id arg ... . rest-id) body ...+)
; Similarly as before, this is a shorthand for:
; (define id (lambda (arg ... . rest-id) body ...+))

; ### Examples ###

; 'apply' applies proc using the content of (list* v ... lst) as the (by-position) arguments.
(define (avg . l)
  (/ (apply + l) (length l)))

; EXAMPLE OF CALL
(avg 1 2 3)


; Defining a curried function using lambdas.
(define make-add-suffix
  (lambda (s2)
    (lambda (s) (string-append s s2))))

; Example of a call
((make-add-suffix "!") "hello")

; Using the function shorthand, the same example can be written as:
(define (make-add-suffix2 s2)
  (lambda (s) (string-append s s2)))


; The define form further supports a shorthand for defining curried functions that reflects nested function calls.
(define ((make-add-suffix3 s2) s)
  (string-append s s2))

(define louder (make-add-suffix3 "!"))

; EXAMPLE OF CALL
(louder "tralala")

; Racket also has the curry function that can be used to curry any function.
; NOT INCLUDED IN racket/base
(define (add a b)
  (+ a b))
; Example of use:
(define (add-one x)
  (curry add 1)) 


; The full syntax of the function shorthand for defined is as follows:

; (define (head args) body ...+)
; 
; head	 	=	 	id
;  	 	|	 	(head args)
; 
; args	 	=	 	arg ...
;  	 	|	 	arg ... . rest-id


; The expansion of this shorthand has one nested lambda form for each head in the definition, 
; where the innermost head corresponds to the outermost lambda.


; Multiple Values

; Multiple-valued functions can be implemented in terms of the values function,
; which takes any number of values and returns them as the results.
; Example of use
(define (split-name name)
  (let ([parts (regexp-split " " name)]) ; Split by space between first and last name.
    (if (= (length parts) 2)             ; Check if form of name correct.
        (values (list-ref parts 0) (list-ref parts 1))
        (error "not a <first> <last> name"))))

; EXAMPLE OF CALL
(split-name "Julia Smith")


; The define-values form binds multiple identifiers at once to 
; multiple results produced from a single expression.
; THINK PATTERN MATCHING!
(define-values (given surname) (split-name "Julia Smith"))

; Internal function definition example (factorial)
(define (fac n)
  (define (aux n [acc 1])
    (if (= n 0) acc (aux (- n 1) (* n acc)))) 
  (aux n))


; ### Named let
; More on this in later part!

; A normal let usage can be considered a anonymous procedure call:
(let ([a 10]
      [b 20])
  (+ a b))

; Same as
((lambda (a b)
   (+ a b)) 10 20)


; A named let just binds that procedure to a name in the scope of the procedure
; Note similarity with internal functions.

; EQUIVALENT DEFINITIONS FOR A CUMULATIVE SUM OF A SIMPLE LIST
; NOTE - Can use 'first' and 'rest' instead of 'car' and 'cdr' if using #lang racket and not #lang racket/base.
(let sum ([l '(1 2 3)] ; 'sum' is the name of the let expression - we treat the values as arguments. the '(1 2 3) and 0 are the starting values.
          [acc 0])
  (if (null? l) acc (sum (cdr l) (+ (car l) acc))))
; NOTE - The above computes the comulative sum directly (cumulative sum of '(1 2 3)).

(define (sum1 [l '(1 2 3)] [acc 0]) 
  (if (null? l) acc (sum1 (cdr l) (+ (car l) acc))))
(sum1 '(1 2 3))

(define sum2 (lambda ([l '(1 2 3)] [acc 0])
               (if (null? l) acc (sum2 (cdr l) (+ (car l) acc)))))
(sum2 '(1 2 3))

; using let-rec
; Note how the function aux references itself in the body.
; MORE ON THIS LATER!
((lambda (l) (letrec (
                      [aux (lambda (l acc) (if (null? l) acc (aux (cdr l) (+ (car l) acc))))]) 
               (aux l 0))) '(1 2 3))

