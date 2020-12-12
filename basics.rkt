#lang racket

; ####### I. BASICS #######

; Racket is a functional programming language.
; Successor of Scheme
; Brackets are rougly analogous to HTML tags.
; Brackets make the tree-structure of a program trivially observable.
; () and [] are interchangeable (there are conventions).

; e     ... expression
; (e)   ... call of function e with no arguments
; ((e)) ... call of result of function e (called with no arguments) with no arguments

; ### Declaring Values and Functions
; NOTE: section II goes into much more detail!

(define x 1) ; x <- 1
(define y 2) ; y <- 2

; Everything uses prefix (Polish) notation!
; Add values bound to x and y.
(+ x y) ; x + y = 1 + 2 = 3

; ## Defining a Simple Function
(define (sayhello name) ; sayhello is the function name (id), name is the first parameter.
  (string-append "Hello, " name "!")) ; string-append takes any number of strings and concatenates them.
; Yes, identifiers in Racket can contain - as well as many other symbols that are typically
; reserved in other languages.

; Note (remember fun and val "ways" to define functions in SML) that this
; is a shorthand for:
(define sayhello2 (lambda (name)
                    (string-append "Hello, " name "!")))



; ### Control Flow
; An 'if sentence' has the for
; (if <cond> <then> <else>),
; where <cond> is the condition expression, <then> is the expression evaluated if
; <cond> evaluates to #t (#t is 'true' in Racket) and <else> is evaluated if <cond>
; evaluates to #f (#f is 'false' in Racket).
; Simple example:
(define some-val 5)
(if (< some-val 3) (print "some-val is less than 3.") (print "some-val is greater than 3."))

; If we have many conditions, we can use the Racket version of a 'switch'.
(cond [(< some-val 0) (print "some-val is less than 0.")]
      [(< some-val 3) (print "some-val is greater than 0 but less than 3")]
      [else (print "some-val is greater or equal to 3.")]) ; 'catch-all clause'


; Logical operators are and, or, ... and are used as:
(and #t #t #t)
(or #f #t #f)

