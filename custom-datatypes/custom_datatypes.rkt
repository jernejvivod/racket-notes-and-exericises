#lang racket

; ####### VI. Custom datatypes #######

; Racket is dynamically typed so explicit definitions of alternatives are not needed.
; A simple solution:
;  - simulating alternatives using lists of the form
;    (type val1 ... valn)
;  - make funcions for checking the datatype and functions used to access the elements.

; Simple examples of this naive approach.
(define (Bus n)
  (list "bus" n))  ; The string is a "datatype tag"

(define (Car type color)
  (list "car" type color))

(define (Bicycle) (list "bicycle"))

(define (Segment time means)
  (list "segment" time means))

; Functions for checking type equalities.
(define (Bus? x) (eq? (car x) "bus"))
(define (Car? x) (eq? (car x) "car"))
(define (Bicycle? x) (eq? (car x) "bicycle"))
(define (Segment? x) (eq? (car x) "segment"))

; Functions for accessing properties.
(define (Bus-n x) (car (cdr x)))
(define (Car-type x) (car (cdr x)))
(define (Car-color x) (car (cdr (cdr x))))
(define (Segment-time x) (car (cdr x)))
(define (Segment-means x) (car (cdr (cdr x))))

; Such an approach introduces many problems (the implementations are transparent, verbosity, etc.).

; A better solution is to use the struct function.
; (struct name (comp1 comp2 ... compn)) #:transparent    <- attribute allowing printing in REPL.

; The constructor, type equality comparison and component access functions are
; automatically created.
; (name comp1 comp2 ... compn)
; (name? e)
; (name-comp1 e),..., (name-compn e)

; Advantages:
;   - the implementation of the type is completely hidden
;   - the program is expanded using a new datatype
;   - automatic checking for errors
;   - the data can only be constructed using the constructor.
;   - the data can only be accessed using the accessing functions.

; TO BE CONTINUED.

