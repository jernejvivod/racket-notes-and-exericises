#lang racket

; ####### V. EAGER AND LAZY EVALUATION, DELAY AND FORCE, STREAMS #######

; The semantics of a programming language must specify when the expressions are
; evaluated. Remember the declerations:
(define x1 (+ 1 2))
(define x2 (string-append "this is a " "test."))

; If the expression being bound is a function, the body is evaluated when
; the function is called (delayed evaluation).
(define x3 (lambda (x) (+ x 2)))

; What about the conditional (if <condition> <then> <else>)?
; The expressions <then> and <else> are evaluated only after evaluating
; <condition> (only the applicable one).

; Note the differences between the following:
; ###
; # 1.
(define (power x n)
  (if (= n 0)
      1
      (* x (power x (- n 1)))))
; ###
; # 2.
(define (my-if cond then else)
  (if cond then else)) ; difference - both then and else are evaluated when passed
; as arguments to the my-if function.

(define (power2 x n)
  (my-if (= n 0)
         1
         (* x (power2 x (- n 1)))))
; ###

; Idea - if we want to delay the evaluation, we can put the expression into a function (can have no parameters).
; "In compute science, a thunk is a parameterless closure created to prevent the evaluation of an expression until forced at a later time."

; We can use this to fix our custom if and power code.
(define (my-if2 cond then else)
  (if cond (then) (else))) ; Evaluate the functions passed as then and else arguments.

(define (power3 x n)
  (my-if2 (= n 0)
          (lambda () 1) ; Force delayed evaluation.
          (lambda () (* x (power3 x (- n 1))))))


; How could we evaluate an expression only when explicitly needed?

; Some constructs we will need:
; A sequence of expressions (the value of the last expression is returned):
(begin (+ 1 2) (+ 2 3) (print "test") (+ 7 7))

; A pair with mutable components.
; mcons - constructor
; mcar - head
; mcdr - tail
; mpair? - is pair?
; set-mcar! - set new head.
; set-mcdr! - set new tail.

; zakasnitev in sprozitev (delay and force)
; The mechanism is built into Racket.

; Delay takes a delayed function and returns a pair with components:
; bool: has the expression been evaluated?
; the delay function or the evaluated expression.

; Delay
(define (my-delay thunk)
  (mcons #f thunk))

; Force
(define (my-force prom)
  (if (mcar prom) ; Check if already evaluated.
      (mcdr prom) ; If yes, return evaluated expression.
      (begin (set-mcar! prom #t) ; Else, set flag to true.
             (set-mcdr! prom ((mcdr prom))) ; Evaluate expression.
             (mcdr prom)))) ; Return evaluated expression.

; Example
(define md
  (my-delay
   (lambda () (+ 3 2))))

(my-force md)


; ### Streams

; stream - infinite sequence of values that cannot be defined by specifying all the values.
; Idea - give only the current value and delay the evaluation (thunk) to compute the
; next value.

; Idea: give only the current value and delay the evaluation (thunk) to compute the next
; value.

; Let us define the stream as a pair:
; '(value . func-for-next)

; The pair has the following properties:
;    - The delayed function (thunk) generates the next element in the sequence
;      that is also the pair of the same form.
;    - The delayed function can contain a recursive call that is performed only when the
;      function is called.


; Obtaining the elements:
; (car s)                 .... the first element
; (car ((cdr s)))         .... the second element
; (car ((cdr ((cdr s))))) .... the third element

; ### Examples ###

; A stream of ones
(define ones (cons 1 (lambda () ones))) ; pair of value 1 and delaying function.

; A stream of natural numbers
; f is a function that takes an integer x and returns a pair consisting of x and a delaying function
; that returns the result of calling f with the successor of x.
(define natural
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (f 1)))

; A stream alternating between 1 and -1
; Similar idea as above.
(define plusminus
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x -1)))))])
    (f 1)))

; A stream of powers of 2
; Similar idea as above.
(define powers2
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (f 2)))

; A function that outputs the first n values in the stream
(define (print-stream n stream)
  (if (> n 1) ; If printing more than one element.
      (begin
        (displayln (car stream)) ; Print head of stream.
        (print-stream (- n 1) ((cdr stream)))) ; Recursive call for rest of stream.
      (displayln (car stream)))) ; (<else> clause) - print just the head of stream.

; A function for outputting a stream while a condition holds
(define (print-stream-while stream condition)
  (cond [(condition (car stream)) (begin  ; If condition holds, print head and make recursive call for rest of stream.
                                    (displayln (car stream))
                                    (print-stream-while ((cdr stream)) condition))]
        [else #t])) ; (<else> clause) - return signal value for finish.

; A function for counting the number of elements in a stream before a condition holds
(define (count-stream-while stream condition)
  (define (aux stream condition acc)  ; Define an auxiliary function that also accepts an accumulator.
    (cond [(condition (car stream)) (aux ((cdr stream)) condition (+ acc 1))] ; If condition holds, make recursive call for rest with incremented acc.
          [else acc])) ; (<else> clause) - return accumulator.
  (aux stream condition 0)) ; Call auxiliary function with initial accumulator value.

