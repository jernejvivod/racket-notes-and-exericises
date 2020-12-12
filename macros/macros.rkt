#lang racket

; ####### VI. MACROS #######

; A macro defines how a programming language syntax is transformed into another syntax.
;  -> a tool given by the programming language
;  -> an expansion of the language with new keywords
;  -> implementation of syntactic sugars

; Macro expansion is carried out before compilation.

; examples:
; (my-if cond then e1 else e2)                     ... own if sentence
; (if3 cond1 then e1 elsif cond2 then e2 else e3)  ... triple if
; (first stream), (second stream),                 ... elements of a stream 
; (anotate xyz "some comment")                     ... annotation of variables

; ### Defining macros

; Use reserved keyword 'define-syntax'
; Other keywords given by 'syntax-rules'
; We give patters for macro expansion in [ ... ]

; ### Examples ###
(define-syntax if3  ; State the "function" name.
  (syntax-rules (then elseif else)  ; State other keywords in the syntax.
    [(if3 e1 then e2 elsif e3 then e4 else e5) ; State the expression.
     (if e1 e2 (if e3 e4 e5))])) ; State what the expression is equivalent to.

(define-syntax third
  (syntax-rules ()  ; Here, we have no additional keywords to state.
    [(third e)
     (car ((cdr ((cdr e)))))]))

(define-syntax anotate
  (syntax-rules ()
    [(anotate e s)
     e]))


; Possible syntactic errors can occur:
;    - when using the macro syntax,
;    - when using the syntax that the macro expands into.

; The following points require attention:
;    1. Is the macro even applicable (could a function be used instead)?
;    2. What is the priority of the computed expressions?
;    3. How are the expressions in macros evaluated?
;    4. The semantics of the variable scope - we use two environments:
;          - the environemnt in the macro definition,
;          - the environemnt where the macro expands into the code.


; ### Example of where a macro could be useful.
; When defining my-delay, we had to provide a delaying function (thunk).
; (my-delay (lambda () (+ 3 2)))
; We want to simplify this to
; (my-delay (+ 3 2))
; Can only be done using macros as arguments are evaluated when the function is called.
(define-syntax my-delaym
  (syntax-rules ()
    [(my-delaym e)
     (mcons #f (lambda () e))]))

; my-force can be in the form of a function (a macro would not work as desired).

; One problem that can occur with macros in C++:
;   #define ADD(x,y) x+y
;   this macro performs the following expansion: Add(1,2)*3 -> 1+2*3
;   To obtain the expected behaviour, the macro should be defined as
;   #define ADD(x,y) ((x)+(y))

; Racket DOES NOT have these problems as it uses prefix notation that clearly defines priority.
; (my-add a b) -> (+ a b)
; (* (my-add 1 2) 3) -> (* (+ 1 2) 3)

; We must consider how many times each expression is evaluated.
; Example of two macros that are not equivalent.
(define-syntax twice1
  (syntax-rules () [(twice1 x) (+ x x)])) ; Expression x is evaluated twice.

(define-syntax twice2
  (syntax-rules () [(twice2 x) (* 2 x)])) ; Expression x is evaluated only once.


; We can avoid multiple evaluations of the same expression using local variables.
(define-syntax twice3
  (syntax-rules ()
    [(twice3 x) (let ([my-x x]) (+ my-x my-x))])) ; my-x is evaluated only once.


; ### Macros - scope semantics

; Note - naive expansion, as used by, for example, C++, is equivalent to find & replace.
; This can cause unexpected results as the variables with names used in the macros may
; already be in use. We call this an "unhygienic macro system".

; Let us look at an example.

(define-syntax swap
  (syntax-rules ()
    ((swap x y)   ; Swap values in variables x and y.
     (let ([tmp x]) ; Bind to temporary variable.
       (set! x y)  ; Perform standard swap.
       (set! y tmp)))))

(let ([tmp 5] ;    tmp <- 5
      [other 6]) ; other <- 6
  (let ([tmp tmp]) ; THE EXPANDED MACRO  - tmp <- tmp (6)
    (set! tmp other) ; tmp <- 6 ... PROBLEM - we would like to consider the externally defined tmp!!!
    (set! other tmp)) ; other <- 6
  (list tmp other)) ; '(5 6) ... THE SWAP WAS NOT MADE!

; How does Racket solve this issue?
; (Note - in naive implementations, this is solved using uncommon variable names.)
; A possible solution would be for the macro to automatically rename all variables.
; A potential solution could also be to use some sort of closure. Use lexical scope!
; Automatically rename local variables. Racket uses this!

; SUMMARY - THE 4 POTENTIAL PROBLEMS
; 1. Is the use of macros appropriate for a specific task? (WHY NOT USE A FUNCTION?)
; 2. Priority of computations? (HOW TO DEAL WITH ENSURING CORRECT PRIORITY OF COMPUTATION AFTER EXPANDING MACROS?)
; (NOT APPLICABLE FOR RACKET!)
; 3. How to avoid redundant multiple evaluations? (USE LOCAL VARIABLES INTRODUCED BY let)
; 4. How to deal with scope semantics problems? (WHAT IF THE VARIABLES USED IN MACRO ALREADY USED IN CODE?)
; (NOT APPLICABLE FOR RACKET!)

