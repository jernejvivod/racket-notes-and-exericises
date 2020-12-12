#lang racket

;  _____            _        _     _   _       _            
; |  __ \          | |      | |   | \ | |     | |           
; | |__) |__ _  ___| | _____| |_  |  \| | ___ | |_ ___  ___ 
; |  _  // _` |/ __| |/ / _ \ __| | . ` |/ _ \| __/ _ \/ __|
; | | \ \ (_| | (__|   <  __/ |_  | |\  | (_) | ||  __/\__ \
; |_|  \_\__,_|\___|_|\_\___|\__| |_| \_|\___/ \__\___||___/
; by Jernej Vivod                                                          
                                                           

; #### INDEX ####
;
; line 29  ... Basics
; line 90  ... Lists and Racket Syntax
; line 189 ... Functions
; line 365 ... Local Environments
; line 419 ... Eager and Lazy Evaluation, Delay and Force, Streams
; line 583 ... Memoization
; line 624 ... Macros
; line 755 ... Custom Datatypes







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







; ####### II. LISTS AND RACKET SYNTAX #######

; The cons function is similar to :: in SML.

; The cons function actually accepts any two values, not just a list for the second argument.
; When the second argument is not empty and not itself produced by cons,
; the result prints in a special way. The two values joined with cons are printed between
; parentheses, but with a dot in between.

(cons 1 2)

; In general, the result of cons is a pair (tuple).

; The more traditional names for first and rest are car and cdr.

; For nested lists, especially, the quote form lets you write a list as an expression in 
; essentially the same way that the list prints.
(quote ("red" "green" "blue"))

; This is equivalent to
'("red" "green" "blue")

; Equivalent ways to define lists:
(list (list 1 2 3) 5 (list "a" "b" "c"))
(quote ((1 2 3) 5 ("a" "b" "c")))


; ### Quoting (CAN SKIP FOR NOW!)

; If you wrap an identifier with quote, then you get output that looks like an identifier, 
; but with a ' prefix.
(quote jane-doe)

; A value that prints like a quoted identifier is a symbol. In the same way that parenthesized 
; output should not be confused with expressions, a printed symbol should not be 
; confused with an identifier. In particular, the symbol (quote map) 
; has nothing to do with the map identifier or the predefined function that 
; is bound to map, except that the symbol and the identifier happen to be made 
; up of the same letters.

; Indeed, the intrinsic value of a symbol is nothing more than its character content. In this sense,
; symbols and strings are almost the same thing, and the main difference is how they print. 
; The functions symbol->string and string->symbol convert between them.

; Some examples
map

(quote map)
(symbol? (quote map))

(symbol? map)

(procedure? map)

(string->symbol "map")
(symbol->string (quote map))

(car (quote (road map)))

(symbol? (car (quote (road map))))

; The quote form has no effect on a literal expression such as a number or string.
(quote 42)
(quote "on the record")


; The syntax of Racket is not defined directly in terms of character streams. Instead, the syntax is
; determined by two layers:

; - a reader layer, which turns a sequence of characters into lists, symbols, and other constants;
; - an expander layer, which processes the lists, symbols , and other constants to parse them as an
; expression.


; Normally, . is allowed by the reader only with a prenthesized sequence, and only before the last
; element of the sequence. However, a pair of .s can also appear around a single element in a
; parenthesized sequence, as long as the element is not first or last. Such a pair triggers a 
; reader conversion that moves the element between .s to the front of the list. 
; The conversion enables a kind of general infix notation.

(1 . < . 2)
'(1 . < . 2)

; This two-dot convention is non-traditional, and it has essentially nothing to do with the dot notation for non-list pairs. Racket programmers use the infix convention sparingly — mostly for asymmetric binary operators such as < and is-a?.

; the quote operate takes in some expression and converts it to a literal expression.

; The expression (+ 2 2) is not evaluated.
'(3 (+ 2 2) 5)

; If you want to evaluate the expression before constructing the list, use:
(list 3 (+ 2 2) 5)







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
