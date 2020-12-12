#lang racket

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

