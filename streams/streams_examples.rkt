#lang racket

#|
Another drawback of these two datastructures is their finiteness. This statement might sound surprising
initially, since any datastructure that we want to represent must be finite for obvious reasons. There are
applications, however, in which it is more convenient to structure the computation assuming that we
have one or more infinite datasources, from which we can extract as much data as we need for the
problem at hand. If we are looking, for example, for the smallest prime number with a particular property,
we might not know how far in the infinite sequence of ordered prime numbers we need to search to find
it. In such a situation, it might make sense to conceive of our computation as using a potentially infinite
datasource that produces prime numbers on demand. A stream is such a datasource.

We stress that "infinite" and "infinity" refer to potential (theoretical) infinity, and not actual infinity. Due to
the obvious limitations of various resources (time, memory, and others), no computation will produce an
infinite sequence of values. Our streams are infinite in potential, not in fact.

Before we define the stream datatype, let us think for a minute about how we could specify an infinite
stream of values. It is immediately clear that we can not actually enumerate these values, so we are left
with the alternative of providing a method for computing them. This means that our streams must rely on
an infinity of function calls. This is only possible if (at least some of) the functions involved are directly or
indirectly recursive. Now, the infinite sequence of function calls can not run to the "end" (there is no end
of infinity, of course); at any given time, only a finite number of such calls must have been initiated. Thus
me must have a mechanism for (temporarily) stopping further recursive calls. If we understand how we
can suspend, and later resume, the sequence of computations that generates the stream values, then
we can write streams in Racket.
|#



; Let us now define the simplest stream - an infinite stream that consists of the same repeated value, ad infinitum:
(define (const val)
  (cons val (lambda () (const val)))) ; Construct pair of value and delayed function that returns the next stream element.

; ### Using Racket stream tools ###
(define (const2 val)
  (stream-cons val (const2 val)))



; ### Examples of using this stream ###
(car (const 7)) ; Get head of stream.
(car ((cdr (const 7)))) ; Get second element of stream.


	
; shd: Returns the first element of a stream.
(define (shd stream)
  (car stream)) ; Get head of stream.

; ### Using Racket stream tools ###
(define (shd2 stream)
  (stream-first stream))



; stl: Returns the stream that results after removing the first element.
(define (stl stream)
  ((cdr stream))) ; Get rest of stream.

; ### Using Racket stream tools ###
(define (stl2 stream)
  (stream-rest stream))



; smap: Applies a function to every element of a stream.
(define (smap f stream)
  (cons (f (car stream)) (lambda () (smap f ((cdr stream)))))) ; Construct stream element with
; the function applied to the head element and recursive call as return value of the delaying function
; with the rest of the stream.

; ### Using Racket stream tools ###
; use (stream-map f s)



; take_n: Returns the list of the first n elements of the stream.
(define (take-n n stream)
  (if (= n 0) null (cons (car stream) (take-n (- n 1) ((cdr stream)))))) ; if taking 0 elements, return empty list. else concatenate head and results of take-n
; for the rest of the stream.

; ### Using Racket stream tools ###
(define (take-n2 n stream)
  (stream->list (stream-take stream n)))



; nth: Return the nth element in the stream.
(define (nth n stream)
  (if (= n 0) (car stream) (nth (- n 1) ((cdr stream))))) ; Similar idea as above.

; ### Using Racket stream tools ###
; use (stream-ref s i)



; sfilter: Produces a stream of values that satisfy a predicate.
(define (sfilter f stream)
  (if (f (car stream)) ; Check if predicate holds for head of stream.
      (cons (car stream) (lambda () (sfilter f ((cdr stream))))) ; If predicate holds, keep head and make recursive call for rest of stream.
      (sfilter f ((cdr stream))))) ; Else make recursive call for rest of stream (head is discarded).

; ### Using Racket stream tools ###
; use (stream-filter f s)



; ####### Creating finite streams #######

; list-to-stream: Produces a finite stream containing elements of passed list.
(define (list-to-stream l)
  (if (null? l) null (cons (car l) (lambda () (list-to-stream (cdr l)))))); End of stream represented with null. Better way?

; ### Using Racket stream tools ###
(define (list-to-stream2 l)
  (if (null? l) empty-stream (stream-cons (car l) (list-to-stream2 (cdr l)))))



; sconcat: concatenate streams stream1 and stream2.
(define (sconcat stream1 stream2)
  (cond [(null? stream1) stream2] ; If first stream is null, return second stream.
        [else (cons (car stream1) (lambda () (sconcat ((cdr stream1)) stream2)))])) ; Else, construct stream with head element of first stream
; and results of recursive sconcat for stream1 without the first element and stream2.
; ### Using Racket stream tools ###
(define (sconcat2 stream1 stream2)
  (if (stream-empty? stream1)
      stream2
      (stream-cons (car stream1) (sconcat2 (stream-rest stream1) stream2))))



; nats: Returns a stream that contains natural numbers starting from n
(define (nats)
  (define (aux n) ; auxiliary function.
    (cons n (lambda () (aux (+ n 1))))) ; Create stream element.
  (aux 1)) ; Call auxiliary function with first value.

; ### Using Racket stream tools ###
(define (nats2)
  (define (aux n)
    (stream-cons n (aux (+ n 1))))
  (aux 1))



; fib: Returns a stream that contains the Fibonacci sequence starting with a and b
(define (fib)
  (define (aux val1 val2) ; auxiliary function
    (cons val1 (lambda () (aux val2 (+ val1 val2)))))
  (aux 1 1)) ; Call auxiliary function with first two fibonacci numbers.

; ### Using Racket stream tools ###
(define (fib2)
  (define (aux val1 val2)
    (stream-cons val1 (aux val2 (+ val1 val2))))
  (aux 1 1))



; pows: Return a stream of powers of base to incrementing powers (incrementing by 1).
; exp is the starting exponent value.
(define (pows base)
  (define (aux pow) ; auxiliary function
    (cons (expt base pow) (lambda () (aux (+ pow 1))))) ; Create stream element.
  (aux 0)) ; Call auxiliary function with initial power.

; ### Using Racket stream tools ###
(define (pows2 base)
  (define (aux pow)
    (stream-cons (expt base pow) (aux (+ pow 1))))
  (aux 0))



; multiples: Returns a stream of multiples of n.
(define (multiples n)
  (define (aux mult) ; auxiliary function
    (cons (* n mult) (lambda () (aux (+ mult 1))))) ; Create stream element.
  (aux 1)) ; Call auxiliary function with initial multiple.

; ### Using Racket stream tools ###
(define (multiples2 n)
  (define (aux mult)
    (stream-cons (* mult n) (aux (+ mult 1))))
  (aux 1))



#|
The Sieve of Eratosthenes is possibly the oldest systematic method (algorithm) for generating the
sequence of all prime numbers. The "sieve" can be described as follows:

step 1: Generate the sequence of natural numbers starting at 2.
step 2: Position yourself just before the beginning of the sequence.
step 3: Find the next available number in the sequence. Write it down; it is prime.
step 4: Cross out (delete) all multiples of the number identified in step 3.
step 5: Continue with step 3.
|#


; sieve: implement the Sieve of Eratosthenes. This function expects to be passed a stream of natural numbers starting with 2.



; discard_n: return stream left after removing first n elements.
(define (discard-n n stream)
  (if (= n 0) stream (discard-n (- n 1) ((cdr stream))))) ; base case - discard 0 elements.
; Else, discard (n - 1) elements from the rest of the stream.

; ### Using Racket stream tools ###
(define (discard-n2 n stream)
  (if (= n 0) stream (discard-n2 (- n 1) (stream-rest stream))))


 
; zip: take two streams and return stream with terms alternatingly from stream 1 and stream 2.
(define (zip stream1 stream2) ; Put element from first stream into stream and make recursive call with swapped streams.
  (cons (car stream1) (lambda () (zip stream2 ((cdr stream1))))))

; ### Using Racket stream tools ###
(define (zip2 stream1 stream2)
  (stream-cons (stream-first stream1) (zip2 stream2 (stream-rest stream1))))


 
; zip_n: simiar to above function. The parameter n tells how many elements in a row to take from each stream.
(define (zipn n stream1 stream2)
  (define (aux count stream1 stream2)  ; Auxiliary function - take elements from stream until count reaches 0. After that, reset count and swap streams.
    (if (= count 0) (aux n stream2 stream1) (cons (car stream1) (lambda () (aux (- count 1) ((cdr stream1)) stream2)))))
  (aux n stream1 stream2))

; ### Using Racket stream tools ###
(define (zipn2 n stream1 stream2)
  (define (aux count stream1 stream2)
    (if (= count 0)
        (aux n stream2 stream1)
        (stream-cons (stream-first stream1) (aux (- count 1) (stream-rest stream1) stream2))))
  (aux n stream1 stream2))




; unzip: take stream and return ordered pair of streams where the first stream contains the odd elements in stream and second stream even elements of stream.


; rle: encode stream using the rle encoding algorithm. Return ordered pair (int * 'a).

; rle_decode: take a stream of ordered pairs (int * 'a) where the first element is the number of repetitions of element at second position and
; return a stream representing the decoded data (convert each ordered pair to n repetitions of elements and add to stream).

; An explicitly defined stream for testing the rle_decode function.

; fold_n: perform fold on first n elements of list.

 
; fold_stream: perform fold operation on stream and return stream of accumulator values.
