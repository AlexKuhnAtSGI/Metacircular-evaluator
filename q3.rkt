#lang r5rs
;Alexander Kuhn
;ID 101023154

;Taken from the course notes
(define (append list1 list2)
  (cond ((null? list1) list2)
        ((list? list1) (cons (car list1) (append (cdr list1) list2)))
        (else (cons list1 list2))))
        
;Part of my solution to Assignment 3
;Takes as argument a list L, an index i, and an item A to place in the list at the given index
(define (splice L i A)
  ;splice-iter has two extra arguments, Z and count
  ;splice-iter works by cdr-ing down L until it hits the right index: meanwhile, it populates Z with the items ignored from L
  ;count tracks how far we have to go until we get to the right index, at which point it pops A onto L and attaches Z back to the front of L
  (define (splice-iter L i A Z count)
    (cond ((= i count) (append (reverse Z) (append A L)))
          (else (splice-iter (cdr L) i A (cons (car L) Z) (+ 1 count)))))
  (splice-iter L i A '() 0))

(define (splice-replace L i A)
  ;identical to standard splice, but it overwrites the item at the given index rather than prepending it with A
  (define (splice-iter L i A Z count)
    (cond ((= i count) (append (reverse Z) (append A (cdr L))))
          (else (splice-iter (cdr L) i A (cons (car L) Z) (+ 1 count)))))
  (splice-iter L i A '() 0))

(define (splice-delete L i)
  ;identical to splice-replace, but it deletes the item at the given index rather than overwriting it
  (define (splice-iter L i Z count)
    (cond ((= i count) (append (reverse Z) (cdr L)))
          (else (splice-iter (cdr L) i (cons (car L) Z) (+ 1 count)))))
  (splice-iter L i '() 0))

;list object
(define (make-list)
  ;initializes "internal" (the list attribute) as an empty list
  (define internal (list))

  ;prints the list
  (define (print)
    (display internal))

  ;simply counts how many cdrs it can do before the list is null
  (define (size)
    (define (size-iter L count)
      (cond ((null? L)  count)
            (else (size-iter (cdr L) (+ 1 count)))))
    (size-iter internal 0))

  ;purely for my convenience - returns the last index in the list, i.e. size -1
  (define (real-size)
    (define (size-iter L count)
      (cond ((null? L) (- count 1))
            (else (size-iter (cdr L) (+ 1 count)))))
    (size-iter internal 0))

  ;gets ith index
  ;takes one integer argument (the index)
  (define (get i)
    (define (get-iter L i count)
      (cond ((= i count) (car L))
            (else (get-iter (cdr L) i (+ 1 count)))))
    (cond ((> i (real-size)) #f)
          ((< i 0) #f)
          ((integer? i) (get-iter internal i 0))
          (else #f)))

  ;adds item at ith index using splice
  ;takes one integer argument i (the index) and one argument x (the item)
  (define (add i x)
    (cond ((> i (+ 1 (real-size))) #f)
          ((< i 0) #f)
          ((integer? i) (set! internal (splice internal i x)))
          (else #f)))

  ;overwrites item at ith index using splice-replace
  ;takes one integer argument i (the index) and one argument x (the item to overwrite with)
  (define (set i x)
    (cond ((> i (real-size)) #f)
          ((< i 0) #f)
          ((integer? i) (set! internal (splice-replace internal i x)))
          (else #f)))

  ;deletes item at ith index using splice-delete
  ;takes one integer argument i (the index)
  (define (delete i)
    (let ((x (get i)))
      (cond ((> i (real-size)) #f)
            ((< i 0) #f)
            ((integer? i) (set! internal (splice-delete internal i)))
            (else #f))
      x))
  
  ;this is how methods are recognized as valid calls by list objects
  (define (dispatch method)
    (cond ((eq? method 'print) print)
          ((eq? method 'size) size)
          ((eq? method 'get) get)
          ((eq? method 'add) add)
          ((eq? method 'set) set)
          ((eq? method 'delete) delete)
          (else (lambda() (display "Unknown Request: ")(display method)(newline)))))

   
  dispatch
  )







;print/add/make-list tests
(define L1 (make-list))
(define L2 (make-list))
((L1 'add) 0 'a)
((L1 'add) 1 'b)
((L1 'add) 2 'c)
((L1 'add) 3 'd)

(display "L2: ") (newline)
(display "Expected: ()")(newline)
(display "Actual: ")((L2 'print))(newline)

(display "L1: ")(newline)
(display "Expected: (a b c d)")(newline)
(display "Actual: ")((L1 'print))(newline)
(newline)

;adding to empty list was tested already, so we test specific index now
((L1 'add) 2 'z)
(display "L1: ")(newline)
(display "Expected: (a b z c d)")(newline)
(display "Actual: ")((L1 'print))(newline)
(newline)

((L1 'add) 5 't)
(display "L1: ")(newline)
(display "Expected: (a b z c d t)")(newline)
(display "Actual: ")((L1 'print))(newline)
(newline)

;add validity checks
((L1 'add) -1 'z)
(display "L1: ")(newline)
(display "Expected: (a b z c d t)")(newline)
(display "Actual: ")((L1 'print))(newline)
(newline)
((L1 'add) 7 'z)
(display "L1: ")(newline)
(display "Expected: (a b z c d t)")(newline)
(display "Actual: ")((L1 'print))(newline)
(newline)




;size tests
(display "L1 size: ")(newline)
(display "Expected: 6")(newline)
(display "Actual: ")(display ((L1 'size))) (newline)
(display "L2 size: ")(newline)
(display "Expected: 0")(newline)
(display "Actual: ")(display ((L2 'size))) (newline)
(newline)

;get tests
(display "L1[1]: ")(newline)
(display "Expected: b")(newline)
(display "Actual: ")(display ((L1 'get) 1))(newline)
(newline)

(display "L1[5]: ")(newline)
(display "Expected: t")(newline)
(display "Actual: ")(display ((L1 'get) 5))(newline)
(newline)

;get validity checks
(display "L1[-1]: ")(newline)
(display "Expected: #f")(newline)
(display "Actual: ")(display ((L1 'get) -1))(newline)
(newline)
(display "L1[11]: ")(newline)
(display "Expected: #f")(newline)
(display "Actual: ")(display ((L1 'get) 11))(newline)
(newline)

;set tests
((L1 'set) 0 9)
(display "L1[0] set to 9: ")(newline)
(display "Expected: (9 b z c d t)")(newline)
(display "Actual: ")((L1 'print))(newline)
(newline)

((L1 'set) 3 'r)
(display "L1[3] set to r: ")(newline)
(display "Expected: (9 b z r d t)")(newline)
(display "Actual: ")((L1 'print))(newline)
(newline)

;set validity checks
((L1 'set) -4 'a)
(display "L1[-4] set to a: ")(newline)
(display "Expected: (9 b z r d t)")(newline)
(display "Actual: ")((L1 'print))(newline)
(newline)

((L1 'set) 80 255)
(display "L1[80] set to 255: ")(newline)
(display "Expected: (9 b z r d t)")(newline)
(display "Actual: ")((L1 'print))(newline)
(newline)

;delete tests
(display "Removing L1[1]: ")(newline)
(display "Expected Value: b")(newline)
(display "Expected List: (9 z r d t)")(newline)
(display "Actual Value: ")(display ((L1 'delete) 1))(newline)
(display "Actual List: ")((L1 'print))(newline)
(newline)

(display "Removing L1[4]: ")(newline)
(display "Expected Value: t")(newline)
(display "Expected List: (9 z r d)")(newline)
(display "Actual Value: ")(display ((L1 'delete) 4))(newline)
(display "Actual List: ")((L1 'print))(newline)
(newline)

;delete validity checks
(display "Removing L1[-41]: ")(newline)
(display "Expected Value: #f")(newline)
(display "Expected List: (9 z r d)")(newline)
(display "Actual Value: ")(display ((L1 'delete) -41))(newline)
(display "Actual List: ")((L1 'print))(newline)
(newline)

(display "Removing L1[41]: ")(newline)
(display "Expected Value: #f")(newline)
(display "Expected List: (9 z r d)")(newline)
(display "Actual Value: ")(display ((L1 'delete) 41))(newline)
(display "Actual List: ")((L1 'print))(newline)
(newline)