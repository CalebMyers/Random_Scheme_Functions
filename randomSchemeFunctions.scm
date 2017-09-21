;***************************************************
;
; Program: Homework 5 -- Scheme Functions
;
; Class:   CS 3200 -- Professor Abukamail
;
; Author:  Caleb Myers 
; Email:   cm346613@ohio.edu
;
; Description: This program contains several scheme 
;              functions and helper functions as 
;              asked on the homwork 5 document. 
;
; Date:        April 9, 2017
;
;***************************************************


;************* QUESTON 1 -- Extract List *************; 

;************************
; Function: extract-lists
;
; Parameter(s): lst - a list
;
; Purpose: returns a list that contains
;          all the lists in lst
;
; Calls: extract-lists
;
;************************
(define (extract-lists lst)
	(if (null? lst)
		(list)
		(if (list? (car lst))
		    (cons (car lst) (extract-lists (cdr lst)))
		    (extract-lists (cdr lst)) 
		)
	)
)


;************* QUESTON 2 -- Count Evens *************; 

;************************
; Function: is-even
;
; Parameter(s): n - an integer
;
; Purpose: returns 1 if n is even,
;          0 if not
;
; Calls: none
;
;************************

(define (is-even n)
	(if ( = (remainder n 2) 0) 1 0)
)

;************************
; Function: evens
;
; Parameter(s): lst - a list of integers
;
; Purpose: returns the number of even numbers
;          in lst
;
; Calls: is-even
;        evens
;
;************************
(define (count-evens lst)
	(if (null? lst)
		0
	    (+ (is-even (car lst)) (evens (cdr lst)))
	)
)


;************* QUESTON 3 -- Create Pairs *************;

;************************
; Function: create-pairs
;
; Parameter(s): lst1 - a list
;               lst2 - a list
;
; Purpose: returns a list of pairs of elements
;          from each list at the same index.
;          If the lists are not evenly lengthed
;          or the lists are empty an empty list
;          is returned
;
; Calls: create-pairs
;
;************************
(define (create-pairs lst1 lst2)
    (if (= (length lst1) (length lst2))
    	(if (null? lst1)
    		(list)
            (cons (list (car lst1) (car lst2)) (create-pairs (cdr lst1) (cdr lst2)))
    	)
    	(list)
    ) 
)


;************* QUESTON 4 -- Cross Product *************;

;************************
; Function: pairs
;
; Parameter(s): atom - an atom
;               lst  - a list
;
; Purpose: returns a list of pairs of atom 
;          with all elements of lst
;
; Calls: pairs
;
;************************
(define (pairs atom lst)
	(if (null? lst)
		(list)
		(cons (list atom (car lst)) (pairs atom (cdr lst)))
	)
)

;************************
; Function: product
;
; Parameter(s): lst1 - a list
;               lst2 - a list
;
; Purpose: returns a list of the cross
;          product of lst1 and lst2
;
; Calls: member
;
;************************
(define (product lst1 lst2)
	(if (null? lst1)
		(list)
		(append (pairs (car lst1) lst2) (product (cdr lst1) lst2))
	)
)


;************* QUESTON 5 -- Remove Duplicates *************;

;************************
; Function: member
;
; Parameter(s): atom - an atom
;               lst  - a list
;
; Purpose: returns true if atom is in lst,
;          false if not
;
; Calls: member
;
;************************
(define (member atom lst)
	(cond ((null? lst) #f)
		  ((equal? atom (car lst)) #t)
		  (else (member atom (cdr lst)))
	)
)

;************************
; Function: add-to-set
;
; Parameter(s): lst - a list
;               set - a set
;
; Purpose: returns a set of set combined
;          with the elements of list
;
; Calls: member
;        add-to-set
;
;************************
(define (add-to-set lst set)
	(if (null? lst)
	    (list) 
    	(if (member (car lst) set)
    		(add-to-set (cdr lst) set)
    		(cons (car lst) (add-to-set (cdr lst) (cons (car lst) (append set (list (car lst))))))
        )
    )
)

;************************
; Function: remove-duplicates
;
; Parameter(s): lst - a list
;
; Purpose: returns a list with elements of 
;          lst without duplicates
;
; Calls: add-to-set
;
;************************
(define (remove-duplicates lst)
    (add-to-set lst '())
)


;************* QUESTON 6 -- Fibonacci Numbers *************;

;************************
; Function: fib
;
; Parameter(s): num - and integer
;
; Purpose: returns returns the numth element
;          of the Fibonacci numbers
;
; Calls: fib
;
;************************
(define (fib num)
	(if (<= num 2)
		1
		(+ (fib (- num 1)) (fib (- num 2)))
	)
)
