(define (empty? s)
(if (null? s) #t #f)
)

(define (set s)
  (if (empty? s) '()
      (if (not (in? (car s) (cdr s))) (cons (car s)(set (cdr s)))
          (set (cdr s))))
)

(define (in? e s)
(if (empty? s) #f
    (if( eq? e (car s)) #t
       (in? e (cdr s))))
)

(define (add e s)
(if (not (in? e s)) (cons e s) s)
)

(define (discard e s)
  (if (empty? s) '()
      (if (eq? e (car s)) (cdr s) (cons (car s) (discard e (cdr s))))) 
)       

(define (union s1 s2)
(if(empty? s1) s2
   (if (in? (car s1) s2) (union (cdr s1) s2) (cons (car s1) (union (cdr s1) s2))))
  
)

(define (intersection s1 s2)
 (if(empty? s1) '()
    (if (in? (car s1) s2) (cons (car s1) (intersection (cdr s1) s2))
        (intersection (cdr s1) s2)))
)

(define (difference s1 s2)
 (if (empty? s1) '()
     (if (in? (car s1) s2) (difference (cdr s1) s2) (cons (car s1) (difference (cdr s1) s2))))
)

(define (symmetric-difference s1 s2)
 (difference (union s1 s2) (intersection s1 s2))
)

(define (subset? s1 s2)
 (if (empty? s1) #t
     (if (not (in? (car s1) s2)) #f (subset? (cdr s1) s2)))
)
          
(define (superset? s1 s2)
 (if (empty? s2) #t
     (if (not(in? (car s2) s1)) #f
     (superset? s1 (cdr s2))))
)

(define (disjoint? s1 s2)
 (if (empty? (intersection s1 s2)) #t #f) 
)

(define (sameset? s1 s2)
  (if (and (empty? (difference s1 s2)) (empty? ( difference s2 s1))) #t #f)
)


; some tests
(define A (set '(1 2 7 9 7 1)))
(define B (set '(2 0 8 0 7 12)))
(define C (set '(9 7)))

(define colors (set '("yellow" "red" "green" "blue" "orange" "purple" "pink")))
(define rgb (set '("red" "green" "blue")))

(define hi (set '(#\h #\i)))

(empty? A) ; #f
(empty? rgb) ;#f
(empty? (set'())) ;#t

(in? 0 A) ; #f
(in? "red" A); #f
(in? 2 A) ; #t

(in? "green" rgb) ; #t
(in? "purple" rgb) ; #f
(in? "i" hi) ;#f
(in? #\i hi) ;#t

(add 9 A) ; (2 9 7 1)
(add 5 A) ; (5 2 9 7 1)

(discard 1 A) ; (2 9 7)
(discard 5 A) ; (2 9 7 1)
(union A B) ; (9 1 2 8 0 7 12)
(union A rgb) ; (2 9 7 1 "red" "green" "blue")

(intersection A rgb) ; ()
(intersection A B) ; (2 7)
(intersection rgb colors) ; ("red" "green" "blue")

(difference A B) ; (9 1)
(difference rgb colors) ; ()
(difference colors rgb) ; ("yellow" "orange" "purple" "pink")

(symmetric-difference A B) ; (9 1 8 0 12)
(symmetric-difference A C) ; (2 1)
(symmetric-difference colors rgb) ; ("yellow" "orange" "purple" "pink")

(subset? A B) ;#f
(subset? C A) ; #t

(subset? colors rgb) ;#f
(subset? rgb colors)  ; #t

(superset? A B) ;#f
(superset?  A C) ; #t

(superset? colors rgb) ;#t
(superset? rgb colors)  ; #f

(disjoint? B C) ;#f
(disjoint? colors A) ;#t

(sameset? (set '(9 1 2 7)) A); #t
(sameset? B A) ; #f