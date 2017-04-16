;;; CS 152 Homework 4 - A simple chatbot
;;; starter code

;;; We'll use the random function implemented in Racket
;;; (random k) returns a random integer in the range 0 to k-1
(#%require (only racket/base random))

;;; some input and output helper functions

;;; prompt:  prompt the user for input
;;; return the input as a list of symbols
(define (prompt)
   (newline)
   (display "talk to me >>>")
   (read-line))

;;; read-line: read the user input till the eof character
;;; return the input as a list of symbols
(define (read-line)
  (let ((next (read)))
    (if (eof-object? next)
        '()
        (cons next (read-line)))))

;;; output: take a list such as '(how are you?) and display it
(define (output lst)
       (newline)
       (display (to-string lst))
       (newline))

;;; to-string: convert a list such as '(how are you?)
;;; to the string  "how are you?"
(define (to-string lst)       
  (cond ((null? lst) "")
        ((eq? (length lst) 1) (symbol->string (car lst)))
        (else (string-append (symbol->string (car lst))
                              " "
                             (to-string (cdr lst))))))


;;;  main function
;;;  usage:  (chat-with 'your-name)

(define (chat-with name)
  (output (list 'hi name))
  (chat-loop name))

;;; chat loop
(define (chat-loop name)
  (let ((input (prompt))) ; get the user input
    (if (eqv? (car input) 'bye)
        (begin
          (output (list 'bye name))
          (output (list 'have 'a 'great 'day!)))
        (begin
	  (reply input name)
          (chat-loop name)))))


;;; your task is to fill in the code for the reply function
;;; to implement rules 1 through 11 with the required priority
;;; each non-trivial rule must be implemented in a separate function
;;; define any helper functions you need below
(define (reply input name)
     (cond
       ((or (eqv? (car input) 'do) (eqv? (car input) 'can) (eqv? (car input) 'will) (eqv? (car input) 'would))
        (begin
         (output (pick-random (yes-no-answer name (change-pronouns input))))))
       ((>= (length (intersection input special-topics)) 2)
        (begin
          (output (special-topic-answer input name))))
       ((eqv? (car input) 'why)
        (begin
          (output (pick-random why-answer))))
       ((eqv? (car input) 'how)
        (begin 
         (output (pick-random how-answer))))
       ((eqv? (car input) 'what)
        (begin
          (output (pick-random what-answer))))
       ((eqv? (last-character (symbol->string (last-word input))) #\?)
        (begin
          (output (pick-random any-question))))
       ((eqv? (find-because input) 'because)
        (begin
          (output (pick-random because-answer))))
       (( and (eqv? (car input) 'i) (or (eqv? (car (cdr input)) 'need) (eqv? (car (cdr input)) 'have)
                                        (eqv? (car (cdr input)) 'want) (eqv? (car (cdr input)) 'think)))
        (begin
          (output (think-need-answer (change-pronouns input)))))
       ((and (eqv? (car input) 'i) (not (eqv? (last-word input) 'too)))
        (begin
          (output (merge input '(too)))))
       (( or (eqv? (car input) 'tell) (eqv? (car input) 'give) (eqv? (car input) 'say))
        (begin
          (output (append '(you) input))))
       (else
         (output (pick-random generic-response))))) ; rule 11 has been implemented for you

;;; pick one random element from the list choices
(define (pick-random choices)
  (list-ref choices (random (length choices))))

;;; generic responses for rule 11
(define generic-response '((that\'s nice)
                           (good to know)
                           (can you elaborate on that?)))

;;; get the response if sentence starts with how
(define how-answer '((why do you ask?)
                     (how would an answer to that help you?)))

;;; get the response if sentence starts with why
(define why-answer '((why not?)))

;;; get the response if sentence starts with what
(define what-answer '((what do you think?)
                      (why do you ask?)))

;;; this function finds the because word in the sentence
(define (find-because lst)
  (cond
    ((null? lst) "")
    ((eqv? (car lst) 'because) 'because)
        (else (find-because (cdr lst)))))

;;; get the response if sentence has word because in it
(define because-answer '((is that the real reason?)))

;;; return the last word of a sentence
(define (last-word lst)
  (if (= (length lst) 1) (car lst)
      (last-word (cdr lst))))

;;; merge two list into one
(define (merge lst1 lst2)
  (append lst1 lst2))

;;; get the last character of a string
(define (last-character str)
  (string-ref str(- (string-length str) 1)))

;;; checks if list is empty
(define (empty? s)
(if (null? s) #t #f)
)

;;; checks if the items is in the list
(define (in? e s)
(if (empty? s) #f
    (if( eq? e (car s)) #t
       (in? e (cdr s))))
)
;;; get the first word of the list
(define (first-word lst)
  (car lst))

;;; get the response if sentence starts with do, can, will and would
(define (yes-no-answer name lst) (list (append '(yes i) (cons (car lst) '())) (no-answer name lst)))

;;; get the response with answer 'no'
(define (no-answer name lst)
(append (list 'no name) '(i) (cons (car lst) '()) '(not) (remove-last(cddr lst)) (cons (remove-? lst) '())))

;;; get the response for any question                                   
(define any-question '((i don\'t know) (i have no idea) (i have no clue) (maybe)))

;;; converting character into a symbol
(define (char->symbol ch)
  (string->symbol (string ch)))

;;;get response for need, think, have and want
(define (think-need-answer lst)
  list (append '(why do you) (cons (car (cdr lst)) '()) (remove-last(cddr lst)) (cons (add-? lst) '() )))

;;; change the pronouns
(define (change-pronouns lst)
  (cond
    ((null? lst) '())
    ((eq? (car lst) 'I) (cons 'you (change-pronouns (cdr lst))))
    ((eq? (car lst) 'am) (cons 'are (change-pronouns (cdr lst))))
    ((eq? (car lst) 'my) (cons 'your (change-pronouns (cdr lst))))
    ((eq? (car lst) 'your) (cons 'my (change-pronouns (cdr lst))))
    ((eq? (car lst) 'me) (cons 'you (change-pronouns (cdr lst))))
    ((eq? (car lst) 'you) (cons 'me (change-pronouns (cdr lst))))
    ((eq? (car lst) 'me?) (cons 'you? (change-pronouns (cdr lst))))
    (else
      (cons (car lst) (change-pronouns (cdr lst))))))

;;;remove ? mark
(define (remove-? lst)
  (string->symbol (substring (to-string (list (last-word lst))) 0 (- (string-length
                                                                      (to-string (list (last-word lst)))) 1))))
;;; remove last element of the list
(define (remove-last lst)
    (if (null? (cdr lst))
        '()
        (cons (car lst) (remove-last (cdr lst)))))

;;;add ? at the end of the sentence
(define (add-? lst)
  (string->symbol (string-append (to-string (list (last-word lst))) "?")))

;;;special topic list
(define special-topics '(family friends friend mom dad brother sister girlfriend boyfriend children  son
                               daughter child wife husband home dog cat pet))

;;; find the intersection
(define (intersection s1 s2)
 (if(empty? s1) '()
    (if (in? (car s1) s2) (cons (car s1) (intersection (cdr s1) s2))
        (intersection (cdr s1) s2))))

;;; get random element from list
(define (random-element list)
  (list-ref list (random (length list))))

;;;get the response for special topics
(define (special-topic-answer input name)
 (append '(tell me more about your) (cons (random-element
                                           (intersection input special-topics)) '()) (cons name '())))
    









