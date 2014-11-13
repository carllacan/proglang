
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; Problem 1
(define (sequence low high stride)
  (cond [(< high low) null]
        [(< (- high low) stride) (cons low null)]
        [#t (cons low (sequence (+ low stride) high stride))]))

; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

; Problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; Problem 4
(define (stream-for-n-steps s n) 
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

; Problem 5
(define  funny-number-stream
  (letrec ([f (lambda (x) (cons x (lambda () 
                                    (if (= (remainder (+ 1 x) 5) 0)(f (-(+ (abs x) 1)))(f (+ (abs x) 1))))))])
  (lambda () (f 1))))

; Problem 6
(define dan-then-dog
  (letrec ([f (lambda (x) (cons x (lambda () 
                                    (if (string=? "dog.jpg" x)(f "dan.jpg")(f "dog.jpg")))))])                               
  (lambda () (f "dan.jpg"))))

; Problem 7
(define (stream-add-zero s)
  (letrec ([f (lambda(x)  
              (cons  (cons 0 (car (x))) (lambda () (f (cdr (x))))))])
    
  (lambda() (f s))))
  

; Problem 8
(define (cycle-lists xs ys)
  (letrec ([find (lambda(e ls) (if(equal? e (car ls))0(+ 1 (find e (cdr ls)))))]
           ;[get-nth (lambda(ls n) (if (= n 0) (car ls) ((get-nth (cdr ls) (- n 1)))))]
           [f (lambda (x) (cons x (lambda () 
                                   (f (cons (list-nth-mod xs (+ (find (car x) xs) 1)) (list-nth-mod ys (+ (find (cdr x) ys) 1)))))))])  
  (lambda () (f (cons (car xs)(car ys))))))


; Problem 9
(define (vector-assoc v vec)
   
   (cond [(= 0 (vector-length vec)) #f]
         [(not (pair? (vector-ref vec 0))) (vector-assoc v (list->vector (cdr (vector->list vec))))]
         [(equal? v (car (vector-ref vec 0))) (vector-ref vec 0)]       
         [#t (vector-assoc v (list->vector (cdr (vector->list vec))))]))
    

; Problem 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [nxt 0]
           [add-to-cache (lambda(v)  (let ([r (assoc v xs)])
                                        (vector-set! cache nxt r)
                                        (set! nxt (remainder (+ 1 nxt) n))
                                        r
                                        ))])
  (lambda(v) (if (vector-assoc v cache)
                 (vector-assoc v cache)
                 (add-to-cache v)
              ))))
    

