#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high stride)
    (if (> low high)
        null
        (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(null? xs) (error "list-nth-mod: empty list")]
        [(< n 0) (error "list-nth-mod: negative number")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pair (s)])
        (cons (car pair) (stream-for-n-steps (cdr pair) (- n 1))))))

  
(define funny-number-stream
  (letrec ([f (lambda (x) (if (= (remainder x 5) 0)
                             (cons (- 0 x) (lambda () (f (+ x 1))))
                             (cons x (lambda () (f (+ x 1))))
                             ))])
    (lambda() (f 1))))

(define dan-then-dog
  (letrec ([f (lambda(s) (let ([next-string (if (string=? s "dog.jpg")
                                                "dan.jpg"
                                                "dog.jpg")])
                           (cons s (lambda () (f next-string)))))])
    (lambda () (f "dan.jpg"))))

(define (stream-add-zero s)
  (letrec ([pair (s)]
           [curr-element (car pair)]
           [next-s (cdr pair)])
   (lambda () (cons (cons 0 curr-element) (stream-add-zero next-s)))
   ))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (x) (cons (cons (list-nth-mod xs x) (list-nth-mod ys x)) (lambda() (f (+ x 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda(num) 
                (if (= (vector-length vec) num)
                    #f
                    (let ([val-element (vector-ref vec num)])
                      (cond [(not (pair? val-element)) (f (+ num 1))]
                            [(equal? (car val-element) v) val-element]
                            [#t (f (+ num 1))]))))])
    (f 0)))


(define (cached-assoc xs n)
  (letrec ([mamo (make-vector n #f)]
           [trace 0]
           [f (lambda (v) (let ([result (vector-assoc v mamo)])
                            (if result
                                result
                                (let ([new-result (assoc v xs)])
                                  (begin (vector-set! mamo (remainder trace n) new-result)
                                         (set! trace (+ trace 1))
                                         new-result)))))])
    f))
