
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
; Problem 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

; Problem 2
(define (string-append-map xs suffix)
  (map (lambda (e) (string-append e suffix)) xs))

; Problem 3
(define (list-nth-mod xs n)
  (cond [(< n 0) (raise "list-nth-mod: negative number")]
        [(null? xs) (raise "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

; Problem 4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

; Problem 5
(define funny-number-stream
  (lambda ()
    (letrec ([nested-stream
              (lambda (x)
                (cons
                 (if (= (remainder x 5) 0)
                     (- 0 x)
                     x)
                 (lambda () (nested-stream (+ x 1)))))])
      (nested-stream 1))))

; Problem 6
(define dan-then-dog
  (lambda () (cons "dan.jpg"
                   (lambda ()
                     (cons "dog.jpg" dan-then-dog)))))

; Problem 7
(define (stream-add-zero s)
  (letrec ([nested-stream
         (lambda () (cons (cons 0 (car (s))) nested-stream))])
    nested-stream))

; Problem 8
(define (cycle-lists xs ys)
  (letrec ([s (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (s (+ n 1)))))])
    (lambda () (s 0))))

; Problem 9
(define (vector-assoc v vec)
  (letrec ([helper (lambda (v vec n)
                     (if (< n (vector-length vec))
                         (if (pair? (vector-ref vec n))
                             (if (= v (car (vector-ref vec n)))
                                 (vector-ref vec n)
                                 (helper v vec (+ n 1)))
                             (helper v vec (+ n 1)))
                         #f))])
    (helper v vec 0)))

; Problem 10
(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [indicator 0]
           [helper (lambda (v)
                     (let ([answer (vector-assoc v memo)])
                       (if answer
                           (let ([new-answer (assoc v xs)])
                             (if new-answer
                                 (begin (vector-set! memo indicator new-answer)
                                        (set! indicator (remainder (+ indicator 1) n))
                                        new-answer)
                                 new-answer))
                           answer)))])
    helper))
