
#lang racket

(provide (all-defined-out))


; question 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

; question 2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

; question 3
(define (list-nth-mod xs n)
  (let ([len-xs (length xs)])
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(= len-xs 0) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n len-xs)))])))

; question 4
(define (stream-for-n-steps s n)
  (if (> n 0)
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))
      null))

; question 5
(define funny-number-stream
  (letrec ([funny-five (lambda (y) (if (= (remainder y 5) 0)
                                        (* y -1)
                                        y))]
           [f (lambda (x) (cons (funny-five x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))
 
; question 6
(define dan-then-dog
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    (lambda () (dan))))

; question 7
(define (stream-add-zero s)
  (letrec ([f (lambda (s) (cons (cons 0 (car (s)))
                                (lambda () (f (cdr (s))))))])
    (lambda () (f s))))

; question 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
                                (lambda () (f (+ n 1)))))])
     (lambda () (f 0))))
           
; question 9
(define (vector-assoc v vec)
  (letrec ([f (lambda (n) (if (< n (vector-length vec))
                              (if (pair? (vector-ref vec n))
                                  (if (equal? (car (vector-ref vec n)) v)
                                      (vector-ref vec n)
                                      (f (+ n 1)))
                                  (f (+ n 1)))
                              #f))])
  (f 0)))

; question 10
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [pos 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v cache)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin (vector-set! cache pos new-ans)
                                   (set! pos (remainder (+ pos 1) n))
                                   new-ans)
                            #f)))))])
    f))

; question 11
(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([expr1 e1]
              [f (lambda ()
                   (let ([e e2])
                     (if (< e expr1) (f) #t)))])
         (f))]))