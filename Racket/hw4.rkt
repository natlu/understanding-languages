
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

        