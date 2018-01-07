#lang racket


(define (evaluate e env)
  (if (atom? e) ; (atom? e) == (not (pair? e))
    (cond [(symbol? e) (lookup e env)]
          [(or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e]
          [else (error "Cannot evaluate" e)])
    (error "Lists not handled yet")))


(define (lookup e env)
  e)

(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))
