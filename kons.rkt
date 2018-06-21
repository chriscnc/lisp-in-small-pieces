#lang racket

(define (kons a d)
  (lambda (msg)
    (case msg
      ((car) a)
      ((cdr) d)
      ((set-car!) (lambda (newv) (set! a newv)))
      ((set-cdr!) (lambda (newv) (set! d newv))))))

(define (kar pair)
  (pair 'car))

(define (set-kdr! pair value)
  ((pair 'set-cdr!) value))
