#lang racket

(define-syntax time-expression
  (syntax-rules ()
    ((_ e) (begin (collect-garbage)
                  (time e)
                  #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List membership
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Return #t if x is a member of the list ys, #f otherwise. Equality is
;; determined using equal?
;;

(define (member? x ys)
  (cond [(null? ys)            #f]
        [(equal? x (first ys)) #t]
        [else                  (member? x (rest ys))]))

;; What if we swap the first two conditions?
;; What if we use length instead of null?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reverse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; Write a function that reverses a list
;

;; (append xs ys)

(define (reverse xs)
  (if (null? xs)
      null
      (append (reverse (rest xs)) (list (first xs)))))

(define (reverse2 xs)
  (define (helper xs acc)
    (if (null? xs)
        acc
        (helper (rest xs) (cons (first xs) acc))))
  (helper xs null))

;
; What about performance?
;
(define (repeat x n)
  (if (= n 0)
      null
      (cons x (repeat x (- n 1)))))

(define long-list (repeat 1 10000))

;; Try: (time-expression (reverse long-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exponent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; Define a function that computes the integer exponent of a number
;

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

(define (expt2 b n)
  (define (helper b counter product)
    (if (= counter 0)
        product
        (helper b (- counter 1) (* b product))))
  (helper b n 1))

(define (fast-expt b n)
  (cond [(= n 0)   1]
        [(even? n) (square (fast-expt b (/ n 2)))]
        [else      (* b (fast-expt b (- n 1)))]))

(define (square x) (* x x))

;
; What about performance?
;

;; Try: (time-expression (expt 2 100000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let Over Lambda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lambda-computer)
  (let ((y 1))
    (lambda (x)
      (+ y x))))

(define procedure-computer
  (lambda (y)
    (define (inc x)
      (+ y x))
    inc))
