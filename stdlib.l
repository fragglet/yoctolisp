
; Standard library functions.
; This file is loaded on startup.

(define null?  (lambda (x) (= x '())))

(define map
  (lambda (callback list)
    (if (null? list)
      '()
      (cons (callback (car list))
            (map callback (cdr list))))))

(define filter
  (lambda (callback list)
    (if (null? list)
      '()
      (if (callback (car list))
        (cons (car list)
              (filter callback (cdr list)))
        (filter callback (cdr list))))))

(define for-each
  (lambda (callback list)
    (if (null? list)
      '()
      (begin (callback (car list))
             (for-each callback (cdr list))))))

(define length
  (lambda (list)
    (if (null? list)
      0
      (+ 1 (length (cdr list))))))

(define <= (lambda (x y) (not (> x y))))
(define >= (lambda (x y) (not (< x y))))

(define min (lambda (x y) (if (< x y) x y)
(define max (lambda (x y) (if (> x y) x y)

