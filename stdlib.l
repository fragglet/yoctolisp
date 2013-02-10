
; Standard library functions.
; This file is loaded on startup.

(define (null? x) (= x '()))

(define (map callback list)
  (if (null? list)
    '()
    (cons (callback (car list))
          (map callback (cdr list)))))

(define (filter callback list)
  (if (null? list)
    '()
    (if (callback (car list))
      (cons (car list)
            (filter callback (cdr list)))
      (filter callback (cdr list)))))

(define (for-each callback list)
  (if (null? list)
    '()
    (begin (callback (car list))
           (for-each callback (cdr list)))))

(define (length list)
  (if (null? list)
    0
    (+ 1 (length (cdr list)))))

(define (<= x y) (not (> x y)))
(define (>= x y) (not (< x y)))

(define (min x y) (if (< x y) x y))
(define (max x y) (if (> x y) x y))

