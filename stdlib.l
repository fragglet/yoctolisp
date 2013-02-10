
; Yoctolisp standard library functions.
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

(define (foldl func acc list)
  (if (null? list)
    acc
    (foldl func
           (func acc (car list))
           (cdr list))))

(define (length list)
  (foldl (lambda (x) (+ x 1)) 0 list))

(define (sum . list) (foldl + 0 list))
(define (product . list) (foldl * 1 list))

(define (reverse-with-ending list ending)
  (if (null? list)
    ending
    (reverse-with-ending
      (cdr list)
      (cons (car list) ending))))

(define (reverse list)
  (reverse-with-ending list '()))

(define (<= x y) (not (> x y)))
(define (>= x y) (not (< x y)))

(define (min first . list)
  (foldl (lambda (x y) (if (< x y) x y))
         first
	 list))

(define (max first . list)
  (foldl (lambda (x y) (if (> x y) x y))
         first
	 list))

(define (id x) x)

(define (cadr x) (car (cdr x)))
(define (caar x) (car (car x)))
(define (cddr x) (cdr (cdr x)))
(define (cdar x) (cdr (car x)))

(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))

