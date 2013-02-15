
; Copyright (c) 2013 Simon Howard
;
; Permission to use, copy, modify, and/or distribute this software
; for any purpose with or without fee is hereby granted, provided
; that the above copyright notice and this permission notice appear
; in all copies.
;
; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
; CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
; LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
; NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

; Yoctolisp standard library functions.
; This file is loaded on startup.

(define (list . values) values)

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

(define (foldr func acc list)
  (if (null? list)
    acc
    (func (car list)
          (foldr func acc (cdr list)))))

(define (reduce func list)
  (foldl func (car list) (cdr list)))

(define (apply func args)
  (eval
    (cons func
          (map (lambda (x) (list 'quote x))
               args))))

(define (curry func x)
  (lambda args (apply func (cons x args))))

(define (compose f g)
  (lambda args (f (apply g args))))

(define (primitive-append a b)
  (if (null? a)
    b
    (cons (car a)
          (append (cdr a) b))))

(define (append . args)
  (foldr primitive-append '() args))

(define (length list)
  (foldl (lambda (x) (+ x 1)) 0 list))

(define (sum . list) (foldl + 0 list))
(define (product . list) (foldl * 1 list))

(define (sort l)
  (if (null? l)
    l
    (let ((pivot (car l))
          (left (filter (lambda (x) (< x pivot)) (cdr l)))
          (right (filter (lambda (x) (>= x pivot)) (cdr l))))
      (append (sort left)
              (list pivot)
              (sort right)))))

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

