(define adder
  (lambda (x)
    (lambda (y)
      (+ x y))))

(define plus-1 (adder 1))

(print (plus-1 2))
