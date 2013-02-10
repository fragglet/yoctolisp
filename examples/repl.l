; read-eval-print loop.

(define (repl)
  (let ((value (read)))
    (if (null? value)
      '()
      (begin (print (eval value))
             (repl)))))

(repl)

