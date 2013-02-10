; read-eval-print loop.

(define repl
  (lambda ()
    (let ((value (read)))
      (if (null? value)
        '()
	(begin (print (eval value))
	       (repl))))))

(repl)

