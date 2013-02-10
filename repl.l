; read-eval-print loop.

(define repl (lambda ()
    (print (eval (read)))
    (repl)))

(repl)

