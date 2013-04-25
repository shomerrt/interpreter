(load "chez-init.ss")
(load "interpreter.ss")
(load "env.ss")
(load "parser.ss")

(define (rl) (load "main.ss"))

(define (rep)
  (begin
    (display "--> ")
    (let ([foo (read)])
      (if (not (equal? foo '(exit)))
	  (begin (write (interpret foo))
		 (newline)
		 (rep))))))