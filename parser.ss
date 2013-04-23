
(define-datatype expression expression? 
  (lit-exp
   (value scheme-value?))
  (var-exp
   (id symbol?))
  (set-exp
   (id symbol?)
   (val expression?))
  (if-exp
   (test-exp expression?)
   (true-exp expression?)
   (false-exp expression?))
  (lambda-exp
   (ids (list-of symbol?))
   (body expression?))
  (let-exp
   (ids (list-of symbol?))
   (vals (list-of expression?))
   (body expression?))
  (app-exp
   (exps (list-of expression?)))
  (exit-exp
  	(val number?)
  )
  (for-exp
  	(test expression?)
  	(update expression?)
  	(return expression?)
  	(body expression?)
  )
)

(define scheme-value? (lambda (v) #t))

(define parse-expression
  (lambda (datum)
    (cond [(symbol? datum) (var-exp datum)]
	  [(number? datum) (lit-exp datum)]
	  [(string? datum) (lit-exp datum)]
	  [(boolean? datum) (lit-exp datum)]	  	  
	  [(null? datum) (lit-exp datum)]
	  [(pair? datum)
	   (cond 
	   	[(eqv? (car datum) 'lambda)
		  (lambda-exp (cadr datum)
			      (parse-expression (caddr datum)))]
		[(eqv? (car datum) 'exit)
			(exit-exp
				(cadr datum)
			)
		]
		[(eqv? (car datum) 'let)
		  (let-exp (map car (cadr datum))
			   (map parse-expression (map cadr (cadr datum)))
			   (parse-expression (caddr datum)))]
		[(eqv? (car datum) 'set!)
		  (set-exp (cadr datum) (parse-expression (caddr datum)))]
		[(eqv? (car datum) 'if)
		  (if-exp (parse-expression (cadr datum))
			  (parse-expression (caddr datum))
			  (parse-expression (cadddr datum)))]
		[(eqv? (car datum) 'quote)
		  (lit-exp (cadr datum))]
		[(eqv? (car datum) 'for)
			
				(if (< (length datum) 2) 
					(eopl:error 'parse-expression "2. Improper for" datum)
					(if (or (or (null? (cadr datum)) (number? (cadr datum))) (number? (caadr datum)))
						(eopl:error 'parse-expression "2. Improper for" datum)
						(for-exp 
							(parse-expression (caadr datum))
							(parse-expression (cadr (cadr datum)))
							(parse-expression (caddr (cadr datum)))
							(if (not (null? (cddr datum)))
								(parse-expression (car (cddr datum)))
								(parse-expression '())
							)
						)
					)
				)
		]	
		[else (app-exp
			(map parse-expression datum))])]
	  [else (eopl:error 'parse-expression
			    "Invalid concrete syntax ~s" datum)])))


