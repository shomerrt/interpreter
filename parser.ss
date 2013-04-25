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
   (body (list-of expression?)))
  (begin-exp
    (body (list-of expression?))
  )
  (app-exp
   (exps (list-of expression?)))
  (exit-exp
  	(val number?)
  )
  (while-exp
  	(test expression?)
  	(body (list-of expression?))
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
			   (map parse-expression (cddr datum))
			)
		]
		[(eqv? (car datum) 'set!)
		  (set-exp (cadr datum) (parse-expression (caddr datum)))]
		[(eqv? (car datum) 'if)
		  (if-exp (parse-expression (cadr datum))
			  (parse-expression (caddr datum))
			  (parse-expression (cadddr datum)))]
		[(eqv? (car datum) 'quote)
		  (lit-exp (cadr datum))]
		[(eqv? (car datum) 'begin)
            (if (null? (cdr datum))
              (eopl:error 'parse-expression "20. Begin is broken" datum)
              (begin-exp
                (map parse-expression (cdr datum))
              )
            )
        ]
        [(eqv? (car datum) 'while)
        	(if (null? (cdr datum))
        		(eopl:error 'parse-expression "1. While is not proper" datum)
        		(while-exp
        			(parse-expression (cadr datum))
        			(map parse-expression (cddr datum))
        		)
        	)
       	]
		[else (app-exp
			(map parse-expression datum))])]
	  [else (eopl:error 'parse-expression
			    "Invalid concrete syntax ~s" datum)])))

(define-syntax for
	(syntax-rules (:)
		[(_ (init-exp : test-exp : update-exp) body-exp)
			(begin init-exp
				(let loop []
					(when test-exp 
						(begin body-exp update-exp (loop))
					)
				)
			)
		]
	)
)

(define-syntax return-first
	(syntax-rules ()
		[(_ n1 n2 ...)
			(let ([first n1])
				(begin n2 ... first)
			)
		]
	)
)


