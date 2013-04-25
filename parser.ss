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
   (body (list-of expression?))
  )
  (let-exp
   (ids (list-of symbol?))
   (vals (list-of expression?))
   (body (list-of expression?))
  )
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
  (and-exp
  	(body (list-of expression?))
  )
  (cond-exp
   (tests (list-of expression?))
   (expr (list-of expression?)))
  (case-exp
   (pkey expression?)
   (clauses (list-of expression?)))
  (clause-exp
   (key expression?)
   (body expression?))
  (or-exp
   (body (list-of expression?)))
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
		  	(lambda-exp 
		  		(cadr datum)
			    (map parse-expression (cddr datum))
			)
		]
		[(eqv? (car datum) 'cond)
		 (cond-exp (map parse-expression (map car (cdr datum)))
			   (map parse-expression (map cadr (cdr datum))))]
		[(eqv? (car datum) 'case)
		 (case-exp  (parse-expression (cadr datum))
			    (map clause-exp 
			     (map parse-expression (map car (cddr datum)))
			     (map parse-expression (map cadr (cddr datum)))))]
		[(eqv? (car datum) 'exit)
			(exit-exp
				(cadr datum)
			)
		]
		[(eqv? (car datum) 'or)
		 (or-exp (map parse-expression (cdr datum)))]
		[(eqv? (car datum) 'and)
			(and-exp
				(map parse-expression (cdr datum))
			)
		]
		[(eqv? (car datum) 'let)
			(let-exp (map car (cadr datum))
			   (map parse-expression (map cadr (cadr datum)))
			   (map parse-expression (cddr datum))
			)
		]
		[(eqv? (car datum) 'let*)
			(let-star-parser (cadr datum) (cddr datum))
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

(define let-star-parser
	(lambda (bindings body)
		(if (null? bindings)
			(let-exp
				(map car bindings)
				(map parse-expression (map cadr bindings))
				(map parse-expression body)
			)
			(let-exp
				(map car (list (car bindings)))
				(map parse-expression (map cadr (list (car bindings))))
				(list (let-star-parser (cdr bindings) body))
			)	
		)
		
	)
)

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


