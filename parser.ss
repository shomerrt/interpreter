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
  (if2-exp
  	(test-exp expression?)
  	(true-exp expression?))
  (lambda-exp
   (ids (list-of symbol?))
   (body (list-of expression?)))
  (lambda2-exp
  	(id (list-of symbol?))
    (body (list-of expression?))
  )
  (named-let-exp
  	(name symbol?)
  	(ids (list-of symbol?))
  	(vals (list-of expression?))
  	(body (list-of expression?))
  )
  (lambda3-exp
  	(id (list-of symbol?))
  	(body (list-of expression?))
  )
  (let-exp
   (ids (list-of symbol?))
   (vals (list-of expression?))
   (body (list-of expression?)))
  (begin-exp
   (body (list-of expression?)))
  (app-exp
  	(rator expression?)
  	(rands (list-of expression?)))
  (exit-exp
   (val number?))
  (while-exp
   (test expression?)
   (body (list-of expression?)))
  (and-exp
   (body (list-of expression?)))
  (cond-exp
   (tests (list-of expression?))
   (bodies (list-of expression?)))
  (case-exp
   (pkey expression?)
   (tests (list-of (lambda (x) (expression? (car x)))))
   (exprs (list-of expression?)))
  (clause-exp
   (key expression?)
   (body expression?))
  (or-exp
   (body (list-of expression?)))
  (define-exp
  	(id symbol?)
  	(body expression?)
  )
  (letrec-exp
   (ids (list-of symbol?))
   (vals (list-of expression?))
   (body (list-of expression?)))
  (call/cc-exp
  	(id expression?))
)

(define scheme-value? (lambda (v) #t))

(define map-case
  (lambda (x)
    (if (eqv? 'else (car x))
	(list (parse-expression (car x)))
	(map parse-expression (car x)))))

(define properize
	(lambda (ls)
		(if (pair? (cdr ls))
			(cons (car ls) (properize (cdr ls)))
			(list (car ls) (cdr ls))
		)
	)
)
(define parse-expression
  (lambda (datum)
    (cond [(symbol? datum) (var-exp datum)]
	  [(number? datum) (lit-exp datum)]
	  [(string? datum) (lit-exp datum)]
	  [(boolean? datum) (lit-exp datum)]	  	  
	  [(null? datum) (lit-exp datum)]
	  [(vector? datum) (lit-exp datum)]

	  [(pair? datum)
	   (cond 
	   	[(eqv? (car datum) 'lambda)
	   		(if (pair? (cadr datum))
	   			(if (list? (cadr datum))
	   				(lambda-exp 
				  		(cadr datum)
					    (map parse-expression (cddr datum))
					)
					(lambda3-exp
						(properize (cadr datum))
						(map parse-expression (cddr datum))
					)
	   			)
				(lambda2-exp
					(list (cadr datum))
                    (map parse-expression (cddr datum))
				)
	   		)
		  	
		]
		[(eqv? (car datum) 'call/cc)
  				(call/cc-exp (parse-expression (cadr datum)))]
		[(eqv? (car datum) 'cond)
		 (cond-exp (map parse-expression (map car (cdr datum)))
			   (map parse-expression (map cadr (cdr datum))))]
		[(eqv? (car datum) 'case)
		 (case-exp  (parse-expression (cadr datum))
			    (map map-case (cddr datum))			    
			    (map parse-expression (map cadr (cddr datum))))]
			    
		[(eqv? (car datum) 'exit)
			(exit-exp
				(cadr datum))]
		[(eqv? (car datum) 'or)
		 (or-exp (map parse-expression (cdr datum)))]
		[(eqv? (car datum) 'and)
			(and-exp
				(map parse-expression (cdr datum)))]
		[(eqv? (car datum) 'or)
			(or-exp
				(map parse-expression (cdr datum)))]
		[(eqv? (car datum) 'define)
			(define-exp
				(cadr datum)
				(parse-expression (caddr datum))
			)
		]
		[(eqv? (car datum) 'let)
			(if (symbol? (cadr datum))
				(named-let-exp
					(cadr datum)
					(map car (caddr datum))
					(map parse-expression (map cadr (caddr datum)))
					(map parse-expression (cdddr datum))
				)
				(let-exp 
					(map car (cadr datum))
				   	(map parse-expression (map cadr (cadr datum)))
				   	(map parse-expression (cddr datum)))
			)
		]
		[(eqv? (car datum) 'letrec)
			(letrec-exp (map car (cadr datum))
			   (map parse-expression (map cadr (cadr datum)))
			   (map parse-expression (cddr datum)))]
		[(eqv? (car datum) 'let*)
			(let-star-parser (cadr datum) (cddr datum))]
		[(eqv? (car datum) 'set!)
		  (set-exp (cadr datum) (parse-expression (caddr datum)))]
		[(eqv? (car datum) 'if)
			(if (null? (cdddr datum))
				(if2-exp (parse-expression (cadr datum))
			  			 (parse-expression (caddr datum)))
				(if-exp (parse-expression (cadr datum))
					  (parse-expression (caddr datum))
					  (parse-expression (cadddr datum))))]
		[(eqv? (car datum) 'quote)
		  (lit-exp (cadr datum))]
		[(eqv? (car datum) 'begin)
		 (if (null? (cdr datum))
		     (eopl:error 'parse-expression "20. Begin is broken" datum)
		     (begin-exp
		      (map parse-expression (cdr datum))))]
		 [(eqv? (car datum) 'while)
			(if (null? (cdr datum))
			 	(eopl:error 'parse-expression "1. While is not proper" datum)
			 	(while-exp
			 		(parse-expression (cadr datum))
			 		(if (not (null? (cddr datum)))
			 			(map parse-expression (cddr datum))
			 			(list (parse-expression '()))
			 		)
			 	)
			)
		]
		[else (app-exp
			(parse-expression (car datum))
			(map parse-expression (cdr datum)))])]
	  [else (eopl:error 'parse-expression
			    "Invalid concrete syntax ~s" datum)])))

(define set!-h
	(lambda (ids vals)
		(cond 	
			[(null? ids) '()]
			[else 
				(cons 
					(set-exp (car ids) (car vals)) 
					(set!-h (cdr ids) (cdr vals))
				)
			]
		)
	)
)

(define fake-vals
	(lambda (n)
		(if (= n 0)
			'()
			(cons (lit-exp #f) (fake-vals (- n 1))))))

(define member?
  (lambda (x ls)
    (cond [(null? ls) #f]
	  [(eqv? x (car ls)) #t]
	  [else (member? x (cdr ls))])))

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

(define improper-lambda-converter
  	(lambda (ids ls)
    	(if (null? (cdr ids))
	        (list (lit-exp (improper-lambda-helper ls)))
	        (cons (car ls) (improper-lambda-converter (cdr ids) (cdr ls)))
	    )
	)
)

(define improper-lambda-helper
	(lambda (ls)
		(if (null? ls)
			'()
			(cons (cadr (car ls)) (improper-lambda-helper (cdr ls)))
		)
	)
)

(define expand-syntax
  (lambda (exp)
    (cases expression exp
    	[app-exp (rator rands)
    		(cases expression rator
               	[lambda2-exp (ids body)
                 	(app-exp 
                 		(lambda-exp ids body)
                  		(improper-lambda-converter ids rands)
                    )
                ]
                [lambda3-exp (ids body)
                	(app-exp
                		(lambda-exp ids body)
                		(improper-lambda-converter ids rands)
                	)
               	]
               	[else 
               		(app-exp 
               			(expand-syntax rator)
                        (map expand-syntax rands)
                    )
               	]
            )
    	]
    	[while-exp (test body)
    		(while-exp (expand-syntax test) (map expand-syntax body))]
    	[call/cc-exp (id)
    		(call/cc-exp (expand-syntax id))]
	   	[let-exp (syms vals bodies)
		    (app-exp 
		      (lambda-exp syms (map expand-syntax bodies))
		      ; (lambda-exp syms (list (begin-exp (map expand-syntax bodies))))
		      (map expand-syntax vals))]
		[lambda-exp (ids bodies)
			(lambda-exp ids (map expand-syntax bodies))
		] 
		[begin-exp (body)
			(begin-exp (map expand-syntax body))]
		[define-exp (ids body)
			(define-exp ids (expand-syntax body))]
	   	[letrec-exp (syms vals bodies)
		    (expand-syntax 
		    	(app-exp  
					(lambda-exp syms 
						(map expand-syntax (append (set!-h syms vals) bodies))
					)
					(map expand-syntax (fake-vals (length syms))) 
				)
			)
		]
	   	[if-exp (conditional if-true if-false)
		   (if-exp (expand-syntax conditional)
			   (expand-syntax if-true)
			   (expand-syntax if-false))]
		[if2-exp (conditional if-true)
		   (if2-exp (expand-syntax conditional)
			   (expand-syntax if-true))]
					;   	[app-exp (exps)
					; 	(app-exp (map expand-syntax exps))
					; ]
					;   	[lambda-exp (ids bodies)
					; 	(lambda-exp ids (map expand-syntax bodies))
					; ]
	   	[and-exp (body)
		    (cond [(null? body) (lit-exp #t)]
			  [(if (null? (cdr body)) (expand-syntax (car body))
			       (expand-syntax (if-exp (car body) (and-exp (cdr body)) (lit-exp #f))))])]
	   	[or-exp (body)
		   (if (null? body)
		       (lit-exp #f)
		       (if (null? (cdr body))
			   (expand-syntax (car body))
			   (expand-syntax
			    (let-exp
			     (list 'why?Wollowski)
			     (list (car body))
			     (list (if-exp (var-exp 'why?Wollowski)
					   (var-exp 'why?Wollowski)
					   (or-exp (cdr body))))))))]
	   	[case-exp (pkey tests exprs)
		     (cond [(null? (cdr tests)) (car exprs)]
			   [else (expand-syntax (if-exp (or-exp (map (lambda (x)
								       (app-exp (var-exp 'eq?) (list x pkey)))
								     (car tests)))
							(car exprs)
							(case-exp pkey (cdr tests) (cdr exprs))))])]
		[set-exp (id exp)
			(set-exp id (expand-syntax exp))
		]
		[named-let-exp (name ids vals body)
			(expand-syntax
				(letrec-exp
					(list name)
					(list (lambda-exp ids body))
					(list (app-exp
					 	(parse-expression name) vals
					))
				)
			)
		]
		[cond-exp (tests bodies)
		  	(cond [(null? (cdr tests)) (car bodies)]
		  		  [else (expand-syntax (if-exp (car tests)
		  		  							   (car bodies)
		  		  							   (cond-exp (cdr tests) (cdr bodies))))])]
	   	[else exp])))
