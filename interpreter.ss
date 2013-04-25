(define flag #f)


(define eval-one-exp
  (lambda (exp)
    (let* ([parse-tree (parse-expression exp)]
	   	[response (eval-expression parse-tree (initial-env))])
      response)))


(define rep
  (lambda ()
    (display ">> ")
    (let ([input (read)])
      (if (equal? input '(exit))
	  (printf "Bye...~%")
	  (let* ([parse-tree (parse-expression input)]
		 [response (eval-expression parse-tree (initial-env))])
	    (pretty-print response)
	    (rep))))))


(define eval-expression
  (lambda (exp env)
    (cases expression exp
	   	[var-exp (id) (apply-env env id)]
           [set-exp (sym val)
		    (let ([the-val (eval-expression val env)])
		      (change-env env
				  sym
				  the-val))]
	   	[lit-exp (val) val]
	   	[let-exp (ids vals body)
		    (let* ([evaluated-vals (eval-expressions vals env)]
			   [extended-env (extend-env ids evaluated-vals env)])
		      (eval-begin body extended-env))]
		[exit-exp (val)
			val
		]
		[begin-exp (body)
				(eval-begin body env)
		]
	   	[if-exp (test-exp true-exp false-exp)
		   (if (eval-expression test-exp env)
		       (eval-expression true-exp env)
		       (eval-expression false-exp env))]
    	[lambda-exp (ids body)
		       (make-closure ids body env)
		]
		[while-exp (test body)
			(whileloop test body env)
		]

	   	[app-exp (exps)
		    (let ([vals (eval-expressions exps env)])
		      (apply-proc (car vals) (cdr vals) env))])))

(define whileloop
	(lambda (test body env)
		(if (eval-expression test env)
			(begin (eval-begin body env) (whileloop test body env))
		)
	)
)
(define for-proc
	(lambda (test update return body env)
		(if (eval-expression test env)
			(begin
				(eval-expression body env)
				(eval-expression return env)
				(eval-expression update env)
				(for-proc test update return body env) 
			)
		)
	)
)

(define eval-begin
	(lambda (ls env)
		(if (null? (cdr ls))
			(begin (eval-expression (car ls) env))
			(begin (eval-expression (car ls) env) (eval-begin (cdr ls) env))
		)
		
	)
)

(define eval-expressions
  (lambda (exps env)
    (if (null? exps)
	'()
	(cons (eval-expression (car exps) env)
	      (eval-expressions (cdr exps) env)))))

(define apply-proc
  (lambda (procedure args env)
    (cases proc procedure
	    [closure (ids body env)
		    (eval-expression body (extend-env ids args env))]
	    [primitive (name)
		      (apply-primitive-procedure name args env)])))
	   

(define make-closure
  (lambda (ids body env)
    (closure ids body env)))

(define-datatype proc proc?
  [closure
   (ids (list-of symbol?))
   (body expression?)
   (env pair?)]
  [primitive
   (name symbol?)]
)


(define apply-primitive-procedure
  	(lambda (name args env)
    	(let 
    		([1st (if (null? args)
			   '()
			   (car args))]

			  [2nd (if (null? args)
				   '()
				   (if (null? (cdr args))
				       '()
				       (cadr args)))]
			  [3rd (if (null? args)
			  		'()
			  		(if (null? (cdr args))
				       '()
				       (if (null? (cddr args))
				       		'()
				       		(caddr args))))
			  ])
	      (case name
			[(+) (if (null? 2nd)
					(+ 1st)
					(if (null? 3rd)
						(+ 1st 2nd)
						(+ 1st 2nd 3rd)
					)
				)
			]
			[(*) (if (null? 2nd)
					(* 1st)
					(* 1st 2nd))]
			[(/) (/ 1st 2nd)]
			[(-) (if (null? (cdr args))
				 (- 1st)
				 (- 1st 2nd))]
			[(cdr) (cdr 1st)]
			[(car) (car 1st)]
			[(zero?) (zero? 1st)]
			[(=) (= 1st 2nd)]
			[(<) (< 1st 2nd)]
			[(>) (> 1st 2nd)]
			[(cons) (cons 1st 2nd)]
			[(list) args]
			[(assq) (assq 1st 2nd)]
			[(assv) (assv 1st 2nd)]
			[(map) (mapper-fxn 1st 2nd env)]
			[(apply) (applier-fxn 1st 2nd env)]
			[(set-car!) (set-car! 1st 2nd)]
			[(null?) (null? 1st)]
			[(eq?) (eq? 1st 2nd)]
			[(display) (display 1st)]
			[(newline) (newline)]
			[(exit) 'need-to-exit])
	    )
	)
)
(define mapper-fxn
	(lambda (proc ls env)
		(if (null? ls)
			'()
			(cons  (apply-proc proc (list (car ls)) env) (mapper-fxn proc (cdr ls) env))
		)
	)
)
(define applier-fxn
	(lambda (p ls env)
		(if (null? (cdr ls))
			(apply-proc p ls env)
			(apply-proc p (list (car ls) (applier-fxn p (cdr ls) env))  env)
		)
	)
)


(define primitive-procedure-names 
	'(+ - * /  zero? = < <= => > cons car cdr list assq assv map apply set-car! null? eq?
				    exit display newline)
)


(define initial-env
  (lambda ()
    (extend-env primitive-procedure-names
		(map primitive primitive-procedure-names)
		(empty-env))))
	  