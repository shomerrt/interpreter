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
	   [for-exp (test update return body)
	   		(for-proc test update return body env)
	   ]
	   [let-exp (ids vals body)
		    (let* ([evaluated-vals (eval-expressions vals env)]
			   [extended-env (extend-env ids evaluated-vals env)])
		      (eval-expression body extended-env))]
		[exit-exp (val)
			val
		]
	   [if-exp (test-exp true-exp false-exp)
		   (if (eval-expression test-exp env)
		       (eval-expression true-exp env)
		       (eval-expression false-exp env))]
    	   [lambda-exp (ids body)
		       (make-closure ids body env)]
	   [app-exp (exps)
		    (let ([vals (eval-expressions exps env)])
		      (apply-proc (car vals) (cdr vals) env))])))

(define for-proc
	(lambda (test update return body env)
		(if (eval-expression test env)
			(begin
				(eval-expression body env)
				(eval-expression return env)
				(eval-expression update env)
				(for-proc test update return body env) 
			)
			(eval-expression return env)
		)
	)
)
(define for-proc2
	(lambda (test update return body)
		(if (test)
			(for-proc2 test update return body)
			return
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
		      (apply-primitive-procedure name args)])))
	   

(define make-closure
  (lambda (ids body env)
    (closure ids body env)))

(define-datatype proc proc?
  [closure
   (ids (list-of symbol?))
   (body expression?)
   (env pair?)]
  [primitive
   (name symbol?)])


(define apply-primitive-procedure
  (lambda (name args)
    (let ([1st (if (null? args)
		   '()
		   (car args))]
	  [2nd (if (null? args)
		   '()
		   (if (null? (cdr args))
		       '()
		       (cadr args)))])
      (case name
	[(+) (+ 1st 2nd)]
	[(*) (* 1st 2nd)]
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
	[(map) (map 1st 2nd)]
	[(apply) (apply 1st 2nd)]
	[(null?) (null? 1st)]
	[(eq?) (eq? 1st 2nd)]
	[(display) (display 1st)]
	[(newline) (newline)]
	[(exit) 'need-to-exit]))))

(define primitive-procedure-names 
	'(+ - * /  zero? = < <= => > cons car cdr list assq assv map apply null? eq?
				    exit display newline)
)


(define initial-env
  (lambda ()
    (extend-env primitive-procedure-names
		(map primitive primitive-procedure-names)
		(empty-env))))
	  
