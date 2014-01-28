(define flag #f)

(define eval-one-exp
  (lambda (exp)
    (let* (
	    	[parse-tree (parse-expression exp)]
	    	[expand-tree (expand-syntax parse-tree)]
			[response (top-level-eval expand-tree)]
		)
      	response)
  )

)

(define top-level-eval
	(lambda (form)
	    (cases expression form
		   [define-exp (sym val)
		     (change-global-env sym (eval-expression val (halt-cont) (empty-env)))]
		   [else (eval-expression form (halt-cont) (empty-env))]
		)
	)
)

(define rep
  (lambda ()
    (display "KIWI>")
    (let ([input (read)])
      (if (equal? input '(exit))
	  (printf "Bye...~%")
	  (let* ([parse-tree (parse-expression input)]
		 [response (eval-expression parse-tree (initial-env))])
	    (pretty-print response)
	    (rep))))))


; (define eval-expression
; 	(lambda (exp cont env)
; 	    (cases expression exp
; 		    [var-exp (id) (apply-cont cont (apply-env env id))]
; 			; [set-exp (sym val)
; 			; 	(apply-cont (set-cont env sym cont) val)]
; 			[set-exp (sym val)
; 			    (let ([the-val (eval-expression val cont env)])
; 			      (change-env env
; 					  sym
; 					  the-val))]
; 			[define-exp (id body)
; 				(let ([value (eval-expression body env)])
; 					(if (null? env)
; 						(change-global-env id value)
; 						(extend-env (list id) (list value) env)))]
; 		    [lit-exp (val) (apply-cont cont val)]
; 		    [exit-exp (val) val]
; 			; [begin-exp (body)
; 			; 		(eval-expression (car body) (begin-cont (cdr body) env cont) env)]
; 			[begin-exp (body)
; 			      (eval-begin body env cont)]
; 		    [if-exp (test-exp true-exp false-exp)
; 	    		(eval-expression test-exp (if-cont true-exp false-exp cont env) env)]
; 	    	[if2-exp (test-exp true-exp)
; 	    		(eval-expression test-exp (if2-cont true-exp cont env) env)]
; 	    	[lambda-exp (ids body)
; 		       (apply-cont cont (make-closure ids body env))]
; 		    [while-exp (test body)
; 			      (whileloop test body env cont)]
; 			[app-exp (rator rands)
; 				(eval-exps (cons rator rands) (proc-cont cont) env)]
; 			[else exp]
; 		)
; 	)
; )

(define eval-expression
	(lambda (exp cont env)
	    (cases expression exp
		    ; [var-exp (id) (apply-env env id)]
		    [var-exp (id) (apply-cont cont (apply-env env id))]
	      ;   [set-exp (sym val)
			    ; (let ([the-val (eval-expression val cont env)])
			    ;   (change-env env
					  ; sym
					  ; the-val))]
			; [set-exp (sym val)
			; 		(apply-cont (set-cont env sym cont) val)]
			[set-exp (sym val)
					(eval-expression val (setapply-cont env sym cont) env)]
			; [define-exp (id body)
			; 	(let ([value (eval-expression body cont env)])
			; 		(if (null? env)
			; 			(change-global-env id value)
			; 			(extend-env (list id) (list value) env)
			; 		) 
			; 	)
			; ]
			[define-exp (id body)
				(if (null? env)
					(eval-expression body (define-g-cont env id cont) env)
					(apply-cont (define-l-cont env id cont) body))
			]
		    ; [lit-exp (val) val]
			[lit-exp (val) (apply-cont cont val)]
		    ; [let-exp (ids vals body)
			   ;  (let* ([evaluated-vals (eval-expressions vals env)]
				  ;  [extended-env (extend-env ids evaluated-vals env)])
			   ;    (eval-begin body extended-env))]
	    	; [letrec-exp (ids vals body) body]
		    ; [exit-exp (val) val]
		    [begin-exp (body)
			      (eval-begin body env cont)]
		    ; [if-exp (test-exp true-exp false-exp)
			   ; (if (eval-expression test-exp cont env)
			   ;     (eval-expression true-exp cont env)
			   ;     (eval-expression false-exp cont env))]
			[if-exp (test-exp true-exp false-exp)
	    		(eval-expression test-exp (if-cont true-exp false-exp cont env) env)]
	   	 	[if2-exp (test-exp true-exp)
	    		(eval-expression test-exp (if2-cont true-exp cont env) env)]
		    ; [lambda-exp (ids body)
			   ;     (make-closure ids body env)]
			[lambda-exp (ids body)
		       (apply-cont cont (make-closure ids body env))]
		    [while-exp (test body)
				(eval-expression
					test
					(if-cont
						; (begin-exp (append body (list (while-exp test body))))
						(begin (eval-begin body env cont) (while-exp test body))
						(parse-expression '(+ 3 3));;DUMMY SHIT. RANDOM EXP. 
						cont
						env
					)
					env 
				)
			]
		    ; [app-exp (rator rands)
			   ;  (let ([b (eval-expression rator cont env)]
			   ;  		[a (eval-expressions rands cont env)])
			   ;    (apply-proc b a env cont))]
			[app-exp (rator rand)
            	(eval-exps (cons rator rand) (proc-cont cont) env)]
           	[call/cc-exp (id)
      				(eval-expression id (call/cc-cont cont) env)]
			[else exp]
		)
	)
)

(define eval-exps
  (lambda (exps cont env)
    (if (null? exps)
	(apply-cont cont (empty-env))
	(eval-expression (car exps) (eval-exps-cont (cdr exps) env cont) env))))


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

; (define eval-begin
;   (lambda (ls env cont)
;     (if (null? (cdr ls))
; 		(begin (eval-expression (car ls) cont env))
; 		(begin (eval-expression (car ls) cont env) (eval-begin (cdr ls) env cont))
; 	)
;   )
; )
(define eval-begin
  (lambda (ls env cont)
    (if (null? (cdr ls))
		(eval-expression (car ls) cont env)
		(eval-expression (car ls) (begin-cont2 (cdr ls) env cont) env)
	)
  )
)


(define eval-closure-bodies
	(lambda (ls env)
    	(if (null? (cdr ls))
			(begin (eval-expression (car ls) env))
			(begin
				(if (eqv? (caar ls) 'define-exp)
					(set! env (eval-expression (car ls) env))
					(eval-expression (car ls) env)
				) 
				(eval-begin (cdr ls) env)
			)
		)
  	)
)

(define eval-expressions
  (lambda (exps cont env)
    (if (null? exps)
	'()
	(cons (eval-expression (car exps) cont env)
	      (eval-expressions (cdr exps) cont env)))))

(define apply-proc
  (lambda (procedure args env cont)
    (cases proc procedure
    	; [closure (parameters body env)
     ;                (eval-expression body (extend-env parameters args env) cont)]
	    [closure (ids body env)
		    (eval-begin body (extend-env ids args env) cont)]
	    [primitive (name)
		      (apply-primitive-procedure name args env cont)]
		[acontinuation (cont)
				(apply-cont cont (car args))])))
	   

(define make-closure
  (lambda (ids body env)
    (closure ids body env)))

(define-datatype proc proc?
  [closure
   (ids (list-of symbol?))
   (body (list-of expression?))
   (env list?)]
  [primitive
   (name symbol?)]
  [acontinuation
  	(cont continuation?)]
)

(define apply-primitive-procedure
  	(lambda (name args env cont)
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
			[(+) (apply-cont cont (apply + args))]
			[(*) (apply-cont cont (apply * args))]
			[(-) (apply-cont cont (apply - args))]
			[(/) (apply-cont cont (apply / args))]
			[(cdr) (apply-cont cont (apply cdr args))]
			[(car) (apply-cont cont (apply car args))]
			[(cons) (apply-cont cont (apply cons args))]
			[(vector) (apply-cont cont (apply vector args))]
			[(vector?) (apply-cont cont (apply vector? args))]		
	      	[(eq?) (apply-cont cont (apply eq? args))]
			[(list) (apply-cont cont (apply list args))]
			[(procedure?) (apply-cont cont (yoloproc? args))]
			[(cadr) (apply-cont cont (apply cadr args))]
			[(=) (apply-cont cont (apply = args))]
			[(cddar) (apply-cont cont (apply cddar args))]

			[(zero?) (apply-cont cont (apply zero? args))]
			[(<) (apply-cont cont (apply < args))]
			[(>) (apply-cont cont (apply > args))]
			[(not) (apply-cont cont (apply not args))]
			[(eqv?) (apply-cont cont (apply eqv? args))]
			[(assq) (apply-cont cont (apply assq args))]
			[(assv) (apply-cont cont (apply assv args))]
			[(apply) (apply-cont cont (apply apply args))]
			[(set-car!) (apply-cont cont (apply set-car! args))]
			[(null?) (apply-cont cont (apply null? args))]
			[(display) (apply-cont cont (apply display args))]
			[(pair?) (apply-cont cont (apply pair? args))]
			[(map) (mapper-fxn 1st 2nd env cont)]
			[(max) (apply-cont cont (apply max args))]
			[(break) (apply-cont (halt-cont) args)]
			[(quote) (apply-cont cont (quote args))]
			)
	    )
	)
)

; (define apply-primitive-procedure
;   	(lambda (name args env cont)
;     	(let 
;     		([1st (if (null? args)
; 			   '()
; 			   (car args))]

; 			  [2nd (if (null? args)
; 				   '()
; 				   (if (null? (cdr args))
; 				       '()
; 				       (cadr args)))]
; 			  [3rd (if (null? args)
; 			  		'()
; 			  		(if (null? (cdr args))
; 				       '()
; 				       (if (null? (cddr args))
; 				       		'()
; 				       		(caddr args))))
; 			  ])
; 	      (case name
; 			[(+) (if (null? 2nd)
; 					(+ 1st)
; 					(if (null? 3rd)
; 						(+ 1st 2nd)
; 						(+ 1st 2nd 3rd)
; 					)
; 				)
; 			]
; 			[(*) (if (null? 2nd)
; 					(* 1st)
; 					(* 1st 2nd))]
; 			[(/) (/ 1st 2nd)]
; 			[(-) (if (null? (cdr args))
; 				 (- 1st)
; 				 (- 1st 2nd))]
; 			[(cdr) (cdr 1st)]
; 			[(car) (car 1st)]
; 			[(zero?) (zero? 1st)]
; 			[(=) (= 1st 2nd)]
; 			[(cddar) (cddar 1st)]
; ;;			[(define) (definer-fxn 1st 2nd env)]
; 			[(<) (< 1st 2nd)]
; 			[(>) (> 1st 2nd)]
; 			[(not) (not 1st)]
; 			[(eqv?) (eqv? 1st 2nd)]
; 			[(member?) (member? 1st 2nd)]
; 			[(cons) (cons 1st 2nd)]
; 			[(list) args]
; 			[(assq) (assq 1st 2nd)]
; 			[(assv) (assv 1st 2nd)]
; 			[(map) (mapper-fxn 1st 2nd env cont)]
; 			[(apply) (applier-fxn 1st 2nd env cont)]
; 			[(set-car!) (set-car! 1st 2nd)]
; 			[(null?) (null? 1st)]
; 			[(eq?) (eq? 1st 2nd)]
; 			[(display) (display 1st)]
; 			[(pair?) (pair? 1st)]
; 			[(max) (if (< 1st 2nd)
; 					2nd
; 					1st)]
; 			[(newline) (newline)]
; 			[(exit) 'need-to-exit]
; 			)
; 	    )
; 	)
; )



(define yoloproc?
	(lambda (x)
		(if (not (list? (car x)))
			#f
			(member? (cadar x) primitive-procedure-names))))

(define mapper-fxn
	(lambda (proc ls env cont)
		(if (null? ls)
			'()
			(apply-proc proc (list (car ls)) env (mapper-cont env cont proc (cdr ls)))
		)
	)
)
(define applier-fxn
	(lambda (p ls env cont)
		(if (null? (cdr ls))
			(apply-proc p ls env cont)
			(apply-proc p (list (car ls) (applier-fxn p (cdr ls) env cont)) env cont)
		)
	)
)

(define primitive-procedure-names 
	'(+ - * /  zero? = cddar < <= => > not cons car cdr list assq assv map apply set-car! null? eq?
				    exit display newline max vector? vector procedure? cadr zero? < > not eqv? assq assv apply
				    set-car! null? display pair? map break quote)
)

(define initial-env
  (lambda ()
    (extend-env primitive-procedure-names
		(map primitive primitive-procedure-names)
		(empty-env))))