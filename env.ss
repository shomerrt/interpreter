(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (syms vals env)
    (cons (cons syms (list->vector vals)) env)))

(define apply-env
  (lambda (env sym)
    (if (null? env)
	(eopl:error 'apply-env "No binding for ~s" sym)
	(let ([syms (caar env)]
	      [vals (cdar env)]
	      [env (cdr env)])
	  (let ((pos (list-find-position sym syms)))
	    (if (number? pos)
		(vector-ref vals pos)
		(apply-env env sym)))))))

(define change-env
  (lambda (env sym val)
    (if (null? env)
	(eopl:error 'apply-env "No binding for ~s" sym)
	(let ([syms (caar env)]
	      [vals (cdar env)]
	      [env (cdr env)])
	  (let ((pos (list-find-position sym syms)))
	    (if (number? pos)
		(vector-set! vals pos val)
		(change-env env sym val)))))))


(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ list-index-r 1)
		 #f))))))
