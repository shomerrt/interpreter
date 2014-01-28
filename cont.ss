(define-datatype continuation continuation?
  (halt-cont)
  (cons-cont
   (v scheme-value?)
   (cont continuation?))
  (proc-cont
   (cont continuation?))
  (eval-exps-cont
   (exps (list-of expression?))
   (env scheme-value?)
   (cont continuation?))
  (if-cont
   (true-exp expression?)
   (false-exp expression?)
   (cont continuation?)
   (env list?))
  (if2-cont
   (true-exp expression?)
   (cont continuation?)
   (env list?))
  (begin-cont
    (body (list-of expression?))
    (env scheme-value?)
    (cont continuation?))
  ; (set-cont
  ;   (env scheme-value?)
  ;   (sym symbol?)
  ;   (cont continuation?))
  (set-cont
    (env scheme-value?)
    (id symbol?)
    (cont continuation?))
  (mapper-cont
    (env scheme-value?)
    (cont continuation?)
    (proc scheme-value?)
    (ls list?)
  )
  (setapply-cont
    (env scheme-value?)
    (id symbol?)
    (cont continuation?))
  (whileloop-cont 
    (test expression?)
    (body (list-of expression?))
    (env scheme-value?)
    (cont continuation?))
  (define-g-cont
    (env scheme-value?)
    (id symbol?)
    (cont continuation?))
  (define-l-cont
    (env scheme-value?)
    (id symbol)
    (cont continuation?))
  (call/cc-cont
    (cont continuation?))
  (begin-cont2
    (body (list-of expression?))
    (env scheme-value?)
    (cont continuation?)
  )
)

; (define whileloop
;   (lambda (test body env cont)
;     (if (eval-expression test cont env)
;       (begin (eval-begin body env cont) (whileloop test body env cont))
;     )
;   )
; )

      ; [define-exp (id body)
      ;   (if (null? env)
      ;     (apply-cont (define-g-cont env id cont) body)
      ;     (apply-cont (define-l-cont env id cont) body))]

(define scheme-value? (lambda (x) #t))

        ;   [set-exp (sym val)
          ; (let ([the-val (eval-expression val cont env)])
          ;   (change-env env
            ; sym
            ; the-val))]

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      [halt-cont () val]
      ; [call/cc-cont (cont)
      ;   ]
      [define-g-cont (env id cont)
        (apply-cont cont (change-env env id val))]
      [define-l-cont (env id cont)
        (extend-env (list id) (list (eval-expression val cont env)) env)]
      [eval-exps-cont (exps env cont)
        (eval-exps exps (cons-cont val cont) env)]
      [cons-cont (v cont)
        (apply-cont cont (cons v val))]
      [proc-cont (cont)
        (apply-proc (car val) (cdr val) (empty-env) cont)]
      [if-cont (if-true-exp if-false-exp next-cont env)
        (if val
         (eval-expression if-true-exp next-cont env)
         (eval-expression if-false-exp next-cont env))]
      [if2-cont (if-true-exp next-cont env)
        (if val
          (eval-expression if-true-exp next-cont env))]
      [begin-cont (body env cont)
        (if (null? body)
              (apply-cont cont val)
              (eval-expression (car body)
                (begin-cont (cdr body) env cont) env))
      ]
      [mapper-cont (env cont proc ls)
        (if (null? ls)
          (apply-cont (cons-cont val cont) '())
          (mapper-fxn proc ls env (cons-cont val cont))
        )
      ]
      [begin-cont2 (body env cont)
        (eval-begin body env cont)
      ]
      ; [set-cont (env sym cont)
      ;   (change-env env sym (eval-expression val cont env))]
      ; [set-cont (env id cont)
      ;   (eval-expression val (setapply-cont env id cont) env)]
      [setapply-cont (env id cont)
        (apply-cont cont (change-env env id val))]
      ; [whileloop-cont (test body env cont)
      ;     (if val
      ;       (eval-expression while-exp test body))]
      ; [whileloop-cont (test body env cont)
      ;     (whileloop test body env cont)]
      [call/cc-cont (cont)
        (cases proc val
          [closure (ids body env)
            (eval-expression 
              (car body) cont (extend-env ids (list (acontinuation cont)) env))]
            [else (eopl:error "not valid call/cc")])]
      [else eopl:error "not valid cont"]
    )
  )
)