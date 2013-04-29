(load "chez-init.ss")
(load "interpreter.ss")
(load "env.ss")
(load "parser.ss")

(define (rl) (load "main.ss"))


	
;;<program>					<form>*
;;
;;<form>					<definition> | <expression>
;;
;;<definition>					<variable definition> | (begin <definition>*)
;;
;;<variable definition>				(define <variable> <expression>)
;;
;;<expression>					<constant>
;;						|	<variable>
;;						|	(quote <datum>)
;;						|	(lambda <formals> <expression> <expression>*)
;;						|	(if <expression> <expression> <expression>)
;;						|	(let <constant><constant>* <expression><expression>*)
;;						|	(begin <expression>*)
;;						|	(exit <constant>)
;;						|	(while <expression><expression>*)
;;						|	(and <expression>*)
;;						|	(or <expression>*)
;;						|	(cond <expression>*<expression>*)
;;						|	(case <constant> <expression>* <expression>*)
;;						|	(clause <expression><expression>)
;;						|	(list <datum>)
;;						|	(app <expression> <expression>*)
;;
;;<variable>					<boolean> | <number> | <string> | <character>
;;
;;
;;<formals>					<variable>
;;						|	(<variable>*)
;;
;;<constant>					<scheme-value?>