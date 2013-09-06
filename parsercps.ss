(load "chez-init.ss")


(define scheme-value? (lambda (v) #t))

(define-datatype expression expression?
  (var-exp
   (id symbol?))
  (lit-exp
   (id scheme-value?))
  (lambda-exp
   (vars list?)
   (bodies expression?))
  (beg-exp
   (bodies (list-of expression?)))
  (app-exp
   (exps (list-of expression?)))
  (if-exp
   (condition expression?)
   (if-else (list-of expression?)))
  (cond-exp
   (bodies (list-of (list-of expression?))))
  (case-exp
   (id expression?)
   (cases (list-of scheme-value?)))
  (let-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies expression?))
  (let*-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies expression?))
  (letrec-exp
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies expression?))
  (nlet-exp
   (name symbol?)
   (vars (list-of symbol?))
   (vals (list-of expression?))
   (bodies expression?))
  (set-exp
   (var symbol?)
   (val expression?))
  (and-exp
   (bodies (list-of expression?)))
  (or-exp
   (bodies (list-of expression?)))
  (while-exp
	(test-exp expression?)
	(bodies expression?))
  (define-exp
    (sym symbol?)
    (val expression?))
  (exit-exp)
  (break-exp
   (bodies (list-of expression?)))
  (call/cc-exp
	(body expression?))
)


(define parse-expression
  (lambda (datum)
    (cond [(symbol? datum) (var-exp datum)]
	  [(number? datum) (lit-exp datum)]
	  [(vector? datum) (lit-exp datum)]
	  [(null? datum) (lit-exp datum)]
	  [(eqv? datum '#t) (lit-exp datum)]
	  [(eqv? datum '#f) (lit-exp datum)]
	  [(string? datum) (lit-exp datum)]
	  [(pair? datum)
	   (cond [(not (list? datum)) (eopl:error 'parse-expression "Error while parsing list: Not a valid list ~s" datum)]
		 [(eq? (car datum) 'call/cc)
			(call/cc-exp (parse-expression (cadr datum)))]
		 [(eq? (car datum) 'exit)
		  (exit-exp)]
		 [(eq? (car datum) 'begin)
		  (beg-exp (map parse-expression (cdr datum)))]
		 [(eq? (car datum) 'break)
		  (break-exp (map parse-expression (cdr datum)))]
		 [(eq? (car datum) 'quote)
		  (if (not (= (length datum) 2)) (eopl:error 'parse-expression "Error while parsing quote: Invalid length: ~s" datum)
		      (lit-exp (cadr datum)))]
		 [(eq? (car datum) 'lambda)
		  (cond [(not-lambda-length? datum)(eopl:error 'parse-expression "Error while parsing lambda: Invalid length: ~s"  datum)]
			[(invalid-formals-lambda? datum) (eopl:error 'parse-expression "Error while parsing lambda: Formals must be symbols: ~s" (cadr datum))]
			[else (if (= (length datum) 3)
				  (lambda-exp (list (cadr datum))
					      (parse-expression (caddr datum)))
				  (lambda-exp (list (cadr datum))
					      (beg-exp (map parse-expression (cddr datum)))))])]
		 [(eq? (car datum) 'if)
		  (cond [(not-if-length? datum) (eopl:error 'parse-expression "Error while parsing if: Invalid length: ~s" datum)]
			[else (if-exp (parse-expression (cadr datum))
				      (map parse-expression (cddr datum)))])]
		 [(eq? (car datum) 'cond)
		  (cond [(< (length datum) 2) (eopl:error 'parse-expression "Error while parsing cond: Invalid length: ~s" datum)]
			[(not (valid-cond-sublists? (cdr datum))) (eopl:error 'parse-expression "Error while parsing cond: Invalid condition format: ~s" datum)]
			[else (cond-exp (map (lambda (x) (list (parse-expression (car x)) (parse-expression (cadr x)))) (cdr datum)))])]
		 [(eq? (car datum) 'case)
		  
		  (cond [(< (length datum) 3) (eopl:error 'parse-expression "Error while parsing case: Invalid length: ~s" datum)]
			[(not (proper-cases? (cddr datum))) (eopl:error 'parse-expression "Error while parsing case: Invalid case syntax: ~s" datum)]
			[else (case-exp (parse-expression (cadr datum)) (map (lambda (x) (if (eqv? (car x) 'else) (list (parse-expression (car x)) (beg-exp (map parse-expression (cdr x))))
											(list (map parse-expression (car x)) (beg-exp (map parse-expression (cdr x)))))) (cddr datum)))])]
		 [(eq? (car datum) 'let*)
		  (cond [(< (length datum) 3) (eopl:error 'parse-expression "Error while parsing let*: Invalid length: ~s" datum)]
			[(not (list? (cadr datum))) (eopl:error 'parse-expression "Error while parsing let*: 1st argument is not a list: ~s" (cadr datum))]
			[(not (proper-sublists? (cadr datum))) (eopl:error 'parse-expression "Error while parsing let*: Invalid sublist in 1st argument: ~s" (cadr datum))]
			[else (if (= (length datum) 3)
				  (let*-exp (map car (cadr datum))
					   (map (lambda (x) (parse-expression (cadr x))) (cadr datum))
				           (parse-expression (caddr datum)))
				  (let*-exp (map car (cadr datum))
					   (map (lambda (x) (parse-expression (cadr x))) (cadr datum))
					   (beg-exp (map parse-expression (cddr datum)))))])]
		 [(eq? (car datum) 'letrec)
		  (cond [(< (length datum) 3) (eopl:error 'parse-expression "Error while parsing letrec: Invalid length: ~s" datum)]
			[(not (list? (cadr datum))) (eopl:error 'parse-expression "Error while parsing letrec: 1st argument is not a list: ~s" (cadr datum))]
			[(not (proper-sublists? (cadr datum))) (eopl:error 'parse-expression "Error while parsing letrec: Invalid sublist in 1st argument: ~s" (cadr datum))]
			[else (letrec-exp (map car (cadr datum))
					  (map (lambda (x) (parse-expression (cadr x))) (cadr datum))
					  (beg-exp (map parse-expression (cddr datum))))])]
		 [(and (eq? (car datum) 'let) (symbol? (cadr datum)))
		  (cond [(< (length datum) 4) (eopl:error 'parse-expression "Error while parsing named let: Invalid length: ~s" datum)]
			[(not (list? (caddr datum))) (eopl:error 'parse-expression "Error while parsing named let: 2nd argument is not a list: ~s" (caddr datum))]
			[(not (proper-sublists? (caddr datum))) (eopl:error 'parse-expression "Error while parsing named let: Invalid sublist in 2nd argument: ~s" (caddr datum))]
			[else (nlet-exp (cadr datum)
					(map car (caddr datum))
					(map (lambda (x) (parse-expression (cadr x))) (caddr datum))
					(beg-exp (map parse-expression (cdddr datum))))])]
		 [(eq? (car datum) 'let)
		  (cond [(< (length datum) 3) (eopl:error 'parse-expression "Error while parsing let: Invalid length: ~s" datum)]
			[(not (list? (cadr datum))) (eopl:error 'parse-expression "Error while parsing let: 1st argument is not a list: ~s" (cadr datum))]
			[(not (proper-sublists? (cadr datum))) (eopl:error 'parse-expression "Error while parsing let: Invalid sublist in 1st argument: ~s" (cadr datum))]
			[else (if (= (length datum) 3)
				  (let-exp (map car (cadr datum))
					   (map (lambda (x) (parse-expression (cadr x))) (cadr datum))
				           (parse-expression (caddr datum)))
				  (let-exp (map car (cadr datum))
					   (map (lambda (x) (parse-expression (cadr x))) (cadr datum))
					   (beg-exp (map parse-expression (cddr datum)))))])]
		 [(eq? (car datum) 'set!)
		  (cond [(not (= (length datum) 3)) (eopl:error 'parse-expression "Error while parsing set!: Invalid length: ~s" datum)]
			[(not (symbol? (cadr datum))) (eopl:error 'parse-expression "Error while parsing set!: 1st argument must be a symbol: ~s" (cadr datum))]
			[else (set-exp (cadr datum)
				       (parse-expression (caddr datum)))])]
		 [(eq? (car datum) 'and)
		  (and-exp (map parse-expression (cdr datum)))]
		 [(eq? (car datum) 'or)
		  (or-exp (map parse-expression (cdr datum)))]
		 [(eq? (car datum) 'while)
			(while-exp (parse-expression (cadr datum)) (beg-exp (map parse-expression (cddr datum))))]
		 [(eq? (car datum) 'define)
		  (cond [(not (= (length datum) 3)) (eopl:error 'parse-expresssion "Error while parsing define: Invalid length: ~s" datum)]
			[(not (symbol? (cadr datum))) (eopl:error 'parse-expression "Error while parsing define: First argument must be a symbol: ~s" (cadr datum))]
			[else (define-exp (cadr datum) (parse-expression (caddr datum)))])]
		 [else (app-exp 
				(map parse-expression datum))])]
	  [else (eopl:error 'parse-expression "Invalid concrete syntac ~s" datum)])))


  
(define unparse-expression
  (lambda (exp)
    (cases expression exp
	   [var-exp (id) id]
	   [lit-exp (id) id]
	   [break-exp (bodies) (cons 'break (unparse-expression bodies))]
	   [beg-exp (bodies)
		    (cons 'begin (map unparse-expression bodies))]
	   [lambda-exp (vars bodies)
		       (cons 'lambda
			     (cons (car vars)
			     (map unparse-expression bodies)))]
	   [app-exp (exps)
			  (map unparse-expression exps)]
	   [if-exp (condition if-true if-false)
		    (if (null? (unparse-expression if-false))
			(list 'if
			      (unparse-expression condition)
			      (unparse-expression if-true))
			(list 'if
			      (unparse-expression condition)
			      (unparse-expression if-true)
			      (unparse-expression if-false)))]
	   [cond-exp (bodies)
		     (cons 'cond (map (lambda (x) (list (unparse-expression (car x)) (unparse-expression (cadr x)))) bodies))]
	   [let-exp (vars vals bodies)
		    (cons 'let
			  (cons (linklet vars vals '())
				(map unparse-expression bodies)))]
	   [let*-exp (vars vals bodies)
		     (cons 'let*
			   (cons (linklet vars vals '())
				 (map unparse-expression bodies)))]
	   [letrec-exp (vars vals bodies)
		       (cons 'letrec
			     (cons (linklet vars vals '())
				   (map unparse-expression bodies)))]
	   [nlet-exp (name vars vals bodies)
		     (cons 'let
			   (cons name
				 (cons (linklet vars vals '())
				       (map unparse-expression body))))]
	   [set-exp (var val)
		    (list 'set! var (unparse-expression val))]
	   [and-exp (bodies)
		    (cons 'and (map unparse-expression bodies))]
	   [or-exp (bodies)
		   (cons 'or (map unparse-expression bodies))]
	   [exit-exp ()
		     (list 'exit)])))

(define linklet
  (lambda (vars vals r)
    (if (null? vars) r
	(linklet (cdr vars) (cdr vals) (append r (list (list (car vars) (unparse-expression (car vals)))))))))
	  

(define not-lambda-length?
  (lambda (datum)
    (if(< (length datum) 3) #t #f)))

(define invalid-formals-lambda?
  (lambda (datum)
    (cond [(symbol? (cadr datum)) #f]
	  [(list? (cadr datum)) (not (symbols-list? (cadr datum)))]
	  [(pair? (cadr datum)) (not (symbols-pair? (cadr datum)))]
	  [else #t])))

(define symbols-list?
  (lambda (x)
    (if (null? x) #t
	(and (or (symbol? (car x)) (and (list? (car x)) (= (length (car x)) 2) (eqv? (caar x) 'ref))) (symbols-list? (cdr x))))))

(define symbols-pair?
  (lambda (x)
    (cond [(symbol? x) #t]
	  [(pair? x) (and (symbol? (car x)) (symbols-pair? (cdr x)))]
	  [else #f])))

(define not-if-length?
  (lambda (datum)
    (if (or (= (length datum) 3) (= (length datum) 4)) #f #t)))

(define proper-sublists?
  (lambda (x)
    (cond [(null? x) #t]
	  [(list? (car x)) (and (and (= (length (car x)) 2) (symbol? (caar x))) (proper-sublists? (cdr x)))]
	  [else #f])))

(define valid-cond-sublists?
  (lambda (x)
    (cond [(null? x) #t]
	  [(and (equal? (caar x) 'else) (not (null? (cdr x)))) #f]
	  [(= (length (car x)) 2) (valid-cond-sublists? (cdr x))]
	  [else #f])))

(define proper-cases?
  (lambda (x)
    (cond [(null? x) #t]
	  [(and (equal? (caar x) 'else) (not (null? (cdr x)))) #f]
	  [(and (list? (car x)) (or (eqv? (caar x) 'else) (list? (caar x))) (> (length (car x)) 1) (not (null? (caar x)))) (proper-cases? (cdr x))]
	  [else #f])))


(define expand-syntax
  (lambda (expr)
    (cases expression expr
	   [let-exp (syms vals bodies)
		    (app-exp (cons (lambda-exp (list syms) (expand-syntax bodies))
			     (map expand-syntax vals)))]
	   [lambda-exp (syms bodies)
		       (lambda-exp syms (expand-syntax bodies))]
	   [beg-exp (bodies)
		      (beg-exp (map expand-syntax bodies))]
	   [break-exp (bodies)
		      (break-exp  (map expand-syntax bodies))]
	   [app-exp (exps)
		    (app-exp (map expand-syntax exps))]
	   [if-exp (condition if-else)
		   (if-exp (expand-syntax condition) (map expand-syntax if-else))]
	   [set-exp (var val)
		    (set-exp var (expand-syntax val))]
		[cond-exp (bodies)
			(cond-exp (map (lambda (x) (list (expand-syntax (car x)) (expand-syntax (cadr x)))) bodies))]
		[and-exp (bodies)
			 (if (null? bodies)
			     (lit-exp #t)
			     (expand-syntax (and-convert (cdr (reverse bodies)) (car (reverse bodies)))))]
		[or-exp (bodies)
			(if (null? bodies)
			    (lit-exp #f)
			    (expand-syntax (or-convert (cdr (reverse bodies)) (car (reverse bodies)))))]
		[let*-exp (vars vals bodies)
			  (expand-syntax (let-convert (reverse vars) (reverse vals) bodies))]
		[letrec-exp (vars vals bodies)
			    (letrec-exp vars (map expand-syntax vals) (expand-syntax bodies))]
		[case-exp (id cases)
			(case-exp (expand-syntax id)
				  (map (lambda (x) (if (equal? (car x) (parse-expression 'else)) (list (expand-syntax (car x)) (expand-syntax (cadr x)))(list (map expand-syntax (car x)) (expand-syntax (cadr x))))) cases))]
		[while-exp (test-exp bodies)
			(while-exp (expand-syntax test-exp) (expand-syntax bodies))]
		[define-exp (sym val)
		  (define-exp sym (expand-syntax val))]
		[call/cc-exp (body)
		  (call/cc-exp (expand-syntax body))]
		[nlet-exp (name vars vals bodies)
		  (letrec-exp (cons name vars) (map expand-syntax (cons (lambda-exp (list vars) (expand-syntax bodies)) vals)) (app-exp (var-exp name) (map expand-syntax vals)))]	
		[else expr])))
	   
(define let-convert
  (lambda (vars vals r)
    (if (null? vars)
	r
	(let-convert (cdr vars) (cdr vals) (let-exp (list (car vars)) (list (car vals)) r)))))

(define and-convert
  (lambda (bodies r)
    (if (null? bodies)
	r
	(and-convert (cdr bodies) (if-exp (car bodies) (list r (lit-exp #f)))))))

(define or-convert
  (lambda (bodies r)
    (if (null? bodies)
	r
	(or-convert (cdr bodies) (if-exp (car bodies) (list (car bodies) r))))))
