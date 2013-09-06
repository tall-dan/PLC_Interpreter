(define-datatype continuation continuation?
  (halt-cont)
  (cons-cont
   (v scheme-value?)
   (cont continuation?))
  (proc-cont
   (cont continuation?))  
  (eval-exps-cont
   (ops (list-of expression?))
   (cont continuation?)
   (env scheme-value?))
  (if-cont
   (if-else (list-of expression?))
   (cont continuation?)
   (env scheme-value?))
  (if-else-cont
   (value scheme-value?)
   (cont continuation?)
   (env scheme-value?))
  (rep-cont)
  (add-cont
   (value scheme-value?)
   (cont continuation?))
  (printLst-cont
   (cont continuation?))
  (while-cont     
   (testExp expression?) 
   (bodies expression?)
   (env list?)
   (cont continuation?))
  (whileHelp-cont 
   (testExp expression?)
   (bodies expression?)
   (env list?) 
   (cont continuation?))
  (set-cont 
   (env list?)
   (var scheme-value?)
   (cont continuation?))
  (extend-global-env-cont 
   (sym scheme-value?)
   (cont continuation?))
  (letrec-cont 
   (vars list?)
   (bodies expression?)
   (env list?)
   (cont continuation?))
  (letrec-help-cont
   (bodies expression?)
   (cont continuation?))	 
  (closureCheck-cont
   (cont continuation?))
  (cond-cont
   (true-case expression?)
   (cont continuation?)
   (env list?)
   (false-case list?))
  (case-cont
   (true-case expression?)
   (env list?)
   (id expression?)
   (false-case list?)
   (cont continuation?))
  (caseHelp-cont
   (arg expression?)
   (env list?)
   (id expression?)
   (false-part list?)
   (cont continuation?))
  (caseHelp2-cont
   (arg scheme-value?)
   (env list?)
   (id expression?)
   (false-part list?)
   (cont continuation?))
  (app1-cont 
   (exps (list-of expression?))
   (env list?)
   (cont continuation?))
  (refHelp-cont
   (vars scheme-value?)
   (body expression?)
   (env2 list?)
   (cont continuation?))
  (refHelp2-cont
   (arg scheme-value?)
   (body expression?)
   (env list?)
   (cont continuation?))
  (append-cont
	(arg list?)
	(cont continuation?))
  (refHelpHelp-cont
   (vars scheme-value?)
   (ops list?)
   (env list?)
   (cont continuation?))
  (refHelpHelp2-cont
   (accu list?)
   (env list?)
   (ops list?)
   (vars scheme-value?)
   (cont continuation?))
  (refEliminator-cont
   (vars list?)
   (cont continuation?))
  (call-cont
   (cont continuation?))
  (last-cont
   (c continuation?)
   (cont continuation?))
	 )
   


(define apply-cont
  (lambda (cont val)
    (cases continuation cont
	   [halt-cont ()		      
		      val]
	   [cons-cont (v k)
		      (apply-cont k (cons v val))]
	   [proc-cont (k)
		      (apply-proc (car val) (cdr val) k)]
	   [eval-exps-cont (ls k env)
		      (eval-expressions-cps ls (cons-cont val k) env)]
	   [if-cont (if-else k env)
		    (if val
			(eval-tree (car if-else) k env)
			(length-cps (cdr if-else) (if-else-cont (cdr if-else) k env)))]
		[if-else-cont (value k env)
			(if (= val 1)
				(eval-tree (car value) k env)
				(rep))]
		[rep-cont ()
			  (begin (pretty-print val) (rep))]
		[add-cont (value k)
			(apply-cont k (+ val value))]
		[printLst-cont (k)
			(if (null? val)
				(apply-cont k val)
			(apply-cont k (car (reverse val))))]
		[while-cont (testExp bodies env k)
			(if val 
				(eval-tree bodies (whileHelp-cont testExp bodies env k) env)
				(apply-cont k '()))]
		[whileHelp-cont (testExp bodies env k)
			(whileHelp testExp bodies env k)]
		[set-cont (env var k)
			(begin
			(change-env env var val)
			(apply-cont k (void)))]
		[extend-global-env-cont (sym k)
			(begin
			(extend-global-env sym val)
			(apply-cont k (void)))]
		[letrec-cont (vars bodies env k)
			(extend-env-letrec vars val env (letrec-help-cont bodies k))]
		[letrec-help-cont (bodies k)
			(eval-tree bodies k val)]
		[closureCheck-cont (k)
				   (if (pair? val)
				       (if (eqv? (car val) 'closure)
					   (apply-cont k '<interpreter-procedure>)
					   (apply-cont k val))
				       (apply-cont k val))]
		[cond-cont (true-case k env false-case)
			   (if val
			       (eval-tree true-case k env)
			       (verify-in-order false-case env k))]
		[case-cont (true-case env id false-case k)
			   (if val
			       (eval-tree true-case k env)
			       (eval-case id false-case env k))]
		[caseHelp-cont (arg env id false-part k)
			       (eval-tree arg (caseHelp2-cont val env id false-part k) env)]
		[caseHelp2-cont (arg env id false-part k)
				(if (equal? val arg)
				    (apply-cont k #t)
				    (eval-help id false-part env k))]
		[app1-cont (exps env k)
				(if (proc? val)
					(cases proc val
						[primitive (id) (eval-expressions-cps exps (proc-cont k) env)]
						[closure (vars body env2) (if (list? vars)
											  (reference-help-cps vars body env2 (cdr exps) env k)
											  (eval-expressions-cps exps (proc-cont k) env))]
						[acontinuation (cont) (eval-expressions-cps exps (proc-cont k) env)])
					(eval-expressions-cps (cdr exps) (last-cont val k) env))]
		[refHelp-cont (vars body env2 k)
				(ref-eliminator vars '() (refHelp2-cont val body env2 k))]
		[refHelp2-cont (arg body env k)
				(apply-proc (closure val body env) arg k)]
		[append-cont (v k)
			     (append-cps-dt v val k)]
		[refHelpHelp-cont (vars ops env k)
				(ref-help-help vars ops env val k)]
		[refHelpHelp2-cont (accu env ops vars k)
				(append-cps-dt accu (list val) (refHelpHelp-cont vars ops env k))]
		[refEliminator-cont (vars k)
				(ref-eliminator vars val k)]
		[call-cont (k)
				(cases proc val
					[closure (vars body env2) (apply-proc val (list k) k)]
					[primitive (id) '()]
					[acontinuation (cont) '()]
					)]
		[last-cont (c k)
				(apply-proc (acontinuation c) val k)]
		)))
