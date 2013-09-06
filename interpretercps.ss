


(define interp
  (lambda (form cont)
    (let ([parse-tree (parse-expression form)])
    (cases expression parse-tree
	   [define-exp (sym val)
	     (eval-tree (expand-syntax val) (extend-global-env-cont sym cont) (empty-env))]
	   [else (eval-tree (expand-syntax parse-tree) cont (empty-env))])))) 
	  

(define eval-tree
  (lambda (exp cont env)
    (cases expression exp
	   [var-exp (id) (apply-cont cont (apply-env env id))]
	   [lit-exp (id) (apply-cont cont id)]
	   [exit-exp () (printf "ta ta for now")]
	   [call/cc-exp (body) (eval-tree body (call-cont cont) env)]
	   [beg-exp (bodies)
		    (eval-expressions-cps bodies (printLst-cont cont) env)]
	   [if-exp (condition if-else) 
			(eval-tree condition (if-cont if-else cont env) env)]
	   [cond-exp (bodies)
		     (verify-in-order bodies env cont)]
	   [lambda-exp (vars bodies)
		       (apply-cont cont (closure (car vars) bodies env))]
	   [app-exp (exps)
			(eval-tree (car exps) (app1-cont exps env cont) env)]
	   [letrec-exp (vars vals bodies)
	           (eval-expressions-cps vals (letrec-cont vars bodies env cont) env)]
	   [set-exp (var val)
		    (eval-tree val (set-cont env var cont) env)]

	   [case-exp (id cases)
			(eval-case id cases env cont)]
	   [while-exp (test-exp bodies)
		      (whileHelp test-exp bodies env cont)]
	   [define-exp (sym val)
		  (if (null? env)
		      (eval-tree val (extend-global-env-cont sym cont) env)
		      (eval-tree (expand-syntax val) (set-cont env sym cont) env) )]
	   [break-exp (bodies)
		      (eval-expressions-cps bodies init-cont env)]
	   [else (eopl:error 'eval-tree "Unknown exp ~s" exp)]
		     )))
			  
(define reference-help-cps
	(lambda (vars body env2 operands env cont)
		(ref-help-help vars operands env '() (refHelp-cont vars body env2 cont))))
			  
(define ref-help-help
	(lambda (vars ops env accu cont)
		(cond [(null? vars) (apply-cont cont accu)]
			  [(list? (car vars)) (append-cps-dt  accu (list (ref-p (cadar ops) env)) (refHelpHelp-cont (cdr vars) (cdr ops) env cont))]
			  [else (eval-tree (car ops) (refHelpHelp2-cont accu env (cdr ops) (cdr vars) cont) env)])))
			  
(define ref-eliminator
	(lambda (vars accu cont)
		(cond [(null? vars) (apply-cont cont accu)]
			  [(list? (car vars)) (append-cps-dt accu (list (cadar vars)) (refEliminator-cont (cdr vars) cont))] 	
			  [else (append-cps-dt accu (list (car vars)) (refEliminator-cont (cdr vars) cont))])))
			 
(define append-cps-dt
  (lambda (L1 L2 K)
    (if (null? L1)
	(apply-cont K L2)
	(append-cps-dt (cdr L1) L2 (cons-cont (car L1) K)))))
			 
(define whileHelp
	(lambda (testExp bodies env cont)
		(eval-tree testExp (while-cont testExp bodies env cont) env)))

(define verify-in-order
  (lambda (x e cont)
    (cond [(null? x) (apply-cont cont (void))]
	  [(equal? (caar x) '(var-exp else)) (eval-tree (cadar x) cont e)]
	  [else (eval-tree (caar x) (cond-cont (cadar x) cont e (cdr x)) e)])))

(define eval-case
	(lambda (id cases env cont)
		(cond [(null? cases) (eopl:error 'eval-case "No match in cases")]
			  [(equal? (caar cases) '(var-exp else)) (eval-tree (cadar cases) cont env)]
			  [else (eval-help id (caar cases) env (case-cont (cadar cases) env id (cdr cases) cont))])))
			  
(define eval-help
	(lambda (id cases env cont)
		(cond [(null? cases) (apply-cont cont #f)]
		      [(eval-tree id (caseHelp-cont (car cases) env id (cdr cases) cont) env)])))
			  
(define init-cont (halt-cont))
	  

(define eval-tree-env
  (lambda (e)
    (lambda (x)
      (eval-tree x e))))

	
(define-datatype proc proc?
  [primitive
   (id symbol?)]
  [closure
   (vars scheme-value?)
   (body expression?)
   (env list?)]
   [acontinuation
	(cont continuation?)])
   
(define-datatype ref-param ref-param?
	[ref-p
	(var symbol?)
	(env list?)])



(define prim-proc-names '(+ - * / car cdr add1 sub1 zero? not = < > >= <= cons list null? eq? equal? atom? length list->vector list? pair? procedure? vector->list vector make-vector vector-ref vector? number? symbol? set-car! set-cdr! vector-set! caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr map apply assq assv append max display newline))

(define prim
    (lambda (x)
          (cons x (primitive x))))

(define global-env
  (map prim prim-proc-names))

(define apply-proc
  (lambda (pro args cont)
    (cases proc pro
	   [closure (vars body env)
		    (eval-tree body cont (extend-env-new vars args env '() '()))]
	   [primitive (id)
		      (apply-cont cont (apply-primitive-proc id args cont))]
		[acontinuation (cont)
			  (apply-cont cont (car args))])))

(define length-cps
  (lambda (ls k)
     (cond [(null? ls) (apply-cont k 0)]
	   [else (length-cps (cdr ls) (add-cont 1 k))])))
	   
(define eval-expressions-cps
  (lambda (exps cont env)
    (if (null? exps)
	(apply-cont cont '())
	(eval-tree (car exps) (eval-exps-cont (cdr exps) cont env) env))))
	
(define apply-primitive-proc ;;Check to make sure procedure? is legal and in cps
  (lambda (id args cont)
    (case id
      [(+) (apply + args)]
      [(-) (apply - args)]
      [(*) (apply * args)]
      [(/) (apply / args)]
      [(car) (caar args)]
      [(cdr) (cdar args)]
      [(add1) (+ (car args) 1)]
      [(sub1) (- (car args) 1)]
      [(zero?) (apply zero? args)]
      [(not) (apply not args)]
      [(=) (apply = args)]
      [(<) (apply < args)]
      [(>) (apply > args)]
      [(>=) (apply >= args)]
      [(<=) (apply <= args)]
      [(cons) (apply cons args)]
      [(list) args]
      [(null?) (apply null? args)]
      [(eq?) (apply eq? args)]
      [(equal?) (apply equal? args)]
      [(atom?) (apply atom? args)]
      [(length) (apply length args)]
      [(list->vector) (apply list->vector args)]
      [(list?) (apply list? args)]
      [(pair?) (apply pair? args)]
      [(procedure?) (or (apply procedure? args) (apply proc? args))]
      [(vector->list) (apply vector->list args)]
      [(vector) (list->vector args)]
      [(make-vector) (apply make-vector args)]
      [(vector-ref) (apply vector-ref args)]
      [(vector?) (apply vector args)]
      [(number?) (apply number? args)]
      [(symbol?) (apply symbol? args)]
      [(set-car!)	(set-car! (car args) (cadr args))]
      [(set-cdr!) (apply set-cdr! args)]
      [(vector-set!) (apply vector-set! args)]
      [(caar) (caaar args)]
      [(cadr) (cadar args)]
      [(cdar) (cdaar args)]
      [(cddr) (cddar args)]
      [(caaar) (caaaar args)]
      [(caadr) (caadar args)]
      [(cadar) (cadaar args)]
      [(caddr) (caddar args)]
      [(cdaar) (cdaaar args)]
      [(cdadr) (cdadar args)]
      [(cddar) (cddaar args)]
      [(cdddr) (cdddar args)]
      [(map) (map (lambda (x) (apply-proc (car args) (list x) cont)) (cadr args))]
      [(apply) (apply-proc (car args) (cadr args) cont)]
      [(assq) (apply assq args)]
      [(assv) (apply assv args)]
      [(append) (apply append args)]
      [(max) (apply max args)]
      [(display) (display (car args))]
      [(newline) (newline)]
      [else (eopl:error 'apply-primitive-proc' "Error applying")])))
	 
