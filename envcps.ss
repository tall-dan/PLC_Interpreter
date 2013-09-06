(load "chez-init.ss")

;;(define-datatype environment environment?
;;  [empty-env-record]
;;  [extended-env-record
;;   (syms (list-of symbol?))
;;   (vals (list-of scheme-value?))
;;   (env environment?)])

(define scheme-value? (lambda (v) #t))


(define empty-env
  (lambda ()
    '()))


(define extend-env
  (lambda (syms vals env)
    (cons (cons syms (list->vector vals)) env)))

(define extend-env-letrec
  (lambda (syms vals env cont)
    (let* ([len (length syms)]
	   [vec (list->vector vals)]
	   [new-env (cons (cons syms vec) env)])
      (for-each (lambda (item pos)
		  (if (proc? item)
		  (vector-set! vec
			       pos
				   (cases proc item
					  [closure (ids bodies toss-env)
						   (closure ids bodies new-env)]
					  [primitive (id)
						     item]
					  [acontinuation (cont) '()		 ]))))
		vals
		(indexes (- len 1) '()))
      (apply-cont cont new-env))))

(define indexes
  (lambda (n accu)
    (if (= n 0)
	(cons 0 accu)
	(indexes (- n 1) (cons n accu)))))

(define extend-env-recur
	(lambda (vars vals env cont)
		(cond [(null? vars) env]
			  [else (extend-env-recur (cdr vars) (cdr vals) (extend-env (list (car vars)) (list (eval-tree (car vals) cont env)) env) cont)])))	  



(define apply-env
  (lambda (env sym)
    (cond [(null? env) (apply-global-env sym)]
	   [else (let ((pos (find-position sym (caar env) 0)))
				  (if (number? pos)
				      (let ([return (vector-ref (cdar env) pos)])
						(if (not (ref-param? return))
							return
						(cases ref-param return
							[ref-p (var env2) (apply-env env2 var)])))
				      (apply-env (cdr env) sym)))])))

(define extend-global-env
  (lambda (sym val)
    (set! global-env (cons (cons sym val) global-env))))


(define apply-global-env
  (lambda (sym)
    (let ((found (find-global sym global-env)))
      (if (null? found) (eopl:error 'apply-global-env "~s is not bound" sym)
	  found))))

(define find-global
  (lambda (sym env)
    (cond [(null? env) '()]
	  [(eqv? (caar env) sym) (cdar env)]
	  [else (find-global sym (cdr env))])))

(define change-env
  (lambda (env sym val)
    (if (null? env)
	(extend-global-env sym val)
	(let ([syms (caar env)]
	      [vals (cdar env)]
	      [next-env (cdr env)])
	  (let ((pos (find-position sym syms 0)))
	    (if (number? pos)
			(let ([return (vector-ref vals pos)])
					(if(not(ref-param? return))
						(vector-set! vals pos val)
						(cases ref-param return
							[ref-p (var env2)
								(change-env env2 var val)])))
		(change-env next-env sym val)))))))

(define change-env-strong
  (lambda (env sym val)
    (if (null? env)
	'() ;;;error
	(let ([syms (caar env)]
	      [vals (cdar env)]
	      [next-env (cdr env)])
	  (let ((pos (find-position sym syms 0)))
	    (if (number? pos)
		(vector-set! vals pos val)
		(set! env (extend-env-letrec (list sym) (list val) env))))))))

(define find-position
  (lambda (sym ls pos)
    (cond [(null? ls) #f]
	  [(eq? sym (car ls)) pos]
	  [else (find-position sym (cdr ls) (+ pos 1))])))

(define make-init-env
  (lambda ()
    (map prim prim-proc-names)))

(define reset-global-env
  (lambda ()
    (set! global-env (make-init-env))))

(define extend-env-new
  (lambda (vars args env newvars newargs)
    (cond [(symbol? vars) (extend-env (append newvars (list vars)) (append newargs (list args)) env)]
	  [(null? vars) (extend-env newvars newargs env)]
	  [else (extend-env-new (cdr vars) (cdr args) env (append newvars (list (car vars))) (append newargs (list (car args))))])))
