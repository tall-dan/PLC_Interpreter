(load "chez-init.ss")
(load "cont.ss")
(load "envcps.ss")
(load "parsercps.ss")
(load "interpretercps.ss")

(define (rl) (load "intcps.ss"))

(define rep
  (lambda ()
    (display "--> ")
    (set! init-cont (rep-cont))
    (interp (read) (closureCheck-cont (rep-cont)))))


(define eval-one-exp
  (lambda (x)
    (set! init-cont (halt-cont))
    (interp x (closureCheck-cont (halt-cont)))))

(define-syntax for
  (syntax-rules (:)
    [(_ (init : test : update) body)
     (begin init (letrec ([loop (lambda () (if test (begin body update (loop))))]) (loop)))]))

(define-syntax return-first
  (syntax-rules ()
    [(_ e1 e2 ...)
     (begin (set! first e1) (begin e2 ...) first)]))
