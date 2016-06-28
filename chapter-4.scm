; 4.1
(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env)))
        (let ((right (list-of-values-lr (rest-operand exps) env)))
          (cons left right)
        )
      )
  )
)

(define (list-of-values-rl exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values-lr (rest-operand exps) env)))
        (let ((left (eval (first-operand exps) env)))
          (cons left right)
        )
      )
  )
)


; 4.4
(define (and? expr)
  (tagged-list? expr 'and))
(define (and-operands expr)
  (cdr expr))
(define (and->if expr)
  (check-and-operands (and-operands expr)))
(define (check-and-operands ops)
  (if ((null? ops) 'false)
      (make-if (car ops)
               (check-and-operands (cdr ops))
               'false)))


(define (or? expr)
  (tagged-list? expr 'or))
(define (or-operands expr)
  (cdr expr))
(define (or->if expr)
  (check-or-operands (or-operands expr)))
(define (check-or-operands ops)
  (if ((null? ops) 'false)
      (make-if (car ops)
               'true
               (check-or-operands (cdr ops)))))




; 4.5
(define (test-cond? clause)
  (eq? (cadr clause) '=>))

(define (cond-action clause) 
  (if (test-cond? clause)
      (cons (caddr clause) (cond-predicate clause))
      (cdr clause)))



; 4.6
(define (let? expr)
  (tagged-list? expr 'let))
(define (let-var expr)
  (map car (cadr expr)))
(define (let-exp expr)
  (map cadr (cadr expr)))
(define (let-body expr)
  (cddr expr))
(define (let->combination expr)
  (cons (make-lambda (let-var expr) (let-body expr))
        (let-exp expr)))




; 4.7
(define (let*? expr)
  (tagged-list? expr 'let*))
(define (let*-exp expr)
  (cadr expr))
(define (let*-body)
  (caddr expr))

; (let* ((x 3)
;        (y (+ x 2))
;        (z (+ x y 5)))
;   (* x z))
; (let ((x 3)) (let ((y (+ x 2)) (let ((z (+ x y 5)) (* x z))))))
(define (list*->nested-lets expr)
  (let ((exps (let*-exp expr))
        (body (let*-body expr)))
       (define (make-let exprs)
         (if (null? exprs)
             body
             (list 'let (list (car exprs)) (make-let (cdr exprs)))
         )   
       )
       (make-let exps)
  )
)



; 4.8
(define (named-let? expr)
  (and (let? expr) (symbol? (cadr expr))))
(define (named-let-func-name expr)
  (cadr expr))
(define (named-let-func-params expr)
  (map car (caddr expr)))
(define (named-let-func-inits expr)
  (map cadr (caddr expr)))
(define (named-let-body expr)
  (cdr (cddr expr)))
; define a function whose name is (named-let-func-name expr) and takes (named-let-func-params expr)
; as arguments
(define (named-let-func expr)
  (list 'define 
        (cons (named-let-func-name expr) (named-let-func-params expr))
        (named-let-func-body expr)))
(define (named-let->combination expr)
  (if (named-let? expr)
      (sequence-exp (list (named-let-func expr) (cons (named-let-func-name expr) (named-let-func-inits expr)))
      (cons (make-lambda (let-var expr) (let-body expr))
        (let-exp expr))))




