(newline)

; 3.1
(define (make-accumulator initial)
  (define (add x) 
    (begin (set! initial (+ x initial))
           initial))
  add)

(define A (make-accumulator 5))

; (display (A 10))
; (newline)
; (display (A 10))



; 3.2
(define (make-monitored f) 
  (let ((counter 0))
    (lambda (command)
      (cond ((eq? command 'how-many-calls?) counter)
            ((eq? command 'reset-count) (set! counter 0))
            (else (begin (set! counter (+ 1 counter))
                         (f command)))))))

; (define s (make-monitored sqrt))
; (newline)
; (display (s 100))
; (newline)
; (display (s 'how-many-calls?))
; (newline)
; (s 'reset-count)
; (display (s 'how-many-calls?))



; 3.3
; (define (make-account balance password)
;   (define (withdraw amount)
;     (if (>= balance amount)
;         (begin (set! balance 
;                      (- balance amount))
;                balance)
;         "Insufficient funds"))
;   (define (deposit amount)
;     (set! balance (+ balance amount))
;     balance)
;   (define (dispatch pwd m)
;     (cond ((not (eq? pwd password)) (error "Incorrect password"))
;           ((eq? m 'withdraw) withdraw)
;           ((eq? m 'deposit) deposit)
;           (else (error "Unknown request: 
;                  MAKE-ACCOUNT" m))))
;   dispatch)


; (define acc 
;   (make-account 100 'secret-password))
; (newline)
; (display ((acc 'secret-password 'withdraw) 40))
; (newline)
; (display ((acc 'some-other-password 'deposit) 50))




; 3.4
(define (call-the-cops amt)
  (display 'call-the-cops))
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((counter 1))
    (lambda (pwd m)
      (cond ((> counter 7) call-the-cops)
            ((not (eq? pwd password)) 
              (begin (set! counter (+ 1 counter))
                     (error "Incorrect password")))
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request: 
                   MAKE-ACCOUNT" m))))))


(define acc 
  (make-account 10000 'secret-password))
(newline)
(display ((acc 'wrong-password 'withdraw) 40))
(display ((acc 'wrong-password 'withdraw) 40))
(display ((acc 'wrong-password 'withdraw) 40))
(display ((acc 'wrong-password 'withdraw) 40))
(display ((acc 'wrong-password 'withdraw) 40))
(display ((acc 'wrong-password 'withdraw) 40))
(display ((acc 'wrong-password 'withdraw) 40))
(display ((acc 'wrong-password 'withdraw) 40))









