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


; (define acc 
;   (make-account 10000 'secret-password))
; (newline)
; (display ((acc 'wrong-password 'withdraw) 40))
; (display ((acc 'wrong-password 'withdraw) 40))
; (display ((acc 'wrong-password 'withdraw) 40))
; (display ((acc 'wrong-password 'withdraw) 40))
; (display ((acc 'wrong-password 'withdraw) 40))
; (display ((acc 'wrong-password 'withdraw) 40))
; (display ((acc 'wrong-password 'withdraw) 40))
; (display ((acc 'wrong-password 'withdraw) 40))




; 3.5
; (define (monte-carlo trials experiment)
;   (define (iter trials-remaining trials-passed)
;     (cond ((= trials-remaining 0)
;            (/ trials-passed trials))
;           ((experiment)
;            (iter (- trials-remaining 1) 
;                  (+ trials-passed 1)))
;           (else
;            (iter (- trials-remaining 1) 
;                  trials-passed))))
;   (iter trials 0))

; (define (predicate x y)
;   (< (+ (expt (- x 5) 2) (expt (- y 7) 2))
;      (expt 3 2)))

; (define (estimate-integral p x1 x2 y1 y2 trials)
;   (define (experiment)
;     (p (random-in-range x1 x2)
;        (random-in-range y1 y2)))
;   (monte-carlo trials experiment))

; (define pi 
;   (/ (estimate-integral predicate 2.0 8.0 4.0 11.0 10000)
;      9))




; 3.6
(define random-init 0) 
(define (rand-update x) (+ x 1)) ; A not-very-evolved PNRG 

(define rand 
 (let ((current-value random-init)) 
   (define (dispatch message) 
     (cond ((eq? message 'reset) 
             (lambda (new-value) (set! current-value new-value)))
           ((eq? message 'generate) 
             (begin (set! current-value (rand-update current-value)) 
                    current-value)))) 
   dispatch)) 

; (display (rand 'generate))
; (newline) 
; ; 1 
; (display ((rand 'reset) 0))
; (newline) 
; ; 0 
; (display (rand 'generate))
; (newline) 
; ; 1 



; 3.7
; (define (make-joint account password new-password)
;   (if (number? ((account password 'deposit) 0)) 
;       (lambda (pwd command)
;         (if (eq? new-password pwd)
;             (account password command)
;             (error "Incorrect password")))
;       (error "Invalid account")))



; 3.8
; (define f
;   (let ((x 1000000))
;     (lambda (v)
;       (if (= x 1000000)
;           (set! x v)
;           0))))

(define f  
  (let ((state 0)) 
    (lambda (x) 
      (let ((xx state))
           (begin (set! state x) xx))))) 

; (newline)
; (display (+ (f 0) (f 1)))
; (newline)
; (display (+ (f 1) (f 0)))




; 3.9
; recursive version
; ---------------|
; | factorial    | Global
; |---------------
;       |
;       v
; ------------|
; |   n: 6    |
; |------------
;       |
;       v
; ------------|
; |   n: 5    |
; |------------
;       |
;       v
; ------------|
; |   n: 4    |
; |------------
;       |
;       v
; ------------|
; |   n: 3    |
; |------------
;       |
;       v
; ------------|
; |   n: 2    |
; |------------
;       |
;       v
; ------------|
; |   n: 1    |
; |------------
      

; iterative version
; ---------------|
; | factorial    | Global
; | fact-iter    |
; |---------------
;       |
;       v
; ------------|
; | n: 6      |
; |------------
;       |
;       v
; ---------------|
; | product:   1 |
; | counter:   1 |
; | max-count: 6 |
; |---------------
;       |
;       v
; ---------------|
; | product:   1 |
; | counter:   2 |
; | max-count: 6 |
; |---------------
;       |
;       v
; ---------------|
; | product:   2 |
; | counter:   3 |
; | max-count: 6 |
; |---------------
;       |
;       v
; ---------------|
; | product:   6 |
; | counter:   4 |
; | max-count: 6 |
; |---------------
;       |
;       v
; ----------------|
; | product:   24 |
; | counter:   5  |
; | max-count: 6  |
; |----------------
;       |
;       v
; -----------------|
; | product:   120 |
; | counter:   6   |
; | max-count: 6   |
; |-----------------
;       |
;       v
; -----------------|
; | product:   720 |
; | counter:   7   |
; | max-count: 6   |
; |-----------------















