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