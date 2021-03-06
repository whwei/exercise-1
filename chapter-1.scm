(newline)
(newline)

; 1.29
(define (sum term a next b) 
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (inc x) (+ 1 x))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (yk k) (f (+ a (* k h))))
  (define (simpson-term k) 
    (* (cond ((or (= k 0) (= k n)) 1)
             ((odd? k) 4)
             (else 2))
       (yk k)))
  (* (/ h 3)
     (sum simpson-term 0 inc n)))



;1.30
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))



; 1.31
(define (dec n) (- n 1))

; product recursive version
; (define (product term a next b) 
;   (if (> a b)
;       1
;       (* (term a)
;          (product term (next a) next b))))

; product iterative version
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (identity x) x)

(define (factorial n)
  (product identity 1 inc n))
  
(define (calc-pi n)
  (define (half-odd n) (+ 1 (* 2 (quotient n 2))))
  (define (pi-term k)
    (if (odd? k)
        (/ (- (half-odd k) 1) (half-odd k))
        (/ (+ (half-odd k) 1) (half-odd k))))

  (* 4
     (product pi-term 2 inc (+ n 1))))



; 1.32
; iterative 
; (define (accumulate combiner null-value term a next b)
;   (define (iter a result)
;     (if (> a b)
;         result
;         (iter (next a) (combiner result (term a)))))
;   (iter a null-value))

; recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))



; 1.33
(define (filtered-accumulate combiner filter null-value term a next b)
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a)
                (filtered-accumulate combiner filter null-value term (next a) next b))
          (combiner null-value 
                (filtered-accumulate combiner filter null-value term (next a) next b)))))

; (define (f1 a b)
;   (filtered-accumulate + prime? 0 squre a inc b))

; (define (f2 n)
;   (define (rp a b) (= (gcd i n) 1))
;   (filtered-accumulate * rp 1 identity 1 inc n))



; 1.34
; (f f) => (f 2) => (2 2) Error!



; 1.35
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))



; 1.36
(define (fixed-point-with-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y) (/ (+ x y) 2))

; (fixed-point-with-print (lambda (x) (+ 1 (/ 1 x))) 2.0)
; (newline)
; (fixed-point-with-print (lambda (x) (average x (+ 1 (/ 1 x)))) 2.0)
; (newline)


; 1.37

; recursive
; (define (cont-frac n d k)
;   (define (k-term kk)
;     (if (> kk k)
;         0
;         (/ (n kk) (+ (d kk) (k-term (+ 1 kk))))))
;   (k-term 1))

; iterative
(define (cont-frac n d k)
  (define (k-term-iter kk result)
    (if (< kk 1)
        result
        (k-term-iter (- kk 1) (/ (n kk) (+ (d kk) result)))))
  (k-term-iter k 0))

; (display (/ 1.0
;             (cont-frac (lambda (i) 1.0)
;                        (lambda (i) 1.0)
;                        100)
;   ))



; 1.38
; (display (+ 2
;             (cont-frac (lambda (i) 1.0)
;                        (lambda (i)
;                           (if (= 2 (remainder i 3))
;                               (* 2 (+ 1 (quotient i 3)))
;                               1.0))
;                         10000)
;   ))



; 1.39
(define (tan-cf x k)
  (cont-frac (lambda (i)
                (if (= i 1)
                    x
                    (- (* x x))))
             (lambda (i) (- (* 2 i) 1))
             k))

; (display (tan-cf 1 10))



; 1.40
(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) 
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) 
               guess))

(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

; (display (newtons-method (cubic 1 2 1) 1))



; 1.41
(define (double fn)
  (lambda (x)
    (fn (fn x))))

; (display ((double inc) 1))

; (((double (double double)) inc) 5) => 21
; (((double (lambda (x) (double (double x)))) inc) 5)
; (display (((lambda (x) ((double double) (double (double x)))) inc) 5))



; 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

; (display ((compose square inc) 6))



; 1.43
(define (repeated f n)
  (define (iter nn result)
    (if (> nn n)
        result
        (iter (+ nn 1) (f result))))
  (lambda (x)
    (iter 1 x)))

; (display ((repeated square 2) 5))



; 1.44
(define (smooth f)
  (lambda (x)
    (/ ( + (f (- x dx)) 
           (f x) 
           (f (+ x dx)))
        3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))



; 1.45
; (define (sqrt x)
;   (fixed-point (lambda (y) (average y (/ x y)))
;                1.0))

(define (average-damp f)
  (lambda (x) 
    (average x (f x))))

(define (power x nn)
  (if (> nn 0)
      (* x (power x (- nn 1)))
      1))

(define (nth-root x n)
  (define (log2 x)
    (/ (log x) (log 2)))
  (fixed-point ((repeated average-damp (ceiling (log2 n))) (lambda (y) (/ x (power y (- n 1)))))
               1.0))

; ((lambda (n) 
;   (define (iter nn)
;     (newline)
;     (display nn)
;     (display " ")
;     (display (nth-root 2 nn))
;     (if (> nn 1)
;         (iter (- nn 1))
;         1))
;   (iter n)) 10)



; 1.46
(define (iterative-improve is-good-enough improve-guess)
  (lambda (x)
    (let ((improved-x (improve-guess x)))
      (if (is-good-enough x improved-x)
          improved-x
          ((iterative-improve is-good-enough improve-guess) improved-x)))))

; sqrt
(define (sqrt x)
  ((iterative-improve (lambda (a b) (< (abs (- a b)) tolerance))
                      (average-damp (lambda (y) (/ x y))))
                      1.0))

; fixed-point
(define (fixed-point-iter f first-guess)
  ((iterative-improve (lambda (a b) (< (abs (- a b)) tolerance))
                      f)
                      first-guess))

; (define (sqrt-iter x)
;   (fixed-point-iter (lambda (y) (average y (/ x y)))
;                     1.0))
; (newline)
; (display (sqrt-iter 2))








