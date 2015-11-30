(newline)
(newline)
(newline)

; 2.1
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (if (or (< (* n d) 0))
              (- (/ n g))
              (/ n g)) 
          (abs (/ d g)))))

; (define one-half (make-rat 6 (- 9)))
; (print-rat one-half)



; 2.2
(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment sp ep)
  (cons sp ep))

(define (start-segment sg)
  (car sg))

(define (end-segment sg)
  (cdr sg))

(define (midpoint-segment sg)
  (define (average a b) (/ (+ a b)))
  (make-point (average (x-point (start-segment sg)) (x-point (end-segment sg)))
              (average (y-point (start-segment sg)) (y-point (end-segment sg)))))


; (define (print-point p)
;   (newline)
;   (display "(")
;   (display (x-point p))
;   (display ",")
;   (display (y-point p))
;   (display ")"))

; (define p1 (make-point 0 0))
; (define p2 (make-point 3 2))
; (print-point p1)
; (print-point p2)

; (define s1 (make-segment p1 p2))
; (print-point (start-segment s1))
; (print-point (end-segment s1))
; (print-point (midpoint-segment s1))



; 2.3
; segment implementation
(define (make-rectangle s) s)
(define (l-rectangle r)
  (abs (- (x-point (start-segment r)) (x-point (end-segment r)))))
(define (h-rectangle r)
  (abs (- (y-point (start-segment r)) (y-point (end-segment r)))))

(define (perimeter-rectangle r)
  (* 2 (+ (l-rectangle r) (h-rectangle r))))

(define (area-rectangle r)
  (* (l-rectangle r) (h-rectangle r)))


; (define p1 (make-point 0 0))
; (define p2 (make-point 3 3))
; (define s1 (make-segment p1 p2))


; (define r1 (make-rectangle s1))
; (display (perimeter-rectangle r1))
; (newline)
; (display (area-rectangle r1))



; 2.4
; (define (cons x y) 
;   (lambda (m) (m x y)))

; (define (car z) 
;   (z (lambda (p q) p)))

; (define (cdr z)
;   (z (lambda (p q) q)))




; 2.5
(define (exp base e)
  (if (> e 0)
      (* base (exp base (- e 1)))
      1))

(define (divide p d)
  (let ((r (remainder p d))
        (np (quotient p d)))
    (if (= r 0)
        (divide np d)
        p
        )))

(define (divide-counter p d)
  (define (divide-counter-iter p d counter)
    (if (= 0 (quotient p d))
        counter
        (divide-counter-iter (quotient p d) d (+ counter 1))))
  (divide-counter-iter p d 0))

(define (cons-2a3b a b) 
  (* (exp 2 a) (exp 3 b)))

(define (car-2a3b p) 
  (divide-counter (divide p 3) 2))

(define (cdr-2a3b p)
  (divide-counter (divide p 2) 3))

; (define p1 (cons-2a3b 11 17)) 
; (display p1)
; (newline)
; (display (car-2a3b p1))
; (newline)
; (display (cdr-2a3b p1))



; 2.6
; one => (define one (lambda (f) (lambda (x) (f x))))

; two => (define two (lambda (f) (lambda (x) (f ((one f) x))))))
;     => (deinfe two (lambda (f) (lambda (x) (f ((lambda (y) (f y)) x)))))
;     => (deinfe two (lambda (f) (lambda (x) (f (f x)))))

; (define (my-add a b)
;     (lambda (f)
;       (lambda (x)
;         ((a f) ((b f) x)))))



; 2.7
(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
                    (lower-bound y))
                 (+ (upper-bound x) 
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) 
               (lower-bound y)))
        (p2 (* (lower-bound x) 
               (upper-bound y)))
        (p3 (* (upper-bound x) 
               (lower-bound y)))
        (p4 (* (upper-bound x) 
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval 
                 (/ 1.0 (upper-bound y)) 
                 (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (upper-bound x)
  (max (car x) (cdr x)))

(define (lower-bound x)
  (min (car x) (cdr x)))



; 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


; 2.9
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;    (width (add-interval x y))
; => (/ (- (upper-bound (add-interval x y)) (lower-bound (add-interval x y))) 2)
; => (/ (- (+ (upper-bound x) (upper-bound y)) (+ (lower-bound x) (lower-bound y))) 2)
; => (/ (+ (- (upper-bound x) (lower-bound x)) (- (upper-bound y) (lower-bound y))) 2)
; => (+ (/ (- (upper-bound x) (lower-bound x)) 2) (/ (- (upper-bound y) (lower-bound y)) 2))
; => (+ (width x) (width y))

; (define i1 (make-interval 4 5))
; (define i2 (make-interval 1 2))
; (define w1 (width (mul-interval i1 i2)))
; (define w2 (* (width i1) (width i2)))
; (newline)
; (display (= w1 w2))



; 2.10
(define (div-interval-2 x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
      (error "Error: divide by a interval with span 0")
      (mul-interval x 
                    (make-interval 
                      (/ 1.0 (upper-bound y)) 
                      (/ 1.0 (lower-bound y))))))

; (define i1 (make-interval 2 10))
; (define span-0 (make-interval -1 9))
; (display (div-interval-2 i1 span-0))



; 2.12
(define (make-center-percent c p)
  (let ((diff (* c p 0.01)))
    (make-interval (+ c diff)
                   (- c diff))))

