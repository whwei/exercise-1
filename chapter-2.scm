(newline)
(newline)
(newline)
(define nil '())

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





; 2.17
(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

; (display (last-pair (list 23 72 149 34)))



; 2.18
; (define (reverse l)
;   (if (null? (cdr l))
;       l
;       (append (reverse (cdr l)) (cons (car l) '()))))

(define (reverse l)
  (define (iter p result)
    (if (null? p)
        result
        (iter (cdr p) (cons (car p) result))))
  (iter l '()))

; (display (reverse (list 1 4 9 16 25)))



; 2.19
(define us-coins 
  (list 25 50 10 5 1))

(define (cc amount coin-values)
  (cond ((= amount 0) 
         1)
        ((or (< amount 0) 
             (no-more? coin-values)) 
         0)
        (else
         (+ (cc 
             amount
             (except-first-denomination 
              coin-values))
            (cc 
             (- amount
                (first-denomination 
                 coin-values))
             coin-values)))))

(define (no-more? l)
  (= 0 (length l)))

(define (first-denomination l)
  (car l))

(define (except-first-denomination l)
  (cdr l))

; (define no-more? null?)
; (define first-denomination car)
; (define except-first-denomination cdr)

; (display (cc 100 us-coins))



; 2.20
(define (same-parity first . args)
  (define (iter l result rm)
    (if (null? l)
        result
        (if (= rm (remainder (car l) 2))
            (iter (cdr l) (append result (cons (car l) '())) rm)
            (iter (cdr l) result rm))))
  (iter args (list first) (remainder first 2)))

; (display (same-parity 1 2 3 4 5 6 7))
; (newline)
; (display (same-parity 2 3 4 5 6 7))



; 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) 
            (square-list (cdr items)))))

; (define (square-list items)
;   (map square items))

; (display (square-list (list 1 2 3 4)))



; 2.22
; (define (square-list items)
;   (define (iter things answer)
;     (if (null? things)
;         answer
;         (iter (cdr things)
;               (cons (square (car things))
;                     answer))))
;   (iter items nil))
; answer = last transformed list 
; next-answer = a new pair that (car pair) = [transform(current element)] and (cdr pair) = [last transformed list]
; which cause the final list is in reverse order

; (define (square-list items)
;   (define (iter things answer)
;     (if (null? things)
;         answer
;         (iter (cdr things)
;               (cons answer
;                     (square 
;                      (car things))))))
; answer = last transformed list 
; next-answer = a new pair that (car pair) = [last transformed list] and (cdr pair) = [transform(current element)] 
; which is in conflict with the definition of pairs in a list

; (display (square-list (list 1 2 3 4)))



; 2.23
(define (for-each action items)
  (cond 
    ((null? items) #t)
    (else (action (car items))
          (for-each action (cdr items)))))

; (for-each 
;  (lambda (x) (newline) (display x))
;  (list 57 321 88))




; 2.25
; (define items (list 1 3 (list 5 7) 9))
; (display (car (cdr (car (cdr (cdr items))))))
; (define items (cons (cons 7 nil) nil))
; (display (car (car items)))
; (define items (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
; (display (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr items)))))))))))))



; 2.26
; (1 2 3 4 5 6)
; ((1 2 3) 4 5 6)
; ((1 2 3) (4 5 6))




; 2.27
(define (deep-reverse l)
  (define (iter p result)
    (if (null? p)
        result
        (if (pair? (car p))
            (iter (cdr p) (cons (deep-reverse (car p)) result))
            (iter (cdr p) (cons (car p) result)))))
  (iter l nil))
(define x 
  (list (list 1 2) (list 3 4)))
; (display (reverse x))
; (newline)
; (display (deep-reverse x))



; 2.28
(define (fringe x)
  (cond ((null? x) nil)
        ((not (pair? x)) (list x))
        (else (append (fringe (car x)) (fringe (cdr x))))))


(define x 
  (list (list 1 2) (list 3 4)))

; (display (fringe x))
; (newline)
; (display (fringe (list x x)))



; 2.29
; (define (make-mobile left right)
;   (list left right))

; (define (make-branch length structure)
;   (list length structure))

; (define (left-branch m)
;   (car m))

; (define (right-branch m)
;   (car (cdr m)))

; (define (branch-length b)
;   (car b))

; (define (branch-structure b)
;   (car (cdr b)))

(define (branch-weight b)
  (if (is-mobile? (branch-structure b))
      (total-weight (branch-structure b))
      (branch-structure b)))

(define (is-mobile? x)
  (pair? x))

(define (total-weight m)
  (+ (branch-weight (left-branch m))
     (branch-weight (right-branch m))))

(define (branch-balanced? b)
  (if (is-mobile? (branch-structure b))
      (balanced? (branch-structure b))
      #t))

(define (branch-torque b)
  (* (branch-length b)
     (branch-weight b)))

(define (balanced? b)
  (let ((l (left-branch b))
        (r (right-branch b)))
       (and (= (branch-torque l) (branch-torque r))
            (branch-balanced? l)
            (branch-balanced? r))))

; (display (balanced? (make-mobile (make-branch 2 3) 
;                          (make-branch 3 2))) )




(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (cdr m))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (cdr b))




; 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
          (if (pair? sub-tree)
              (square-tree sub-tree)
              (square sub-tree)))
       tree))

(define tree (list 1
                         (list 2 (list 3 4) 5)
                         (list 6 7)))
; (display tree)
; (newline)
; (display (square-tree tree))




; 2.31
(define (tree-map fn tree)
  (map (lambda (sub-tree)
          (if (pair? sub-tree)
              (tree-map fn sub-tree)
              (fn sub-tree)))
       tree))

(define (square-tree tree) 
  (tree-map square tree))

; (display tree)
; (newline)
; (display (square-tree tree))



; 2.32
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s)))
            (append-car (lambda (ss)
              (append (list (car s)) ss))))
        (append rest (map append-car rest)))))

; (display (subsets (list 1 2 3)))




; 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))


(define (map p sequence)
  (accumulate (lambda (x y) (append (cons (p x) nil) y)) 
              nil sequence))

; (display (map square (list 1 2 3)))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

; (display (append (list 1 2 3) (list 4 5 6)))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; (display (length (list 1 2 3)))



; 2.34
(define 
  (horner-eval x coefficient-sequence)
  (accumulate 
   (lambda (this-coeff higher-terms)
     (+ (* higher-terms x) this-coeff))
   0
   coefficient-sequence))

; (display (horner-eval 2 (list 1 3 0 5 0 1)))



; 2.35
(define (count-leaves t)
  (accumulate 
    +
    0 
    (map (lambda (node) 
            (if (pair? node)
                (count-leaves node)
                1))
         t)))



; (define x (cons (list 1 2) (list 3 4)))

; (display (count-leaves x))
; (newline)
; (list x x)

; (display (count-leaves (list x x)))




; 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; (define sq (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
; (display (accumulate-n + 0 sq))










