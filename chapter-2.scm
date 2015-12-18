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


; (define (map p sequence)
;   (accumulate (lambda (x y) (append (cons (p x) nil) y)) 
;               nil sequence))

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



; 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) 
         m)))

; (define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
; (define v (list 1 1 1 1))
; (display (matrix-*-vector m v))
; (newline)
; (display (transpose m))
; (newline)
; (display (matrix-*-matrix m (list (list 1 1 1) (list 1 1 1) (list 1 1 1) (list 1 1 1))))



; 2.38
; both commutativity and associativity



; 2.39
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(define (reverse sequence)
  (fold-right 
   (lambda (x y) (append (reverse y) (cons x nil))) nil sequence))

; (define (reverse sequence)
;   (fold-left 
;    (lambda (x y) (cons y (reverse x))) nil sequence))

; (display (reverse (list 1 2 3 4)))



; 2.40
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))
(define (unique-pairs n)
  (flatmap (lambda (x) 
              (map (lambda (y)
                      (cons x y))
                   (enumerate-interval 1 (- x 1))))
           (enumerate-interval 1 n)))


; (display (unique-pairs 5))



; 2.41
(define (triple-sum n s)
  (define (triples n)
    (flatmap (lambda (x) 
                (flatmap (lambda (y)
                            (map (lambda (z)
                                    (list x y z))
                                 (enumerate-interval 1 (- y 1))))
                         (enumerate-interval 1 (- x 1))))
             (enumerate-interval 1 n)))
  (filter (lambda (list) (= s (accumulate + 0 list))) (triples n)))

; (display (triple-sum 3 6))



; 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
          (lambda (positions) 
            (safe? k positions))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                     (adjoin-position 
                      new-row 
                      k 
                      rest-of-queens))
                   (enumerate-interval 
                   1 
                   board-size)))
            (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board nil)

(define (adjoin-position new-row k rest-of-queens) (cons new-row rest-of-queens))

(define (safe? k positions) 
 (check-safe 
  (car positions) 
  1 
  (cdr positions))) 


(define (check-safe k distance rest) 
 (cond ((null? rest) #t) 
       ((= (car rest) k) #f) 
       ((= (- (car rest) distance) k) #f) 
       ((= (+ (car rest) distance) k) #f) 
       (else (check-safe k (+ distance 1) (cdr rest))))) 

; (display (queens 4))



; 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter 
                                  (- n 1))))
        (below painter 
                (beside smaller smaller)))))



; 2.45
(define (split f g)
  (lambda (painter n)
    (if (= 1 n)
        painter
        (let ((next ((split f g) painter (- n 1))))
          (f painter (g next next))))))



; 2.46
(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))
(define (add-vect v1 v2) 
  (cons (+ (xcor-vect v1) (xcor-vect v2)) 
        (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2) 
  (cons (- (xcor-vect v1) (xcor-vect v2)) 
        (- (ycor-vect v1) (ycor-vect v2))))
(define (add-vect s v) 
  (cons (* s (xcor-vect v)) 
        (* s (ycor-vect v))))



; 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (car (cdr frame)))
(define (edge2-frame frame) (car (cdr (cdr frame))))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (car (cdr frame)))
(define (edge2-frame frame) (cdr (cdr frame)))


; (define f (make-frame 1 2 3)) 
; (display (origin-frame f))
; (newline)
; (display (edge1-frame f))
; (newline)
; (display (edge2-frame f))
; (newline)



; 2.48
(define (make-segment s e) (cons s e))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))



; 2.53
; (a b c)
; ((george))
; ((y1 y2))
; (y1 y2)
; #f => 1
; #f
; (red shoes blue socks)



; 2.54
; (define (equal? a b)
;   (cond ((and (null? a) (null? b)) #t)
;         ((eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
;         (else #f)))

; (display (equal? '(this is a list) 
;         '(this is a list)))
; (newline)
; (display (equal? '(this is a list) 
;         '(this (is a) list)))



; 2.55
; ''abracadabra => (quote (quote abracadabra))



; 2.56
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) 
             (=number? m2 0)) 
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) 
         (* m1 m2))
        (else (list '* m1 m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product 
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product 
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ((exponentiation? exp)
          (make-product
            (make-product 
              (exponent exp) 
              (make-exponentiation (base exp)
                (make-sum (exponent exp) -1)))
            (deriv (base exp) var)))
        (else (error "unknown expression 
                      type: DERIV" exp))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((=number? base 1) 1)
        (else (list '** base exp))))



; 2.59
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (union-set s1 s2)
  (cond ((and (not (null? s2)) (null? s1)) s2)
        ((and (not (null? s1)) (null? s2)) s1)
        ((element-of-set? (car s1) s2) (union-set (cdr s1) s2))
        (else (cons (car s1) (union-set (cdr s1) s2)))))

; (display (union-set '(1 3) '(2 4)))
; (display (element-of-set? 1 '(1 2 3)))
; (display (adjoin-set '(1) '(2 3)))




; 2.60
; Θ(1)
(define (adjoin-set x set) (cons x set))

; Θ(n)
(define (union-set s1 s2) (append s1 s2))

; Θ(n^2)
(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) nil)
        ((element-of-set? (car s1) s2) 
          (cons (car s1) (intersection-set (cdr s1) s2)))
        (else (intersection-set (cdr s1) s2))))


; (display (intersection-set '(1 1 2 3 4 4) '(4 2 2 2)))



; 2.61
(define (adjoin-set x set)
  (cond ((null? set) (cons x nil))
        ((not (< (car set) x)) (cons x set))
        (else (append (cons (car set) nil) (adjoin-set x (cdr set))))))


; (display (adjoin-set 3 '(1 2 3 4)))
; (display (adjoin-set 0 '(1 2 3 4)))
; (display (adjoin-set 10 '(1 2 3 4)))



; 2.62
(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else (let ((x1 (car s1))
                   (x2 (car s2)))
                   (if (< x1 x2)
                       (cons x1 (union-set (cdr s1) s2))
                       (cons x2 (union-set s1 (cdr s2))))))))

; (display (union-set '(1 2 3 4) '(2 3 4 5)))






























