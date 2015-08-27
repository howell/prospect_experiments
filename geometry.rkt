#lang racket

(provide
 (struct-out posn)
 (struct-out circle)
 (struct-out line)
 intersection-circle-line
 line-through-points
 point-distance
 point-between?)


(struct posn (x y) #:transparent)

(struct circle (center radius) #:transparent)

;; ax + by = c
(struct line (a b c) #:transparent)

;; solve a quadratic formula of the form Ax^2 + Bx + C = 0
;; evaluates to #f if there is no solution
;; evalutes to n if n is the only solution
;; evaluates to (cons n m) with n >= m if there are two solutions
(define (solve-quadratic A B C)
  (define discriminant (- (expt B 2) (* 4 A C)))
  (cond
    [(< discriminant 0) #f]
    [(equal? discriminant 0) (/ (- B) (* 2 A))]
    [else (cons
           (/ (+ (- B) (sqrt discriminant)) (* 2 A))
           (/ (- (- B) (sqrt discriminant)) (* 2 A)))]))

(module+ test
  (require rackunit)
  ;; x^2 - 2x - 3
  (check-equal? (solve-quadratic 1 -2 -3)
                (cons 3 (- 1)))
  ;; x^2 + x - 4
  (check-equal? (solve-quadratic 1 1 -4)
                (cons (/ (+ -1 (sqrt 17)) 2)
                      (/ (- -1 (sqrt 17)) 2)))
  ;; x^2 - 3x - 4
  (check-equal? (solve-quadratic 1 -3 -4)
                (cons 4 -1))
  ;; x^2 - 4
  (check-equal? (solve-quadratic 1 0 -4)
                (cons 2 -2))
  ;; 6x^2 + 11x - 35
  (check-equal? (solve-quadratic 6 11 -35)
                (cons 5/3 -7/2))
  ;; x^2 - 3x + 29
  (check-equal? (solve-quadratic 1 -3 29)
                #f)
  ;; x^2 - 2x + 1
  (check-equal? (solve-quadratic 1 -2 1)
                1))

;; calculate the y value from an x value on a line
(define (line-y-at-x l x)
  (match-define (line a b c) l)
  (/ (- c (* a x)) b))

(module+ test
  ;; y = 6/5x + 14 => -6/5x + y = 14
  (define line0 (line -6/5 1 14))
  (check-equal? (line-y-at-x line0 0)
                14)
  (check-equal? (line-y-at-x line0 4)
                (+ 24/5 14))
  (check-equal? (line-y-at-x line0 -10)
                2)
  ;; y = 4 => 0x + y = 4
  (define line1 (line 0 1 4))
  (check-equal? (line-y-at-x line1 921312941)
                4)
  (check-equal? (line-y-at-x line1 0)
                4))

;; compute the y value(s) of a circle for a given x value
;; returns #f if there are none
;; returns y if there is exactly one
;; returns (cons y1 y2) if there are two, with y1 > y2
(define (circle-y-at-x c x)
  (match-define (circle (posn x0 y0) r) c)
  (define y-y0^2 (- (expt r 2) (expt (- x x0) 2)))
  (cond
    [(< y-y0^2 0) #f]
    [(equal? y-y0^2 0) (+ y0 (sqrt y-y0^2))]
    [else (cons (+ y0 (sqrt y-y0^2))
                (- y0 (sqrt y-y0^2)))]))

(module+ test
  ;; x^2 + y^2 = 1
  (define circle0 (circle (posn 0 0) 1))
  (check-equal? (circle-y-at-x circle0 0)
                (cons 1 -1))
  (check-equal? (circle-y-at-x circle0 1)
                0)
  (check-equal? (circle-y-at-x circle0 2)
                #f))

;; compute the x value(s) of a circle for a given y value
;; returns #f if there are none
;; returns x if there is exactly one
;; returns (cons x1 x2) if there are two, with x1 > x2
(define (circle-x-at-y c y)
  (match-define (circle (posn x0 y0) r) c)
  (circle-y-at-x (circle (posn y0 x0) r) y))

(module+ test
  ;; (x - 3)^2 + (y - 4)^2 = 3^2
  (define circle1 (circle (posn 3 4) 3))
  (check-equal? (circle-x-at-y circle1 4)
                (cons 6 0))
  (check-equal? (circle-x-at-y circle1 1)
                3)
  (check-equal? (circle-x-at-y circle1 7)
                3))


;; compute the intersecting points of a circle c and line l
;; if there are no such points return #f
;; if there is one such point (x, y), return (posn x y)
;; if there are two such points (x1, y1) (x2, y2), return (cons (posn x1 y1) (posn x2 y2))
(define (intersection-circle-line circ l)
  (match-define (circle (posn x0 y0) r) circ)
  (match-define (line a b c) l)
  (cond
    [(equal? a 0) ;; horizontal line: y = c/b
     (define c/b (/ c b))
     (match (circle-x-at-y circ c/b)
       [#f #f]
       [(cons x1 x2) (cons (posn x1 c/b) (posn x2 c/b))]
       [x (posn x c/b)])]
    [(equal? b 0) ;; vertical line: x = c/a
     (define c/a (/ c a))
     (match (circle-y-at-x circ c/a)
       [#f #f]
       [(cons y1 y2) (cons (posn c/a y1) (posn c/a y2))]
       [y (posn c/a y)])]
    [else
     (define slope (/ (- a) b))
     (define y-int (/ c b))
     (define A (+ 1 (expt slope 2)))
     (define B (* 2 (- (* slope y-int) (* slope y0) x0)))
     (define C (+ (expt y0 2) (- (expt r 2)) (expt x0 2) (* -2 y-int y0) (expt y-int 2)))
     (match (solve-quadratic A B C)
       [#f #f]
       [(cons x1 x2) (cons (posn x1 (line-y-at-x l x1))
                           (posn x2 (line-y-at-x l x2)))]
       [x (posn x (line-y-at-x x))])]))

(module+ test
  (define unit-circle (circle (posn 0 0) 1))
  (define x=0 (line 1 0 0))
  (define y=0 (line 0 1 0))
  (check-equal? (intersection-circle-line unit-circle x=0)
                (cons (posn 0 1) (posn 0 -1)))
  (check-equal? (intersection-circle-line unit-circle y=0)
                (cons (posn 1 0) (posn -1 0)))
  (define y=x (line -1 1 0))
  (check-equal? (intersection-circle-line unit-circle y=x)
                (cons (posn (/ (sqrt 2) 2) (/ (sqrt 2) 2))
                      (posn (- (/ (sqrt 2) 2)) (- (/ (sqrt 2) 2))))))

;; Reduce the coefficients of a line to the smallest integer equivalents
;; and ensure that the y coefficient (b) is non-negative
(define (normalize-line l)
  (match-define (line a b c) l)
  (define d (gcd a b c))
  (define s (if (< b 0) -1 1))
  (line (/ a d s) (/ b d s) (/ c d s)))

;; construct the line passing through points p1 and p2
(define (line-through-points p1 p2)
  (match-define (cons (posn x1 y1) (posn x2 y2)) (cons p1 p2))
  (define y2-y1 (- y2 y1))
  (define x2-x1 (- x2 x1))
  (normalize-line (line (- y2-y1) x2-x1 (- (* y1 x2-x1) (* x1 y2-y1)))))

(module+ test
  (check-equal? (line-through-points (posn 0 0) (posn 1 0))
                (line 0 1 0))
  (check-equal? (line-through-points (posn 0 0) (posn 1 1))
                y=x)
  (check-equal? (line-through-points (posn -100 -100) (posn 12345 12345))
                y=x)
  (define y=-x (line 1 1 0))
  (check-equal? (line-through-points (posn -5 5) (posn 4 -4))
                y=-x))

;; calculate the distance between two points
(define (point-distance p1 p2)
  (match-define (cons (posn x1 y1) (posn x2 y2)) (cons p1 p2))
  (sqrt (+ (expt (- x1 x2) 2)
           (expt (- y1 y2) 2))))

(module+ test
  (check-equal? (point-distance (posn 0 0) (posn 0 0))
                0)
  (check-equal? (point-distance (posn 1 1) (posn 0 1))
                1)
  (check-equal? (point-distance (posn -2 4) (posn -2 0))
                4)
  (check-= (point-distance (posn -1 -1) (posn 1 1))
           (/ 4 (sqrt 2))
           .0002))

;; calculate if p is between p1 p2
(define (point-between? p1 p2 p)
  (match-define (list (posn x0 y0) (posn x1 y1) (posn x y)) (list p1 p2 p))
  (and (>= x (min x0 x1))
       (<= x (max x0 x1))
       (>= y (min y0 y1))
       (<= y (max y0 y1))))

(module+ test
  (check-true (point-between? (posn 0 0) (posn 0 0) (posn 0 0)))
  (check-true (point-between? (posn -1 -1) (posn 1 1) (posn 0 0)))
  (check-true (point-between? (posn -1 -1) (posn 1 1) (posn 0 1)))
  (check-false (point-between? (posn -1 -1) (posn 1 1) (posn 2 0))))


