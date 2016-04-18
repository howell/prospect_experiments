#lang racket

(require "./geometry.rkt")

(provide closest-colliding
         move-player-x
         move-player-y)

;; rect posn (listof rect) -> (U rect #f)
;; find the colliding rectange that is closest to the given position, if any exist
(define (closest-colliding r p rs)
  (define collisions (filter (lambda (x) (overlapping-rects? r x))
                             rs))
  (define sorted (sort collisions #:key rect-top-left
                       (lambda (p1 p2)
                         (< (point-distance p p1)
                            (point-distance p p2)))))
  (match sorted
    ['() #f]
    [(cons closest _) closest]))

(module+ test
  (require rackunit)
  
  (check-equal? (closest-colliding (rect (posn 0 0) 1 1)
                                   (posn 0 0)
                                   '())
                #f)
  
  (check-equal? (closest-colliding (rect (posn 0 0) 2 1)
                                   (posn 0 0)
                                   (list (rect (posn 1 0) 1 1)))
                (rect (posn 1 0) 1 1))
  
  (check-equal? (closest-colliding (rect (posn 1 1) 1 1)
                                   2
                                   (list (rect (posn 3 0) 2 30)))
                #f)
  
  (check-equal? (closest-colliding (rect (posn 0 0) 10 1)
                                   (posn 0 0)
                                   (list (rect (posn 4 0) 1 1)
                                         (rect (posn 3 0) 1 1)
                                         (rect (posn 5 0) 1 1)))
                (rect (posn 3 0) 1 1)))

;; rect num [listof rect] -> (pair rect bool)
;; attempt to move the player given by the first argument along the x-axis
;; when a collision occurs move as far as possible without colliding
;; returns the new rect for the player as well as if a collision occured
(define (move-player-x p dx env)
  (match-define (rect (posn p-x0 p-y0) p-w p-h) p)
  (match-define (and p-n (rect (posn pn-x0 pn-y0) pn-w pn-h)) (move-rect p dx 0))
  (match-define motion-rect
    (if (negative? dx)
        (rect (posn pn-x0 pn-y0) (+ (abs dx) p-w) p-h)
        (rect (posn p-x0 p-y0) (+ dx p-w) p-h)))
  (define closest-col (closest-colliding motion-rect (rect-top-left p) env))
  (if closest-col
      (match-let* ([(rect (posn col-x0 _) col-w _) closest-col]
                   [new-x0 (if (< p-x0 col-x0)
                               (- col-x0 p-w)
                               (+ col-x0 col-w))])
        (cons (rect (posn new-x0 p-y0) p-w p-h) #t))
      (cons p-n #f)))

(module+ test
  (check-equal? (move-player-x (rect (posn 0 0) 1 1)
                               1
                               '())
                (cons (rect (posn 1 0) 1 1) #f))
  
  (check-equal? (move-player-x (rect (posn 0 0) 1 1)
                               1
                               (list (rect (posn 1 0) 1 1)))
                (cons (rect (posn 0 0) 1 1) #t))
  
  (check-equal? (move-player-x (rect (posn 1 1) 1 1)
                               2
                               (list (rect (posn 3 0) 2 30)))
                (cons (rect (posn 2 1) 1 1) #t))
  
  (check-equal? (move-player-x (rect (posn 1 1) 1 1)
                               2
                               (list (rect (posn 3 0) 2 30)
                                     (rect (posn 40 40) 1 1)))
                (cons (rect (posn 2 1) 1 1) #t))
  
  (check-equal? (move-player-x (rect (posn 0 0) 1 1)
                               10
                               (list (rect (posn 6 -8) 30 40)))
                (cons (rect (posn 5 0) 1 1) #t))
  
  (check-equal? (move-player-x (rect (posn 0 0) 1 1)
                               10
                               (list (rect (posn 3 -4) 1 20)))
                (cons (rect (posn 2 0) 1 1) #t))
  
  (check-equal? (move-player-x (rect (posn 0 0) 1 1)
                               4
                               (list (rect (posn 2 0) 1 1)
                                     (rect (posn 1 0) 1 1)))
                (cons (rect (posn 0 0) 1 1) #t))
  
  (check-equal? (move-player-x (rect (posn 1 0) 1 1)
                               -1
                               (list (rect (posn 0 0) 1 1)))
                (cons (rect (posn 1 0) 1 1) #t)))

;; rect -> rect
;; swap the x and y coordinates as well as the width and height of a rect
(define (flip-rect r)
  (match-define (rect (posn x y) w h) r)
  (rect (posn y x) h w))

;; rect num [listof rect] -> (pair rect bool)
;; attempt to move the player given by the first argument along the y-axis
;; when a collision occurs move as far as possible without colliding
;; returns the new rect for the player as well as if a collision occured
(define (move-player-y p dy env)
  (match-define (cons r col?) (move-player-x (flip-rect p) dy (map flip-rect env)))
  (cons (flip-rect r) col?))

(module+ test
  (check-equal? (move-player-y (rect (posn 0 0) 1 1)
                               1
                               '())
                (cons (rect (posn 0 1) 1 1) #f))
  
  (check-equal? (move-player-y (rect (posn 0 0) 1 1)
                               1
                               (list (rect (posn 0 1) 1 1)))
                (cons (rect (posn 0 0) 1 1) #t))
  
  (check-equal? (move-player-y (rect (posn 1 1) 1 1)
                               2
                               (list (rect (posn 0 3) 30 2)))
                (cons (rect (posn 1 2) 1 1) #t))
  
  (check-equal? (move-player-y (rect (posn 1 1) 1 1)
                               2
                               (list (rect (posn 0 3) 30 2)
                                     (rect (posn 40 40) 1 1)))
                (cons (rect (posn 1 2) 1 1) #t))
  
  (check-equal? (move-player-y (rect (posn 0 0) 1 1)
                               10
                               (list (rect (posn -8 6) 40 30)))
                (cons (rect (posn 0 5) 1 1) #t))
  
  (check-equal? (move-player-y (rect (posn 0 0) 1 1)
                               10
                               (list (rect (posn -4 3) 20 1)))
                (cons (rect (posn 0 2) 1 1) #t))
  
  (check-equal? (move-player-y (rect (posn 0 0) 1 1)
                               4
                               (list (rect (posn 0 2) 1 1)
                                     (rect (posn 0 1) 1 1)))
                (cons (rect (posn 0 0) 1 1) #t)))
