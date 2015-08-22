#lang prospect

(require racket/gui)
(require racket/draw)

(struct posn (x y) #:transparent)

;; kind of assumed to be rectangular
(struct shape (top-left x-size y-size) #:transparent)

;; clip x to be in the range [min,max]
(define (clip x min max)
  (cond
    [(< x min) min]
    [(> x max) max]
    [else x]))

#;(module+ test
    (require rackunit)
    (check-equal? (clip 5 0 0) 0)
    (check-equal? (clip 0 0 0) 0)
    (check-equal? (clip -1 10 20) 10)
    (check-equal? (clip -1 -20 -10) -10)
    (check-equal? (clip 1 1 2) 1)
    (check-equal? (clip 2 1 2) 2))

;; Move a point on a single dimension so that it is within [floor + margin-low, ceil - margin-high].
;; If the point is already inside that range, return unchanged.
;; Otherwise, make the minimum adjustment to put it into that range.
(define (adjust-1d center margin-low margin-high floor ceil)
  (clip center (+ floor margin-low) (- ceil margin-high)))

#;(module+ test
    (check-equal? (adjust-1d 5 10 10 0 100)
                  10)
    (check-equal? (adjust-1d 95 10 10 0 100)
                  90)
    (check-equal? (adjust-1d 50 10 10 0 100)
                  50)
    ;; weird case
    (check-equal? (adjust-1d 50 100 100 0 100)
                  100))

;; move a shape within a canvas
;; the canvas is assumed to be a rectangle with its top left corner at (0,0) and bottom right corner at bot-right-pos
;; the shape is moved in the x axis by dx and y axis by dy. The shape is not moved outside of the canvas.
(define (move-shape-in-canvas shp dx dy bot-right-pos)
  (match-define (shape (posn tl-x tl-y) x-size y-size) shp)
  (match-define (posn x-max y-max) bot-right-pos)
  (shape (posn (clip (+ tl-x dx) 0 (- x-max x-size))
               (clip (+ tl-y dy) 0 (- y-max y-size)))
         x-size y-size))

;; translates a key press into a directional move - a (posn dx dy)
(define (key-to-posn-delta key delta)
  (match key
    ["left" (posn (- delta) 0)]
    ["right" (posn delta 0)]
    ["up" (posn 0 (- delta))]
    ["down" (posn 0 delta)]
    [_ (posn 0 0)]))

#;(module+ test
    (check-equal? (key-to-posn-delta "left" 10)
                  (posn -10 0))
    (check-equal? (key-to-posn-delta "right" 42)
                  (posn 42 0))
    (check-equal? (key-to-posn-delta "down" 999)
                  (posn 0 999))
    (check-equal? (key-to-posn-delta "up" -3)
                  (posn 0 3))
    (check-equal? (key-to-posn-delta "w" 5)
                  (posn 0 0)))

(define (arrow? key)
  (match key
    [(or 'left 'right 'up 'down) #t]
    [_ #f]))

;; protocol description - Take 1 (mimic big-bang program)
;; key board events are injected into the system using messages of the form (key-event k)
;; the God process listens for key events, translates them into commands, updates the
;; state of the world accordingly, and draws to the canvas.

(struct key-event (key) #:transparent)

(define game-canvas%
  (class canvas%
    (init-field key-handler)
    (define/override (on-char event)
      (define key-code (send event get-key-code))
      (cond
        [(char? key-code) (key-handler (string key-code))]
        [(arrow? key-code) (key-handler (symbol->string key-code))]
        [else (void)]))
    (super-new)))

;; it1 and it2 are both shapes
(struct worldstate (it1 it2) #:transparent)

(define (draw-shape-ellipse dc sh color)
  (match-define (shape (posn tl-x tl-y) x-size y-size) sh)
  (send dc set-brush color 'solid)
  (send dc set-pen color 1 'transparent)
  (send dc set-smoothing 'aligned)
  (send dc draw-ellipse tl-x tl-y x-size y-size))

(define (draw-ws dc ws)
  (match-define (worldstate it1 it2) ws)
  (send dc suspend-flush)
  (send dc clear)
  (draw-shape-ellipse dc it1 "blue")
  (draw-shape-ellipse dc it2 "red")
  (send dc resume-flush))

(define CANVAS-SIZE 400)
(define DELTA 15)
(define RADIUS 20)
(define IT1 (shape (posn 0 0) (* RADIUS 2) (* RADIUS 2)))
(define IT2 (shape (posn 340 340) (* RADIUS 2) (* RADIUS 2)))

(define (make-world dc)
  (define worldstate0 (worldstate IT1 IT2))
  (draw-ws dc worldstate0)
  (spawn
   (lambda (e ws)
     (match e
       [(message (at-meta (key-event key)))
        #f]
       [_ #f]))
   worldstate0
   (sub `(key-event ,?) #:meta-level 1)))

(parameterize ((current-eventspace (make-eventspace)))
  (define frame (new frame%
                     [label "My Frame"]
                     [width CANVAS-SIZE]
                     [height CANVAS-SIZE]))
  (define canvas
    (let* ([circ-radius 20]
           [circ (box (shape (posn 0 0) (* circ-radius 2) (* circ-radius 2)))])
      (new game-canvas%
           [parent frame]
           [key-handler (lambda (key) (send-ground-message (key-event key)))])))
  (send frame show #t)
  (make-world (send canvas get-dc)))