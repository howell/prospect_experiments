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

(define (wasd-to-arrows k)
  (match k
    ["w" "up"]
    ["a" "left"]
    ["s" "down"]
    ["d" "right"]
    [_ #f]))

(define (arrow? key)
  (match key
    [(or 'left 'right 'up 'down) #t]
    [_ #f]))

;; check if two circles with centers p1 p2 and radii r1 r2
;; are colliding
(define (colliding-circles? p1 r1 p2 r2)
  (match-define (posn x1 y1) p1)
  (match-define (posn x2 y2) p2)
  (define d^2 (+ (expt (- x1 x2) 2)
                 (expt (- y1 y2) 2)))
  (define r^2 (expt (+ r1 r2) 2))
  (< d^2 r^2))

(define (circle-center sh)
  (match-define (shape (posn tl-x tl-y) d _) sh)
  (define r (/ d 2))
  (posn (+ tl-x r) (- tl-y r)))

;; protocol description - Take 1 (mimic big-bang program)
;; key board events are injected into the system using messages of the form ('key-event k)
;; the God process listens for key events, translates them into commands, updates the
;; state of the world accordingly, and draws to the canvas.


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

(define (key-press ws key bot-right)
  (match-define (worldstate it1 it2) ws)
  (match key
    [(or "w" "a" "s" "d")
     (match-define (posn dx dy) (key-to-posn-delta (wasd-to-arrows key) DELTA))
     (define it2-n (move-shape-in-canvas it2 dx dy bot-right))
     (if (colliding-circles? (circle-center it1) RADIUS (circle-center it2-n) RADIUS)
         ws
         (struct-copy worldstate ws [it2 it2-n]))]
    [(or "left" "right" "up" "down")
     (match-define (posn dx dy) (key-to-posn-delta key DELTA))
     (define it1-n (move-shape-in-canvas it1 dx dy bot-right))
     (if (colliding-circles? (circle-center it1-n) RADIUS (circle-center it2) RADIUS)
         ws
         (struct-copy worldstate ws [it1 it1-n]))]
    [_ ws]))

(define (make-world dc bot-right)
  (define worldstate0 (worldstate IT1 IT2))
  (draw-ws dc worldstate0)
  (spawn
   (lambda (e ws)
     (match e
       [(message (at-meta `(key-event ,key)))
        (define ws-n (key-press ws key bot-right))
        (draw-ws dc ws-n)
        (transition ws-n '())]
       [_ #f]))
   worldstate0
   (sub `(key-event ,?) #:meta-level 1)))

(define (make-frame)
  (parameterize ((current-eventspace (make-eventspace)))
    (define frame (new frame%
                       [label "My Frame"]
                       [width CANVAS-SIZE]
                       [height CANVAS-SIZE]))
    (define canvas
      (new game-canvas%
           [parent frame]
           [key-handler (lambda (key) (send-ground-message `(key-event ,key)))]))
    (send frame show #t)
    (define-values (x-max y-max) (send canvas get-client-size))
    (make-world (send canvas get-dc) (posn x-max y-max))))

(make-frame)