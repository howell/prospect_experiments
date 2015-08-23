#lang prospect

(require racket/set)

;; protocol description - Take 2 (process for each dot)
;; key board events are injected into the system using messages of the form ('key-event k)
;; each dot process P asserts its current location as ('shape shape) and listens for the location of every other thing
;; when
;; a drawing process D listens for every location and draws/erases to the screen as postions are asserted and retracted

(struct posn (x y) #:transparent)
;; kind of assumed to be rectangular
(struct shape (top-left x-size y-size color) #:transparent)
(struct dot-state (my-shape everyone-else) #:transparent)

(struct arrow-keys (up left down right) #:transparent)

(define shape-detector
  (compile-projection `(shape ,(?!))))

(define (key-to-arrow arr-keys key)
  (match-define (arrow-keys up left down right) arr-keys)
  (match key
    [(== up) "up"]
    [(== left) "left"]
    [(== right) "right"]
    [(== down) "down"]
    [_ (error 'key-to-arrow "expected arrow key")]))

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
  (match-define (shape (posn tl-x tl-y) x-size y-size _) shp)
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
  (match-define (shape (posn tl-x tl-y) d _ _) sh)
  (define r (/ d 2))
  (posn (+ tl-x r) (- tl-y r)))

(define DELTA 20)

(define (dot-behavior arr-keys bot-right)
  (match-define (arrow-keys up left down right) arr-keys)
  (lambda (e s)
    (match-define (dot-state me others) s)
    (match e
      [(patch added removed)
       ;; update the position of all shapes
       (define vacated (matcher-project/set removed shape-detector))
       (define moved (matcher-project/set added shape-detector))
       (transition (set-remove (set-union (set-subtract others vacated) moved) me) '())]
      [(message (at-meta `(key-event ,key)))
       (match key
         [(or (== up) (== left) (== down) (== right))
          (define arrow-key (key-to-arrow arr-keys key))
          (match-define (posn dx dy) (key-to-posn-delta arrow-key DELTA))
          (define moved (move-shape-in-canvas me dx dy bot-right))
          (define any-colliding? (for/fold [(acc #f)]
                                           [(other others)]
                                   (or acc (colliding-circles? (circle-center moved) (circle-center other)))))
          (if any-colliding?
              #f
              (transition (dot-state moved others)
                          (patch-seq (retract `(shape ,?))
                                     (assert `(shape ,moved)))))])]
         [_ #f]
      [_ #f])))

(define (spawn-dot shape keys)
  (spawn
   (dot-behavior keys)
   (dot-state shape (set))
   (assert `(shape ,shape))
   (sub `(shape ,?))
   (sub `(key-event ,?) #:meta-level 1)))