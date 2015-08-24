#lang prospect

(require racket/set racket/gui racket/draw)

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
  (match-define (shape (posn tl-x tl-y) x-size y-size c) shp)
  (match-define (posn x-max y-max) bot-right-pos)
  (shape (posn (clip (+ tl-x dx) 0 (- x-max x-size))
               (clip (+ tl-y dy) 0 (- y-max y-size)))
         x-size y-size c))

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

(define shape-detector
  (compile-projection `(shape ,(?!))))

(define (match-shapes m)
  (for/set [(x (matcher-project/set m shape-detector))]
    (match-define (list sh) x)
    sh))

(define (dot-behavior arr-keys bot-right)
  (match-define (arrow-keys up left down right) arr-keys)
  (lambda (e s)
    (match-define (dot-state me others) s)
    (match e
      [(patch added removed)
       ;; update the position of all shapes
       (define vacated (match-shapes removed))
       (define moved (match-shapes added))
       (transition (dot-state me (set-remove (set-union (set-subtract others vacated) moved) me)) '())]
      [(message (at-meta `(key-event ,key)))
       (match key
         [(or (== up) (== left) (== down) (== right))
          (define arrow-key (key-to-arrow arr-keys key))
          (match-define (posn dx dy) (key-to-posn-delta arrow-key DELTA))
          (define moved (move-shape-in-canvas me dx dy bot-right))
          (define any-colliding? (for/fold [(acc #f)]
                                           [(other others)]
                                   (or acc (colliding-circles? (circle-center moved) RADIUS (circle-center other) RADIUS))))
          (if any-colliding?
              #f
              (transition (dot-state moved others)
                          (patch-seq (retract `(shape ,?))
                                     (assert `(shape ,moved)))))]
         [_ #f])]
      [_ #f])))

(define (spawn-dot shape keys bot-right)
  (spawn
   (dot-behavior keys bot-right)
   (dot-state shape (set))
   (assert `(shape ,shape))
   (sub `(shape ,?))
   (sub `(key-event ,?) #:meta-level 1)))

(define (draw-shape-ellipse dc sh)
  (match-define (shape (posn tl-x tl-y) x-size y-size color) sh)
  (send dc set-brush color 'solid)
  (send dc set-pen color 1 'transparent)
  (send dc set-smoothing 'aligned)
  (send dc draw-ellipse tl-x tl-y x-size y-size))

(define (draw-shapes dc shapes)
  (send dc suspend-flush)
  (send dc clear)
  (for ([sh shapes])
    (draw-shape-ellipse dc sh))
  (send dc resume-flush))

(define (spawn-drawer dc)
  (spawn
   (lambda (e shapes)
     (match e
       [(patch added removed)
        ;; update the position of all shapes
        (define vacated (match-shapes removed))
        (define moved (match-shapes added))
        (define new-state (set-union (set-subtract shapes vacated) moved))
        (draw-shapes dc new-state)
        (transition new-state '())]
       [_ #f]))
   (set)
   (sub `(shape ,?))))

;; gui stuff
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

(define RADIUS 20)
(define DOT1 (shape (posn 0 0) (* RADIUS 2) (* RADIUS 2) "blue"))
(define DOT2 (shape (posn 340 340) (* RADIUS 2) (* RADIUS 2) "red"))
(define DOT3 (shape (posn 0 340) (* RADIUS 2) (* RADIUS 2) "green"))

(define (arrow? key)
  (match key
    [(or 'left 'right 'up 'down) #t]
    [_ #f]))

(define bot-right (box #f))

(define (make-frame width height)
  (parameterize ((current-eventspace (make-eventspace)))
    (define frame (new frame%
                       [label "My Frame"]
                       [width width]
                       [height height]))
    (define canvas
      (new game-canvas%
           [parent frame]
           [key-handler (lambda (key) (send-ground-message `(key-event ,key)))]))
    (send frame show #t)
    (define-values (x-max y-max) (send canvas get-client-size))
    (set-box! bot-right (posn x-max y-max))
    (define dc (send canvas get-dc))
    (spawn-drawer dc)))

(make-frame 400 400)

(spawn-dot DOT1 (arrow-keys "w" "a" "s" "d") (unbox bot-right))
(spawn-dot DOT2 (arrow-keys "up" "left" "down" "right") (unbox bot-right))
(spawn-dot DOT3 (arrow-keys "k" "h" "j" "l") (unbox bot-right))