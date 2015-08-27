#lang prospect

(require racket/gui racket/draw racket/set)
(require "./geometry.rkt")

;; Protocol description - take 3 (collision detection)
;; key board events are injected into the system using messages of the form ('key-event k)
;; - each dot process has a unique label P and asserts its current location as ('shape P shape).
;;   To move from shape to shape', P retracts ('shape P shape) and asserts ('shape P shape').
;; - A collision detection process listens for ('shape ? ?) assertions and maintains a state of
;;   where everything is. If a collision is detected between dots P and Q, the process sends a
;;   message ('move L dx dy) to one of P or Q.
;; - When a dot P receives a move message it updates its assertions to reflect the new location
;; - a drawing process D listens for every location and draws/erases to the screen as postions
;;   are asserted and retracted

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

(define (sh-circle-center sh)
  (match-define (shape (posn tl-x tl-y) d _ _) sh)
  (define r (/ d 2))
  (posn (+ tl-x r) (- tl-y r)))

(define DELTA 20)

;; listen for key event messages
;; assert current location
;; listen for move commands from collision detector
(define (dot-behavior label arr-keys bot-right)
  (match-define (arrow-keys up left down right) arr-keys)
  (lambda (e me)
    (match e
      [(message (at-meta `(key-event ,key)))
       (match key
         [(or (== up) (== left) (== down) (== right))
          (define arrow-key (key-to-arrow arr-keys key))
          (match-define (posn dx dy) (key-to-posn-delta arrow-key DELTA))
          (define moved (move-shape-in-canvas me dx dy bot-right))
          (transition moved
                      (patch-seq (retract `(shape ,label ,?))
                                 (assert `(shape ,label ,moved))))]
         [_ #f])]
      [(message `(move ,(== label) ,dx ,dy))
       (define moved (move-shape-in-canvas me dx dy bot-right))
       (transition moved
                   (patch-seq (retract `(shape ,label ,?))
                              (assert `(shape ,label ,moved))))]
      [_ #f])))

(define (spawn-dot shape keys bot-right)
  (define label (gensym "dot"))
  (spawn
   (dot-behavior label keys bot-right)
   shape
   (assert `(shape ,label ,shape))
   (sub `(move ,label ,? ,?))
   (sub `(key-event ,?) #:meta-level 1)))

(define shape-detector
  (compile-projection `(shape ,(?!) ,(?!))))

;; labeled shapes
(struct shape-l (label shape) #:transparent)

(define (match-shapes m)
  (for/set [(x (matcher-project/set m shape-detector))]
    (match-define (list lbl sh) x)
    (shape-l lbl sh)))

;; test if a labeled shape is colliding with any others in a collection of shapes
(define (first-colliding sh others)
  (for/fold [(acc #f)]
            [(other others)]
    (or acc (if (colliding-circles? (sh-circle-center sh) RADIUS (sh-circle-center other) RADIUS)
                other
                #f))))

(define (random-in-range low high)
  (+ low (random (- high low))))

(define (shape-to-circle sh)
  (match-define (shape (posn tl-x tl-y) d _ _) sh)
  (define r (/ d 2))
  (circle (posn (+ r tl-x) (+ r tl-y)) r))

;; if there exists a colliding circle C:
;; determine the previous location of this dot, (x0,y0)
;; determine the line L through (x0, y0) and the desired (colliding) location (x1, y1)
;; (location means center of the circle)
;; fine the nearest point p to (x0, y0) where L interesects C
;; place the new circle center a distance of RADIUS along L (towards (x0, y0)) from p
(define (collision-calculation old-shape new-shape colliding-shape)
  (match-define (list old-c new-c coll-c) (map shape-to-circle (list old-shape new-shape colliding-shape)))
  (printf "\n\n~v\n\n" (list old-c new-c coll-c))
  (match-define (posn x0 y0) (circle-center old-c))
  (define L (line-through-points (circle-center old-c) (circle-center new-c)))
  (printf "\nL = ~vx + ~vy = ~v\n" (line-a L) (line-b L) (line-c L))
  (match-define (posn int-x int-y)
    (match (intersection-circle-line coll-c L)
      [#f (error 'collision-calculation (format "no intersection ~v ~v" new-c L))]
      [(cons p1 p2) (if (< (point-distance p1 (circle-center old-c)) (point-distance p2 (circle-center old-c)))
                        p1
                        p2)]
      [p p]))
  (define x-direction
    (cond
      [(equal? int-x x0) 0]
      [(< int-x x0) 1]
      [else -1]))
  (define y-direction
    (cond
      [(equal? int-y y0) 0]
      [(< int-y y0) 1]
      [else -1]))
  (printf "\n\nint-x = ~v, int-y = ~v\n\n" int-x int-y)
  (printf "\n\nx0 = ~v, y0 = ~v\n\n" x0 y0)
  (cond
    [(zero? x-direction) (posn 0 (* y-direction (abs (- y0 int-y))))]
    [(zero? y-direction) (posn (* x-direction (abs (- x0 int-x))) 0)]
    [else
     (define θ (atan (/ (abs (- y0 int-y)) (abs (- x0 int-x)))))
     (define r (circle-radius old-c))
     (define dx (* x-direction (sin θ) r))
     (define dy (* y-direction (cos θ) r))
     (posn dx dy)]))

;; 2nd attempt
;; let (x0, y0) denote the center of the dot's previous location
;; let (x1, y1) denote the center of the dot's new (colliding) location
;; let (x2, y2) denote the center of the circle the dot collided with
;; let L be the line passing through (x0, y0) and (x1, y1)
;; we want to determine a point on the line L between (x0, y0) and (x1, y1) such that there is no collision
;; particularly, we want the closest point on L to the colliding shape where there is no collision
;; let r1 be the radius of the moving circle
;; let r2 be the radius of the colliding circle
;; then the center of the relocated circle will be on L a distance of r1 + r2 away from the (x2, y2)
;; we can find this point by imagining a circle C with center (x2, y2) and radius r1 + r2
;; the desired point will be the intersection of C and L between (x0, y0) and (x1, y1)
(define (collision-calc2 old-shape new-shape colliding-shape)
  (match-define (list old-c new-c coll-c) (map shape-to-circle (list old-shape new-shape colliding-shape)))
  (match-define (circle (posn x0 y0) r1) old-c)
  (match-define (posn x1 y1) (circle-center new-c))
  (match-define (circle (posn x2 y2) r2) coll-c)
  (define L (line-through-points (posn x0 y0) (posn x1 y1)))
  (printf "\n\nL = ~v\n\n" L)
  (define C (circle (posn x2 y2) (+ r1 r2)))
  (printf "\n\nC = ~v\n\n" C)
  (match-define (posn x3 y3)
    (match (intersection-circle-line C L)
      [#f (error 'collision-calc (format "no intersection ~v ~v" C L))]
      [(cons p1 p2) (if (< (point-distance p1 (circle-center new-c)) (point-distance p2 (circle-center new-c)))
                        p1
                        p2)
                    #;(printf "\n\n~v\n~v\n\n" p1 p2) #;(if (point-between? (posn x0 y0) (posn x1 y1) p1)
                        p1
                        p2)]
    [p p]))
  (printf "\n\n(x3, y3) = (~v, ~v)\n\n" x3 y3)
  (posn (exact-round (- x3 x1)) (exact-round (- y3 y1))))

;; listen for the location of every dot. When a collision is detected between two dots,
;; tell one of them to move a random amount.
(define (spawn-collision-detector)
  (define BACKOFF (/ DELTA 2))
  (spawn
   (lambda (e dots-old)
     (match e
       [(patch added removed)
        (define vacated (match-shapes removed))
        (define new-locs (match-shapes added))
        (define dots-n
          (for/fold ([acc dots-old])
                    ([removed vacated])
            (hash-remove acc (shape-l-label removed))))
        (match-define (cons next-dots msgs)
          (for/fold ([acc (cons dots-n '())])
                    ([new-dot new-locs])
            (match-define (cons dots-n msgs) acc)
            (match-define (shape-l label sh) new-dot)
            (define dots-n2 (hash-set dots-n label sh))
            (match (first-colliding sh (hash-values dots-n))
              [#f (cons dots-n2 msgs)]
              [colliding-sh
               (define old-shape (hash-ref dots-old label))
               (match-define (posn dx dy) (collision-calc2 old-shape sh colliding-sh))
               (cons dots-n2 (cons (message `(move ,label ,dx ,dy))
                                   msgs))])))
        (transition next-dots msgs)]
       [_ #f]))
   (hash)
   (sub `(shape ,? ,?))))


(define (draw-shape-ellipse dc sh smoothing)
  (match-define (shape (posn tl-x tl-y) x-size y-size color) sh)
  (send dc set-brush color 'solid)
  (send dc set-pen color 1 'solid)
  (send dc set-smoothing smoothing)
  (send dc draw-ellipse tl-x tl-y x-size y-size))

(define (draw-shapes dc shapes)
  (send dc suspend-flush)
  (send dc clear)
  (for ([sh shapes])
    (draw-shape-ellipse dc sh 'aligned))
  (send dc resume-flush))

(define (update-canvas dc added removed)
  (define background (send dc get-background))
  (send dc suspend-flush)
  (for ([sh removed])
    (draw-shape-ellipse dc (struct-copy shape sh [color background]) 'unsmoothed))
  (for ([sh added])
    (draw-shape-ellipse dc sh 'aligned))
  (send dc resume-flush))

(define (spawn-drawer dc)
  (spawn
   (lambda (e shapes)
     (match e
       [(patch added removed)
        (define vacated (match-shapes removed))
        (define moved (match-shapes added))
        (define new-state (set-union (set-subtract shapes vacated) moved))
        (draw-shapes dc (set-map new-state shape-l-shape))
        (transition new-state '())]
       [_ #f]))
   (set)
   (sub `(shape ,? ,?))))

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
(define DOT2 (shape (posn 340 100) (* RADIUS 2) (* RADIUS 2) "red"))
(define DOT3 (shape (posn 0 340) (* RADIUS 1) (* RADIUS 1) "green"))
(define DOT4 (shape (posn 340 0) (* RADIUS 2) (* RADIUS 2) "purple"))

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

(spawn-collision-detector)
(spawn-dot DOT1 (arrow-keys "w" "a" "s" "d") (unbox bot-right))
(spawn-dot DOT2 (arrow-keys "up" "left" "down" "right") (unbox bot-right))
(spawn-dot DOT3 (arrow-keys "k" "h" "j" "l") (unbox bot-right))
(spawn-dot DOT4 (arrow-keys "i" "y" "u" "o") (unbox bot-right))