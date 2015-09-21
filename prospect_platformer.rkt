#lang prospect

;; Events
;; keyboard events are injected into the system at the ground level as either
;; (key-press key) or (key-release key) messages
;; 
;; processes in the system:
;; Level Manager Process
;;   Needs to create the first level when spawned
;;   When the player reaches the goal (a (level-complete) message) retract the current level and spawn the next by:
;;     - spawning a game logic process
;;     - spawning a rendering process
;;     - asserting the environment as (static rect)
;;     - asserts the initial location of the player as (player rect)
;;     - spawning any enemies
;; Game Logic Process:
;;   Decides where the player is on the map and when the game is over.
;;   Processes (jump-request) messages. If the player is allowed to jump sends a (jump) message.
;;   Listens for (move-x id dx) and (move-y id dy) messages and attempts to move the player (id = 'player) or enemy accordingly.
;;   When a (move-y id _) command results in a collision with the environment a (y-collision id) message is sent.
;;   The new location of the player is sent as a (player rect) message.
;;   The new location of an enemy is sent as a (enemy id rect) message.
;;   The environment is determined by assertions of the shape (static rect).
;;   The initial location of enemies is determined by assertions of the shape (enemy id rect).
;;   If the player kills an enemy then sends a (kill-enemy id) message.
;;   Asserts the location of the goal as (goal rect).
;;   When the player reaches the goal, quits and sends the message (level-complete)
;;   When the player loses (leaves the map), quits and sends the message (defeat)
;; Timer Process:
;;   Sends a (timer-tick) message at a fixed rate
;; Player Process:
;;   Translates keyboard event messages into movement commands.
;;   asserts (move-left) or (move-right) while the left or right arrow key is held down
;;   sends a (jump-request) message when space is pressed
;; Horizontal Motion Process:
;;   Interprets the output of the Player Process into commands for the Game Logic Process.
;;   Sends the messsage (move-x 'player +-dx) on every (timer-tick) while (move-left) or (move-right) is being asserted.
;; Vertical Motion Process:
;;   Represents gravity and the player's attempts to fight gravity by jumping.
;;   Sends (move-y 'player dy) every (timer-tick).
;;   Interprets (jump) messages into upward motion.
;;   When a (y-collision 'player) is detected reset velocity to 0
;; Enemy Process(es):
;;   Asserts the initial position and id as (enemy id (rect posn0 w h)).
;;   Moves by sending (move-x id dx) and (move-y id dy) messages.
;;   Quits when a (kill-enemy id) message is received.
;; Rendering Process:
;;   Tracks and draws the state of the game:
;;   - (static rect) assertions
;;   - (player rect) messages
;;   - (goal rect) messages
;;   - (level-complete)/(defeat) assertions
;;   - (enemy id rect) messages
;;   Redraws every timer tick.
;; Enemy Making Process:
;;   Spawns enemy processes on demand.
;;   Listens for (spawn-enemy spawn) and spawns the argument

(require "./geometry.rkt"
         "./periodic_timer.rkt"
         rackunit
         prospect/drivers/timer
         racket/set
         racket/gui
         racket/draw
         racket/block)

;; velocity and acceleration
(struct motion (v a) #:transparent)

;; int
(struct move-x (id dx) #:transparent)

;; int
(struct move-y (id dy) #:transparent)

(struct move-left () #:transparent)
(struct move-right () #:transparent)

(struct jump-request () #:transparent)
(struct jump () #:transparent)

;; any
(struct y-collision (id) #:transparent)

;; rect
(struct player (rect) #:transparent)

;; any * rect
(struct enemy (id rect) #:transparent)

;; spawn
(struct spawn-enemy (spawn) #:transparent)

;; any
(struct kill-enemy (id) #:transparent)

;; rect
(struct goal (rect) #:transparent)

;; rect
(struct static (rect) #:transparent)

;; key
(struct key-press (key) #:transparent)
(struct key-release (key) #:transparent)

(struct timer-tick () #:transparent)

(struct level-complete () #:transparent)
(struct victory () #:transparent)
(struct defeat () #:transparent)

;; rect * (listof rect) * rect * (hashof symbol -> enemy)
(struct game-state (player env goal enemies) #:transparent)

;; rect * (listof rect) * rect * (listof spawn)
(struct level (player0 env0 goal enemies) #:transparent)

;; translate key presses into commands
;; asserts (move-left)/(move-right) while the left/right arrow key is held down
;; space becomes a (jump-request) message
(define (player-behavior e s)
  ;; state is (U 'left 'right #f)
  (match e
    [(message (at-meta (key-press key)))
     (match key
       ['left (if s
                  #f
                  (transition key (assert (move-left))))]
       ['right (if s
                   #f
                   (transition key (assert (move-right))))]
       [#\space (transition s (message (jump-request)))]
       [_ #f])]
    [(message (at-meta (key-release (== s))))
     (transition #f
                 (list (retract (move-left))
                       (retract (move-right))))]
    [_ #f]))

(define (spawn-player)
  (spawn
   player-behavior
   #f
   (sub (key-press ?) #:meta-level 1)
   (sub (key-release ?) #:meta-level 1)))

(define left-matcher (compile-projection (move-left)))
(define right-matcher (compile-projection (move-right)))
(define (not-set-empty? s) (not (set-empty? s)))

;; the horizontal motion behavior tries to move the player along the x-axis
;; by sending the messsage (move-x +-dx) each timer tick while (move-left) or
;; (move-right) is being asserted
(define ((horizontal-motion-behavior dx) e s)
  ;; state is (U #f 'left 'right)
  (match e
    [(patch p-added p-removed)
     (define left-added? (not-set-empty? (matcher-project/set p-added left-matcher)))
     (define left-removed? (not-set-empty? (matcher-project/set p-removed left-matcher)))
     (define right-added? (not-set-empty? (matcher-project/set p-added right-matcher)))
     (define right-removed? (not-set-empty? (matcher-project/set p-removed right-matcher)))
     (cond
       [left-added? (transition 'left '())]
       [right-added? (transition 'right '())]
       [(or left-removed? right-removed?) (transition #f '())]
       [else #f])]
    [(message (timer-tick))
     (match s
       ['left (transition s (message (move-x 'player (- dx))))]
       ['right (transition s (message (move-x 'player dx)))]
       [_ #f])]
    [_ #f]))

(define (spawn-horizontal-motion dx)
  (spawn (horizontal-motion-behavior dx)
         #f
         (sub (move-left))
         (sub (move-right))
         (sub (timer-tick))))

;; the vertical motion behavior tries to move the player downward by sending (move-y dy)
;; this happens periodically when the timer sends a (timer-tick) message
;; when a (jump) message is received, temporarily move the player upward
;; when a (y-collision) is detected reset velocity to 0
;; state is a (cons bool motion)
(define ((vertical-motion-behavior jump-v v-max) e s)
  (match-define (cons jumping? motion-old) s)
  (match e
    [(message (jump))
     (transition (cons #t (motion jump-v (motion-a motion-old))) '())]
    [(message (timer-tick))
     (define motion-n (motion (min v-max (+ (motion-v motion-old) (motion-a motion-old))) (motion-a motion-old)))
     (transition (cons jumping? motion-n) (list (message (move-y 'player (motion-v motion-old)))))]
    [(message (y-collision 'player))
     (transition (cons #f (motion 0 (motion-a motion-old))) '())]
    [_ #f]))

(define (spawn-vertical-motion gravity jump-v max-v)
  (spawn (vertical-motion-behavior jump-v max-v)
         (cons #f (motion 0 gravity))
         (sub (jump))
         (sub (timer-tick))
         (sub (y-collision 'player))))

;; create a clock that sends (timer-tick) every period-ms
(define (spawn-clock period-ms)
  (periodically period-ms (lambda () (message (timer-tick)))))

(define static-detector (compile-projection (static (?!))))
(define static-rects-matcher
  (lambda (m)
    (set-map (matcher-project/set m static-detector) car)))

(define enemy-detector (compile-projection (enemy (?!) (?!))))
(define (patch-enemies p)
  (define-values (added removed) (patch-project/set p enemy-detector))
  (values (set-map added (lambda (l) (enemy (first l) (second l))))
          (set-map removed (lambda (l) (enemy (first l) (second l))))))

;; (listof enemy) (listof enemy) (hashof symbol -> enemy) -> (hashof symbol -> enemy)
;; update a hash of enemies with additions and subtractions
(define (update-enemy-hash added-enemies removed-enemies h)
  (define h2 (for/fold ([acc h])
                       ([e removed-enemies])
               (hash-remove acc (enemy-id e))))
  (for/fold ([acc h2])
            ([e added-enemies])
    (hash-set acc (enemy-id e) e)))

(check-equal? (update-enemy-hash '() '() (hash))
              (hash))

(check-equal? (update-enemy-hash (list (enemy 'foo (rect (posn 0 0) 1 1))) '() (hash))
              (hash 'foo (enemy 'foo (rect (posn 0 0) 1 1))))

(check-equal? (update-enemy-hash '()
                                 (list (enemy 'foo (rect (posn 0 0) 1 1)))
                                 (hash 'foo (enemy 'foo (rect (posn 0 0) 1 1))))
              (hash))

(check-equal? (update-enemy-hash (list (enemy 'foo (rect (posn 1 1) 1 1)))
                                 (list (enemy 'foo (rect (posn 0 0) 1 1)))
                                 (hash 'foo (enemy 'foo (rect (posn 0 0) 1 1))))
              (hash 'foo (enemy 'foo (rect (posn 1 1) 1 1))))

;; the game logic process keeps track of the location of the player and the environment.
;; processes move-x and move-y commands from the player and enemies. When a collision
;; along the y-axis occurs it sends a (y-collision id) message with the id of the moving
;; object.
;; sends a message with the location of the player every time it moves, (player rect)
;; sends a message with the location of an enemy each time it moves, (enemy id rect)
;; If the player is moving down/enemy is moving up and they collide, send a (kill-enemy id)
;; message. Otherwise if the player and the enemy collide the game is over.
;; asserts the location of the goal as (goal g)
;; quits and messages (level-complete) if the player reaches the goal
;; quits and messages (defeat) if the player leaves the map
(define (game-logic-behavior e s)
  (match-define (game-state player-old env-old cur-goal enemies-old) s)
  (match e
    [(message (move-x 'player dx))
     (define player-n (car (move-player-x player-old dx env-old)))
     (cond
       [(overlapping-rects? player-n cur-goal)
        (quit (list (message (level-complete))))]
       [(not (overlapping-rects? player-n (rect (posn 0 0) (posn-x bot-right) (posn-y bot-right))))
        (quit (list (message (defeat))))]
       [(ormap (lambda (e) (overlapping-rects? player-n e)) (map enemy-rect (hash-values enemies-old)))
        (quit (list (message (defeat))))]
       [else (transition (game-state player-n env-old cur-goal enemies-old)
                         (list (message (player player-n))))])]
    [(message (move-y 'player dy))
     (match-define (cons player-n col?) (move-player-y player-old dy env-old))
     (define col-enemies (filter (lambda (e) (overlapping-rects? player-n (enemy-rect e))) (hash-values enemies-old)))
     (define enemies-new (for/fold ([acc enemies-old])
                                   ([e col-enemies])
                           (hash-remove acc (enemy-id e))))
     (define kill-messages (map (lambda (e) (message (kill-enemy (enemy-id e)))) col-enemies))
     (cond
       [(overlapping-rects? player-n cur-goal)
        (quit (list (message (level-complete))))]
       [(not (overlapping-rects? player-n (rect (posn 0 0) (posn-x bot-right) (posn-y bot-right))))
        (quit (list (message (defeat))))]
       [else (transition (game-state player-n env-old cur-goal enemies-new)
                         (cons kill-messages
                               (cons (message (player player-n))
                                     (if col?
                                         (list (message (y-collision 'player)))
                                         '()))))])]
    [(message (move-x enemy-id dx))
     (define maybe-enemy (hash-ref enemies-old enemy-id #f))
     ;; the enemy might not be in the hash if it was recently killed
     (if maybe-enemy
         (block
          (match-define (enemy _ e-rect) maybe-enemy)
          (define e-rect-new (car (move-player-x e-rect dx env-old)))
          (define enemies-new (hash-set enemies-old enemy-id (enemy enemy-id e-rect-new)))
          (if (overlapping-rects? player-old e-rect-new)
              (quit (list (message (defeat))))
              (transition (game-state player-old env-old cur-goal enemies-new)
                          (message (enemy enemy-id e-rect-new)))))
         #f)]
    [(message (move-y enemy-id dy))
     (define maybe-enemy (hash-ref enemies-old enemy-id #f))
     ;; the enemy might not be in the hash if it was recently killed
     (if maybe-enemy
         (block
          (match-define (enemy _ e-rect) maybe-enemy)
          (match-define (cons e-rect-new col?) (move-player-y e-rect dy env-old))
          (define enemies-new (hash-set enemies-old enemy-id (enemy enemy-id e-rect-new)))
          (when (overlapping-rects? player-old e-rect-new)
            (if (positive? dy)
                (quit (list (message (defeat)))) ;; enemy fell on player
                (transition (game-state player-old env-old cur-goal (hash-remove enemies-new enemy-id))
                            (list (message (kill-enemy enemy-id))))))
          (transition (game-state player-old env-old cur-goal enemies-new)
                      (cons (message (enemy enemy-id e-rect-new))
                            (if col?
                                (list (message (y-collision enemy-id)))
                                '()))))
         #f)]
    [(message (jump-request))
     ;; check if there is something right beneath the player
     (if (cdr (move-player-y (game-state-player s) 1 (game-state-env s)))
         (transition s (message (jump)))
         #f)]
    [(patch p-added p-removed)
     (define removed (static-rects-matcher p-removed))
     (define added (static-rects-matcher p-added))
     (define new-env (append added (remove* removed (game-state-env s))))
     (define-values (enemies-added enemies-removed) (patch-enemies e))
     (define enemies-new (update-enemy-hash enemies-added enemies-removed enemies-old))
     (transition (game-state player-old new-env cur-goal enemies-new)
                 (map message enemies-added))]
    [_ #f]))

;; rect goal -> spawn
(define (spawn-game-logic player0 goal0)
  (spawn game-logic-behavior
         (game-state player0 '() goal0 (hash))
         (sub (move-x ? ?))
         (sub (move-y ? ?))
         (sub (static ?))
         (sub (jump-request))
         (sub (enemy ? ?))
         (assert (player player0))
         (assert (goal goal0))))

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
              (rect (posn 3 0) 1 1))

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
              (cons (rect (posn 1 0) 1 1) #t))

;; rect num [listof rect] -> (pair rect bool)
;; attempt to move the player given by the first argument along the y-axis
;; when a collision occurs move as far as possible without colliding
;; returns the new rect for the player as well as if a collision occured
(define (move-player-y p dy env)
  (match-define (rect (posn p-x0 p-y0) p-w p-h) p)
  (match-define (and p-n (rect (posn pn-x0 pn-y0) pn-w pn-h)) (move-rect p 0 dy))
  (match-define motion-rect
    (if (negative? dy)
        (rect (posn pn-x0 pn-y0) p-w (+ (abs dy) p-h))
        (rect (posn p-x0 p-y0) p-w (+ (abs dy) p-h))))
  (define closest-col (closest-colliding motion-rect (rect-top-left p) env))
  (if closest-col
      (match-let* ([(rect (posn _ col-y0) _ col-h) closest-col]
                   [new-y0 (if (< p-y0 col-y0)
                               (- col-y0 p-h)
                               (+ col-y0 col-h))])
        (cons (rect (posn p-x0 new-y0) p-w p-h) #t))
      (cons p-n #f)))

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
              (cons (rect (posn 0 0) 1 1) #t))

(define (star-points scl)
  (map (lambda (pr) (cons (* scl (car pr)) (* scl (cdr pr))))
       `((0 . 10)
         (2 . 6)
         (0 . 4)
         (3 . 4)
         (5 . 0)
         (7 . 4)
         (10 . 4)
         (8 . 6)
         (10 . 10)
         (5 . 7))))

;; drawing-context goal -> void
;; draws the goal as a 3x3 yellow star
(define (draw-goal dc g)
  (match-define (rect (posn x0 y0) _ _) g)
  (send dc set-brush "yellow" 'solid)
  (send dc set-pen "yellow" 1 'solid)
  (send dc set-smoothing 'aligned)
  (send dc draw-polygon (star-points 5) x0 y0))

;; drawing-context rect color -> void
;; draws a black rectangle
(define (draw-rect dc r color)
  (match-define (rect (posn x0 y0) w h) r)
  (send dc set-brush color 'solid)
  (send dc set-pen color 1 'solid)
  (send dc draw-rectangle x0 y0 w h))

;; drawing-context rect (listof rect) goal (listof enemy) -> void
;; draws the game
(define (draw-game dc player env gl enemies)
  (send dc suspend-flush)
  (send dc clear)
  (for ([r env])
    (draw-rect dc r "black"))
  (draw-goal dc gl)
  (for ([e enemies])
    (draw-rect dc (enemy-rect e) "red"))
  (draw-rect dc player "blue")
  (send dc resume-flush))

(define (big-text dc text color)
  (send dc suspend-flush)
  (send dc clear)
  (send dc set-text-mode 'solid)
  (send dc set-text-foreground color)
  (define fnt (make-object font% 100 'default))
  (send dc set-font fnt)
  (send dc draw-text text (/ (posn-x bot-right) 4) (/ (posn-y bot-right) 4))
  (send dc resume-flush))

(define (draw-victory dc)
  (big-text dc "Victory!" "green"))

(define (draw-defeat dc)
  (big-text dc "Defeat." "red"))

;; draw the player (determined from (player rect) messages), static objects
;; defined by (static rect) and (goal rect) assertions and update the screen
;; each (timer-tick) time the player moves - (player rect) messages.
;; if (victory) or (defeat) is detected then quit and draw something special
(define ((render-behavior dc) e s)
  ;; state is a game-state struct
  (match-define (game-state old-player old-env old-goal old-enemies) s)
  (match e
    [(patch p-added p-removed)
     (define added (static-rects-matcher p-added))
     (define removed (static-rects-matcher p-removed))
     (define new-env (append added (remove* removed old-env)))
     (define player-s (matcher-project/set p-added (compile-projection (player (?!)))))
     (define new-player (if (set-empty? player-s) old-player (car (set-first player-s))))
     (define goal-s (matcher-project/set p-added (compile-projection (goal (?!)))))
     (define new-goal (if (set-empty? goal-s) old-goal (car (set-first goal-s))))
     ;#;(define victory? (not-set-empty? (matcher-project/set p-added (compile-projection (victory)))))
     (define-values (enemies-added enemies-removed) (patch-enemies e))
     (define enemies-new (update-enemy-hash enemies-added enemies-removed old-enemies))
     (cond
       #;[victory?
          (printf "victory!\n")
          (draw-victory dc)
          (quit '())]
       [else
        (transition (game-state new-player new-env new-goal enemies-new)
                    '())])]
    [(message (player new-player))
     (transition (game-state new-player old-env old-goal old-enemies)
                 '())]
    [(message (enemy id rect))
     (define new-enemies (hash-set old-enemies id (enemy id rect)))
     (transition (game-state old-player old-env old-goal new-enemies)
                 '())]
    [(message (kill-enemy id))
     (define new-enemies (hash-remove old-enemies id))
     (transition (game-state old-player old-env old-goal new-enemies)
                 '())]
    [(message (victory))
     (printf "\nvictory!\n")
     (draw-victory dc)
     (quit '())]
    [(message (defeat))
     (printf "\n\ndefeat\n\n")
     (transition (game-state (rect (posn -100 -100) 0 0) '() (rect (posn -100 -100) 0 0) (hash))
                 '())]
    [(message (timer-tick))
     (draw-game dc old-player old-env old-goal (hash-values old-enemies))
     #f]
    [_ #f]))

(define (spawn-renderer dc)
  (spawn
   (render-behavior dc)
   (game-state (rect (posn 0 0) 0 0) '() (rect (posn -100 -100) 0 0) (hash))
   (sub (timer-tick))
   (sub (static ?))
   (sub (player ?))
   (sub (enemy ? ?))
   (sub (goal ?))
   (sub (defeat))
   (sub (kill-enemy ?))
   (sub (victory))
   (sub (level-complete))))

;; level -> (constreeof action)
(define (level->actions l)
  (match-define (level player0 env0 goal0 enemies) l)
  (flatten (list (assert (player player0))
                 (map (lambda (r) (assert (static r))) env0)
                 #;(spawn-game-logic player0 goal0)
                 (map (lambda (e) (assert (spawn-enemy e))) enemies))))

;; state is a (non-empty-listof level), the first of which is the current level
;; need a way to kill all enemies
(define (level-manager-behavior e s)
  (match e
    [(message (defeat))
     (match-define (level player0 env0 goal0 enemies) (car s))
     (transition s (list (spawn-game-logic player0 goal0)
                         (retract (static ?))
                         (retract (player ?))
                         (retract (spawn-enemy ?))
                         (apply patch-seq (flatten (level->actions (car s))))))]
    [(message (level-complete))
     (match (cdr s)
       [(cons next-level _) (transition (cdr s) (list (spawn-game-logic (level-player0 next-level) (level-goal next-level))
                                                      (retract (static ?))
                                                      (retract (player ?))
                                                      (retract (spawn-enemy ?))
                                                      (apply patch-seq (flatten (level->actions next-level)))))]
       [_ (quit (list (message (victory))))])]
    [_ #f]))

;; (non-empty-listof level) -> spawn
(define (spawn-level-manager levels)
  (spawn
   level-manager-behavior
   levels
   (apply patch-seq (flatten (list (sub (defeat))
                                   (sub (level-complete))
                                   (level->actions (first levels)))))))

;; enemy spawner
(define (enemy-spawner-behavior e s)
  (match e
    [(? patch/added? p)
     (define added (matcher-project/set (patch-added p) (compile-projection (spawn-enemy (?!)))))
     (define spawns (set-map added car))
     (transition s spawns)]
    [_ #f]))

(define (spawn-enemy-spawner)
  (spawn enemy-spawner-behavior
         (void)
         (sub (spawn-enemy ?))))

;; gui stuff
(define game-canvas%
  (class canvas%
    (init-field key-handler)
    (define/override (on-char event)
      (define key-code (send event get-key-code))
      (define release-code (send event get-key-release-code))
      (cond
        [(release? key-code) (key-handler (key-release release-code))]
        [else (key-handler (key-press key-code))]))
    (super-new)))

(define (space? key)
  (equal? key #\space))

(define (release? key)
  (equal? key 'release))

(define (arrow? key)
  (match key
    [(or 'left 'right 'up 'down) #t]
    [_ #f]))

;; global (mutable) variable with the canvas's bottom-right posn 
(define bot-right #f)

(define (make-frame width height)
  (parameterize ((current-eventspace (make-eventspace)))
    (define frame (new frame%
                       [label "My Frame"]
                       [width width]
                       [height height]))
    (define canvas
      (new game-canvas%
           [parent frame]
           [key-handler send-ground-message]))
    (send canvas focus)
    (send frame show #t)
    (define-values (x-max y-max) (send canvas get-client-size))
    (set! bot-right (posn x-max y-max))
    (define dc (send canvas get-dc))
    (spawn-renderer dc)))

(define PLAYER0 (rect (posn 0 0) 8 32))
(define GOAL0 (rect (posn 500 150) 50 50))

(define FRAMES-PER-SEC 24)

(define GRAVITY-PER-SEC 5)
(define JUMP-V -4)

(define MAX-V 8)

(define DX-PER-SEC 75)

(make-frame 600 400)
(spawn-timer-driver)
(spawn-player)
(spawn-horizontal-motion (/ (* 1.0 DX-PER-SEC) FRAMES-PER-SEC))
(spawn-vertical-motion (/ (* 1.0 GRAVITY-PER-SEC) FRAMES-PER-SEC) JUMP-V MAX-V)
(spawn-clock (/ 1000 FRAMES-PER-SEC))

;; nat nat nat nat (nat symbol -> (U #f (constreeof message))) -> spawn
(define (make-enemy x0 y0 w h mover)
  (define id (gensym 'enemy))
  (spawn
   (lambda (e n)
     (match e
       [(message (kill-enemy (== id)))
        (quit '())]
       [(message (timer-tick))
        (define maybe-messages (mover n id))
        (transition (add1 n)
                    (if maybe-messages
                        maybe-messages
                        '()))]
       [(message (defeat))
        (printf "aaaaarghghagh!\n")
        (quit '())]
       [_ #f]))
   0
   (sub (timer-tick))
   (sub (kill-enemy id))
   (sub (defeat))
   (assert (enemy id (rect (posn x0 y0) w h)))))

;; spawn an enemy that travels from (x0, y0) to (x0 + x-dist, y0) then back to
;; (x0, y0) at a rate of dx per clock tick
(define (make-horiz-enemy x0 y0 w h x-dist dx)
  (define THRESHOLD (/ x-dist dx))
  (make-enemy x0 y0 w h
              (lambda (n id)
                (list (message (move-x id (if (< (modulo n (floor (* 2 THRESHOLD))) THRESHOLD)
                                              dx
                                              (- dx))))))))


(define level0
  (level PLAYER0
         (list (rect (posn 0 200) 150 10)
               (rect (posn 400 200) 1000 10)
               (rect (posn 200 178) 50 10)
               (rect (posn 300 150) 50 10))
         GOAL0
         (list (make-horiz-enemy 0 180 20 20 130 2)
               (make-horiz-enemy 200 158 20 20 30 1)
               (make-horiz-enemy 300 130 20 20 30 1)
               (make-horiz-enemy 400 180 20 20 180 3))))

(spawn-enemy-spawner)
(spawn-game-logic PLAYER0 GOAL0)
(spawn-level-manager (list level0))


#;(let ([x0 275]
        [y0 50]
        [my-w 10]
        [my-h 50])
    (spawn
     (lambda (e s)
       (match-define (cons n (posn old-x old-y)) s)
       (match e
         [(message (timer-tick))
          (define new-y (+ y0 (+ 50 (abs (- (modulo n 100) 50)))))
          (transition (cons (add1 n) (posn old-x new-y))
                      (patch-seq (retract (static ?))
                                 (assert (static (rect (posn old-x new-y) my-w my-h)))))]
         [_ #f]))
     (cons 0 (posn x0 y0))
     (sub (timer-tick))
     (assert (static (rect (posn x0 y0) my-w my-h)))))

