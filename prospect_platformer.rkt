#lang syndicate

;; Events
;; keyboard events are injected into the system at the ground level as either
;; (key-press key) or (key-release key) messages
;; 
;; processes in the system:
;; Level Manager Process:
;;   Manage transitions between levels, which are one of:
;;     - no levels -> first-level, when the game starts
;;     - current-level -> current-level, when the player dies
;;       (a (defeat) message)
;;     - current-level -> next-level, when the player reaches the goal (a
;;       (level-complete) message))
;;     - last-level -> end-screen, when the player beats the game
;;   Transitions are accompished by:
;;     - spawning a game logic process
;;     - asserting the environment as (static rect)
;;     - asserts the initial location of the player as (player rect)
;;     - spawning any enemies.
;;   Similarly, when the player dies (a (defeat) message) retract
;;      the current level and then spawn it again
;; Game Logic Process:
;;   Decides where the player and enemies are on the map and when the level is
;;     over.
;;   Processes (jump-request) messages. If the player is allowed to jump sends a
;;     (jump) message.
;;   Listens for (move-x id dx) and (move-y id dy) messages and attempts to move
;;     the player (id = 'player) or enemy accordingly.
;;   When a (move-y id _) command results in a collision with the environment a
;;     (y-collision id) message is sent.
;;   Sends the entire (game-state ...) as a message every time it updates.
;;   The environment is determined by assertions of the shape (static rect).
;;   The existence of enemies and their initial location is determined by
;;     assertions of the shape (enemy id rect).
;;   If the player kills an enemy sends a (kill-enemy id) message.
;;   When the player reaches the goal, quits and sends the message
;;     (level-complete)
;;   When the player loses (leaves the map/killed by an enemy), quits and sends
;;     the message (defeat)
;; Timer Process:
;;   Sends a (timer-tick) message at a fixed rate
;; Player Process:
;;   Translates keyboard event messages into movement commands.
;;   asserts (move-left) or (move-right) while the left or right arrow key is
;;     held down
;;   sends a (jump-request) message when space is pressed
;; Horizontal Motion Process:
;;   Interprets the output of the Player Process into commands for the Game
;;     Logic Process.
;;   Sends the messsage (move-x 'player +-dx) on every (timer-tick) while
;;     (move-left) or (move-right) is being asserted.
;; Vertical Motion Process:
;;   Represents gravity and the player's attempts to fight gravity by jumping.
;;   Sends (move-y 'player dy) every (timer-tick).
;;   Interprets (jump) messages into upward motion.
;;   When a (y-collision 'player) is detected reset velocity to 0
;; Enemy Process(es):
;;   Asserts the initial position and id as (enemy id (rect posn0 w h)).
;;   Moves by sending (move-x id dx) and (move-y id dy) messages.
;;   Quits when a (kill-enemy id), (defeat), or (level-complete) message is
;;     received.
;; Rendering Process:
;;   Tracks and draws the state of the game:
;;   - (game-state ...) messages
;;   - (victory)/(defeat) assertions
;;   Redraws every timer tick.

(require "./periodic_timer.rkt"
         "../platformer-lib/platform_lib.rkt"
         rackunit
         syndicate/drivers/timer
         syndicate/actor
         racket/set
         (except-in racket/gui field)
         racket/draw
         racket/block)

;; velocity and acceleration
(struct motion (v a) #:transparent)

;; int
(struct move-x (id dx) #:transparent)

;; int
(struct move-y (id dy clock) #:transparent)

(struct move-left () #:transparent)
(struct move-right () #:transparent)

(struct jump-request () #:transparent)
(struct jump () #:transparent)

;; any
(struct y-collision (id clock) #:transparent)

;; rect
(struct player (rect) #:transparent)

;; any * rect
(struct enemy (id rect) #:transparent)

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

;; rect * (listof rect) * rect * (hashof symbol -> enemy) * posn
(struct game-state (player env goal enemies level-size) #:transparent)

;; rect * (listof rect) * rect * (-> (listof spawn)) * posn
(struct level (player0 env0 goal enemies-thunk size) #:transparent)

(define (key-state key action)
  (spawn #:name (format "key-state:~a" key)
         (field [state #f])
         (assert #:when (state) action)
         (on (message (inbound (key-press key)))
             (state #t))
         (on (message (inbound (key-release key)))
             (state #f))))

;; Player Behavior
;; translate key presses into commands
;; asserts (move-left)/(move-right) while the left/right arrow key is held down
;; space becomes a (jump-request) message
(define (spawn-player)
  (list
   (key-state 'left (move-left))
   (key-state 'right (move-right))
   (spawn #:name 'player
          (on (message (inbound (key-press #\space)))
              (send! (jump-request))))))

;; the horizontal motion behavior tries to move the player along the x-axis
;; by sending the messsage (move-x +-dx) each timer tick while (move-left) or
;; (move-right) is being asserted
(define (spawn-horizontal-motion dx)
  (spawn #:name 'horizontal-motion
         ;; use during here?
         (on (asserted (move-left))
             (until (retracted (move-left))
                    (on (message (timer-tick))
                        (send! (move-x 'player (- dx))))))
         (on (asserted (move-right))
             (until (retracted (move-right))
                    (on (message (timer-tick))
                        (send! (move-x 'player dx)))))))

;; the vertical motion behavior tries to move the player downward by sending
;; (move-y dy).
;; this happens periodically when the timer sends a (timer-tick) message.
;; when a (jump) message is received, temporarily move the player upward.
;; when a (y-collision) is detected reset velocity to 0.
(define (spawn-vertical-motion gravity jump-v max-v)
  (spawn #:name 'vertical-motion
         (field [mot (motion 0 gravity)]
                [clock 0])
         (on (message (jump))
             (mot (motion jump-v (motion-a (mot))))
             (clock (add1 (clock))))
         (on (message (timer-tick))
             (send! (move-y 'player (motion-v (mot)) (clock)))
             (mot (motion (min max-v
                               (+ (motion-v (mot)) (motion-a (mot))))
                          (motion-a (mot)))))
         (on (message (y-collision 'player (clock)))
             (mot (motion 0 (motion-a (mot)))))))

;; create a clock that sends (timer-tick) every period-ms
(define (spawn-clock period-ms)
  (define id (gensym 'after))
  (define begin-time (current-inexact-milliseconds))
  (define (set-timer-message! n)
    (send! (set-timer id (+ begin-time (* n period-ms)) 'absolute)))
  (spawn* #:name 'clock
   (set-timer-message! 0) ; does this create a race?
   (until (message (victory)) (field [n 0])
          (on (message (timer-expired id _))
              (send! (timer-tick))
              (set-timer-message! (add1 (n)))
              (n (add1 (n)))))))
          
#;(define (spawn-clock period-ms)
  (periodically period-ms (lambda () (message (timer-tick)))))

;; (hashof Key Any) (listof Key) -> (hashof Key Any)
;; remove a bunch of keys from a hash
(define (hash-remove* h keys)
  (for/fold ([acc h])
            ([k keys])
    (hash-remove acc k)))

;; (hashof symbol enemy) (listof enemy) -> (hashof symbol enemy)
;; remove a bunch of enemies from a hash
(define (hash-remove-enemies h enemies)
  (hash-remove* h (map (lambda (e) (enemy-id e)) enemies)))

;; rect goal -> spawn
;; The game logic process keeps track of the location of the player and the
;; environment.
;; processes move-x and move-y commands from the player and enemies. When a
;; collision along the y-axis occurs it sends a (y-collision id) message with
;; the id of the moving object.
;; Sends the entire state of the game as a message - (message (game-state ...))
;; - every time it changes.
;; If the player is moving down/enemy is moving up and they collide, send a
;; (kill-enemy id) message. Otherwise if the player and the enemy collide the
;; game is over.
;; Quits and messages (level-complete) if the player reaches the goal
;; Quits and messages (defeat) if the player leaves the map
(define (spawn-game-logic player0 goal0 level-size)
  (define gs0 (game-state player0 '() goal0 (hash) level-size))
  (spawn* #:name 'game-logic
   (react/suspend (return!)
                  (field [gs gs0])
     (on (message (move-x 'player $dx))
         (gs (player-motion-x (gs) dx return!)))
     (on (message (move-y 'player $dy $clock))
         (gs (player-motion-y (gs) dy clock return!)))
     (on (message (move-x $id $dx))
         (gs (enemy-motion-x (gs) id dx return!)))
     (on (message (move-y $id $dy $clock))
         (gs (enemy-motion-y (gs) id dy clock return!)))
     (on (message (jump-request))
         (when (cdr (move-player-y (game-state-player (gs))
                                   1
                                   (game-state-env (gs))))
           (send! (jump))))
     (on (asserted (static $r))
         (define new-env (cons r (game-state-env (gs))))
         (define next-state (struct-copy game-state (gs) [env new-env]))
         (send! next-state)
         (gs next-state))
     (on (retracted (static $r))
         (define new-env (remove r (game-state-env (gs))))
         (define next-state (struct-copy game-state (gs) [env new-env]))
         (send! next-state)
         (gs next-state))
     (on (asserted (enemy $id $r))
         (define old-enemies (game-state-enemies (gs)))
         (define new-enemies (hash-set old-enemies id (enemy id r)))
         (define next-state
           (struct-copy game-state (gs) [enemies new-enemies]))
         (send! next-state)
         (gs next-state))
     (on (retracted (enemy $id $r))
         (define old-enemies (game-state-enemies (gs)))
         (define new-enemies (hash-remove old-enemies id))
         (define next-state
           (struct-copy game-state (gs) [enemies new-enemies]))
         (send! next-state)
         (gs next-state)))))

;; game-state num return! -> (U return! GameState)
;; move the player along the x-axis
(define (player-motion-x gs dx return!)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (match-define (posn x-limit y-limit) lsize)
  (define level-rect (rect (posn 0 0) x-limit y-limit))
  (define player-n (car (move-player-x player-old dx env-old)))
  (cond
    [(overlapping-rects? player-n cur-goal)
     (send! (level-complete))
     (return!)]
    [(not (overlapping-rects? player-n level-rect))
     (send! (defeat))
     (return!)]
    [(hit-enemy? enemies-old player-n)
     (send! (defeat))
     (return!)]
    [else
     (define next-state
       (game-state player-n env-old cur-goal enemies-old lsize))
     (send! next-state)
     next-state]))

;; game-state num return! -> (U return! GameState)
;; move the player along the y-axis
(define (player-motion-y gs dy clock return!)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (match-define (posn x-limit y-limit) lsize)
  (define level-rect (rect (posn 0 0) x-limit y-limit))
  (match-define (cons player-n col?) (move-player-y player-old dy env-old))
  (define col-enemies
    (for/list ([e (hash-values enemies-old)]
               #:when (overlapping-rects? player-n (enemy-rect e)))
      e))
  (define enemies-new (hash-remove-enemies enemies-old col-enemies))
  (define kill-messages (for/list [(e col-enemies)]
                          (message (kill-enemy (enemy-id e)))))
  (cond
    [(overlapping-rects? player-n cur-goal)
     (send! (level-complete))
     (return!)]
    [(not (overlapping-rects? player-n level-rect))
     (send! (defeat))
     (return!)]
    [(and (not (empty? col-enemies)) (negative? dy))
     (send! (defeat))
     (return!)]
    [else
     (define next-state
       (game-state player-n env-old cur-goal enemies-new lsize))
     (for ([m (in-list kill-messages)]) (send! (message-body m)))
     (when col? (send! (y-collision 'player clock)))
     (send! next-state)
     next-state]))

;; game-state symbol num return! -> (U return! GameState)
;; move an enemy along the x-axis
(define (enemy-motion-x gs id dx return!)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (define maybe-enemy (hash-ref enemies-old id #f))
  ;; the enemy might not be in the hash if it was recently killed
  (cond
    [maybe-enemy
     (match-define (enemy _ e-rect) maybe-enemy)
     (define e-rect-new (car (move-player-x e-rect dx env-old)))
     (define enemies-new (hash-set enemies-old id (enemy id e-rect-new)))
     (define next-state
       (game-state player-old env-old cur-goal enemies-new lsize))
     (cond
       [(overlapping-rects? player-old e-rect-new)
        (send! (defeat))
        (return!)]
       [else
        (send! next-state)
        next-state])]
    [else gs]))

(define (enemy-motion-y gs id dy clock return!)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (define maybe-enemy (hash-ref enemies-old id #f))
  ;; the enemy might not be in the hash if it was recently killed
  (cond
    [maybe-enemy
     (match-define (enemy _ e-rect) maybe-enemy)
     (match-define (cons e-rect-new col?) (move-player-y e-rect dy env-old))
     (define enemies-new (hash-set enemies-old id (enemy id e-rect-new)))
     (define player-collision? (overlapping-rects? player-old e-rect-new))
     (cond
       [(and player-collision? (positive? dy))
        ;; enemy fell on player
        (send! (defeat))
        (return!)]
       [player-collision?
        (define enemies-final (hash-remove enemies-new id))
        (define next-state
          (game-state player-old env-old cur-goal enemies-final lsize))
        (send! (kill-enemy id))
        (send! next-state)
        next-state]
       [else
        (define next-state
          (game-state player-old env-old cur-goal enemies-new lsize))
        (send! next-state)
        (when col? (send! (y-collision id clock)))
        next-state])]
    [else gs]))

;; (hashof symbol -> enemy) rect -> bool
(define (hit-enemy? enemies-old player-n)
  (ormap (lambda (e) (overlapping-rects? player-n (enemy-rect e)))
         (hash-values enemies-old)))


(define (star-points scale)
  (map (lambda (pr) (cons (* scale (car pr)) (* scale (cdr pr))))
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
;; draws the goal as a 50x50 yellow star
(define (draw-goal dc g)
  (match-define (rect (posn x0 y0) _ _) g)
  (send dc set-brush "yellow" 'solid)
  (send dc set-pen "yellow" 1 'solid)
  (send dc set-smoothing 'aligned)
  (send dc draw-polygon (star-points 5) x0 y0))

;; drawing-context rect color -> void
;; draws a solid rectangle
(define (draw-rect dc r color)
  (match-define (rect (posn x0 y0) w h) r)
  (send dc set-brush color 'solid)
  (send dc set-pen color 1 'solid)
  (send dc draw-rectangle x0 y0 w h))

;; drawing-context rect (listof rect) goal (listof enemy) -> void
;; draws the game
(define (draw-game dc player env gl enemies)
  (for ([r env])
    (draw-rect dc r "black"))
  (draw-goal dc gl)
  (for ([e enemies])
    (draw-rect dc (enemy-rect e) "red"))
  (draw-rect dc player "blue"))

;; num num num -> num
;; determine an offset for side-scrolling
(define (scroll-offset player canvas-size level-size)
  (define csize/2 (/ canvas-size 2))
  (cond
    ;; don't scroll when the player is close to the beginning of the level
    [(< (- player csize/2) 0) 0]
    ;; similarly, don't scroll when near the end
    [(> (+ player csize/2) level-size) (- level-size canvas-size)]
    ;; otherwise put the player at the center of the screen
    [else (- player csize/2)]))

(define (render-game canvas-dc player env gl enemies lsize)
  (match-define (posn x-size y-size) canvas-bot-right)
  (match-define (posn player-x player-y) (rect-top-left player))
  (match-define (posn x-limit y-limit) lsize)
  (define src-x (scroll-offset player-x x-size x-limit))
  (define src-y (scroll-offset player-y y-size y-limit))
  (define bitmap (make-object bitmap% x-limit y-limit))
  (define bitmap-dc (send bitmap make-dc))
  (draw-game bitmap-dc player env gl enemies)
  (send canvas-dc suspend-flush)
  (send canvas-dc clear)
  (send canvas-dc draw-bitmap-section bitmap 0 0 src-x src-y x-size y-size)
  (send canvas-dc resume-flush))

(define (big-text dc text color)
  (send dc suspend-flush)
  (send dc clear)
  (send dc set-text-mode 'solid)
  (send dc set-text-foreground color)
  (define fnt (make-object font% 100 'default))
  (send dc set-font fnt)
  (define tl-x (/ (posn-x canvas-bot-right) 6))
  (define tl-y (/ (posn-y canvas-bot-right) 4))
  (send dc draw-text text tl-x tl-y)
  (send dc resume-flush))

(define (draw-victory dc)
  (big-text dc "Victory!" "green"))

(define (draw-defeat dc)
  (big-text dc "Defeat." "red"))

;; DC GameState -> Void
(define (draw-game-state dc gs)
  (match-define (game-state old-player old-env old-goal old-enemies lsize) gs)
  (render-game dc old-player old-env old-goal (hash-values old-enemies) lsize))

;; draw the state of the game - determined by the last (game-state ...) message
;; received - every (timer-tick).
;; if (victory) is detected draw something special.
(define (spawn-renderer dc)
  (define gs0 (game-state (rect (posn 0 0) 0 0)
                          '()
                          (rect (posn -100 -100) 0 0)
                          (hash)
                          (posn 100 100)))
  (spawn* #:name 'renderer
   (until (message (victory)) (field [gs gs0])
          (on (message (game-state $player $env $goal $enemies $level-size))
              (gs (game-state player env goal enemies level-size)))
          (on (message (timer-tick))
              (draw-game-state dc (gs))))
   (draw-victory dc)))

(define (boot-level! l)
  (match-define (level player0 env0 goal0 enemies-thunk lsize) l)
  (assert! (player player0))
  (for ([r (in-list env0)]) (assert! (static r)))
  (enemies-thunk)
  (spawn-game-logic player0 goal0 lsize))

(define (run-level l)
  (match-define (level player0 env0 goal0 enemies-thunk lsize) l)
  (assert (player player0))
  (for ([r (in-list env0)]) (assert (static r)))
  (enemies-thunk)
  (spawn-game-logic player0 goal0 lsize))

(define (quit-level! l)
  (match-define (level player0 env0 goal0 enemies lsize) l)
  (retract! (player ?))
  (retract! (static ?)))

;; (non-empty-listof level) -> spawn
(define (spawn-level-manager levels)
  (spawn #:name 'level-manager
         (run-level (first levels))
         (stop-when (message (defeat))
                    (spawn-level-manager levels))
         (stop-when (message (level-complete))
                    (match (rest levels)
                      ['() (send! (victory))]
                      [more-levels (spawn-level-manager more-levels)]))))
     
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
(define canvas-bot-right #f)

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
    (set! canvas-bot-right (posn x-max y-max))
    (define dc (send canvas get-dc))
    (spawn-renderer dc)))

(define (make-player x0 y0)
  (rect (posn x0 y0) 8 32))

(define (make-goal x0 y0)
  (rect (posn x0 y0) 50 50))

(define PLAYER0 (make-player 0 0))
(define GOAL0 (make-goal 900 150))
(define GOAL1 (make-goal 500 150))

(define FRAMES-PER-SEC 45)

(define GRAVITY-PER-SEC 6)
(define JUMP-V-PER-SEC -200)

(define TERMINAL-VELOCITY-PER-SEC 200)

(define DX-PER-SEC 75)

(make-frame 600 400)
(spawn-timer-driver)
(spawn-player)
(spawn-horizontal-motion (/ DX-PER-SEC FRAMES-PER-SEC))
(spawn-vertical-motion (/ GRAVITY-PER-SEC FRAMES-PER-SEC)
                       (/ JUMP-V-PER-SEC FRAMES-PER-SEC)
                       (/ TERMINAL-VELOCITY-PER-SEC FRAMES-PER-SEC))
(spawn-clock (/ 1000 FRAMES-PER-SEC))

(define (spawn-frame-listener)
  (define begin-time (current-inexact-milliseconds))
  (spawn #:name 'frame-listener
         (field [frame-num 1]
                [last-ms (current-inexact-milliseconds)])
         (on (message (timer-tick))
             (define now (current-inexact-milliseconds))
             (define elapsed-ms (- now begin-time))
             (define elapsed-s (/ elapsed-ms 1000))
             (define ideal-frames-elapsed (* elapsed-s FRAMES-PER-SEC))
             (define missed-frames (- ideal-frames-elapsed (frame-num)))
             (define fps (/ (frame-num) elapsed-s))
             (printf "fps: ~v\n" fps)
             (frame-num (add1 frame-num))
             (last-ms now))))

(define (spawn-sliding-frame-listener window)
  (define begin-time (current-inexact-milliseconds))
  (spawn* #:name 'sliding-frame-listener
     (react (field [frames 0])
            (stop-when (rising-edge (= (frames) window)))
            (on (message (timer-tick))
                (frames (add1 (frames)))))
     (define now (current-inexact-milliseconds))
     (define elapsed (/ (- now begin-time) 1000))
     (define fps (/ window elapsed))
     (printf "fps: ~v\n" fps)
     (spawn-sliding-frame-listener window)))

#;(spawn-frame-listener)
#;(spawn-sliding-frame-listener 30)

;; nat nat nat nat (nat symbol -> Void) -> spawn
(define (make-enemy x0 y0 w h mover)
  (define id (gensym 'enemy))
  (spawn* #:name 'enemy
   (react/suspend (return!)
                  (field (n 0))
     (assert (enemy id (rect (posn x0 y0) w h)))
     (on (message (kill-enemy id))
         (return!))
     (on (message (defeat))
         (return!))
     (on (message (level-complete))
         (return!))
     (on (message (timer-tick))
         (mover (n) id)
         (n (add1 (n)))))))

;; spawn an enemy that travels from (x0, y0) to (x0 + x-dist, y0) then back to
;; (x0, y0) at a rate of dx per clock tick
(define (make-horiz-enemy x0 y0 w h x-dist dx0)
  (define dx (/ (* dx0 24) FRAMES-PER-SEC))
  (define THRESHOLD (/ x-dist dx))
  (make-enemy x0 y0 w h
              (lambda (n id)
                (define right? (< (modulo n (floor (* 2 THRESHOLD))) THRESHOLD))
                (send! (move-x id (if right?
                                      dx
                                      (- dx)))))))

;; spawn an enemy that travels from (x0, y0) to (x0, y0 + y-dist) then back to
;; (x0, y0) at a rate of dy per clock tick
(define (make-vert-enemy x0 y0 w h y-dist dy0)
  (define dy (/ (* dy0 24) FRAMES-PER-SEC))
  (define THRESHOLD (/ y-dist dy))
  (make-enemy x0 y0 w h
              (lambda (n id)
                (define up? (< (modulo n (floor (* 2 THRESHOLD))) THRESHOLD))
                (send! (move-y id
                               (if up?
                                   dy
                                   (- dy))
                               0)))))

(define level0
  (level PLAYER0
         (list (rect (posn 0 200) 150 10)
               (rect (posn 400 200) 1000 10)
               (rect (posn 200 178) 50 10)
               (rect (posn 300 150) 50 10))
         GOAL0
         (thunk
          (list (make-horiz-enemy 0 180 20 20 130 2)
                (make-horiz-enemy 200 158 20 20 30 1)
                (make-horiz-enemy 300 130 20 20 30 1)
                (make-horiz-enemy 400 180 20 20 180 3)))
         (posn 1000 400)))

(define level1
  (level PLAYER0
         (list (rect (posn 0 200) 600 10))
         GOAL1
         (thunk
          (list (make-horiz-enemy 0 180 20 20 580 4)
                (make-horiz-enemy 0 140 20 20 580 8)
                (make-vert-enemy 50 125 20 20 75 4)
                (make-vert-enemy 100 125 20 20 75 4)
                (make-vert-enemy 150 125 20 20 75 4)
                (make-vert-enemy 200 125 20 20 75 4)
                (make-vert-enemy 250 125 20 20 75 4)
                (make-vert-enemy 300 125 20 20 75 4)
                (make-vert-enemy 350 125 20 20 75 4)
                (make-vert-enemy 400 125 20 20 75 4)))
         (posn 600 400)))

;; int int int int int nat nat -> (list rect)
;; make a stair case starting at a given position
(define (ascending-stairs x0 y0 hdist vdist w h n)
  (for/list ([i (in-range n)])
    (define dx (* hdist i))
    (define dy (* vdist i))
    (rect (posn (+ x0 dx) (+ y0 dy)) w h)))

(define level2
  (let ([stairs (ascending-stairs (+ 50 50) (- 800 40)
                                  100 -40
                                  50 10
                                  10)]
        [birdies (thunk
                  (for/list ([i (in-range 5)])
                    (make-vert-enemy (+ 160 (* i 200))
                                     (- 650 (* i 80))
                                     20
                                     20
                                     120
                                     4)))])
    (level (make-player 0 750)
           (flatten (list stairs
                          (rect (posn 0 800) 50 200)))
           (make-goal 1100 950)
           birdies
           (posn 2000 1000))))

(define levels (list level0 level1 level2))

(spawn-level-manager levels)