#lang prospect

;; Events
;; keyboard events are injected into the system at the ground level as either
;; (key-press key) or (key-release key) messages
;; 
;; processes in the system:
;; Level Manager Process:
;;   Manage transitions between levels, which are one of:
;;     - no levels -> first-level, when the game starts
;;     - current-level -> current-level, when the player dies (a (defeat) message)
;;     - current-level -> next-level, when the player reaches the goal (a (level-complete) message))
;;     - last-level -> end-screen, when the player beats the game
;;   Transitions are accompished by:
;;     - spawning a game logic process
;;     - asserting the environment as (static rect)
;;     - asserts the initial location of the player as (player rect)
;;     - spawning any enemies.
;;   Similarly, when the player dies (a (defeat) message) retract
;;      the current level and then spawn it again
;; Game Logic Process:
;;   Decides where the player and enemies are on the map and when the level is over.
;;   Processes (jump-request) messages. If the player is allowed to jump sends a (jump) message.
;;   Listens for (move-x id dx) and (move-y id dy) messages and attempts to move the player
;;         (id = 'player) or enemy accordingly.
;;   When a (move-y id _) command results in a collision with the environment a (y-collision id)
;;         message is sent.
;;   Sends the entire (game-state ...) as a message every time it updates.
;;   The environment is determined by assertions of the shape (static rect).
;;   The existence of enemies and their initial location is determined by assertions of the shape
;;         (enemy id rect).
;;   If the player kills an enemy sends a (kill-enemy id) message.
;;   When the player reaches the goal, quits and sends the message (level-complete)
;;   When the player loses (leaves the map/killed by an enemy), quits and sends the message (defeat)
;; Timer Process:
;;   Sends a (timer-tick) message at a fixed rate
;; Player Process:
;;   Translates keyboard event messages into movement commands.
;;   asserts (move-left) or (move-right) while the left or right arrow key is held down
;;   sends a (jump-request) message when space is pressed
;; Horizontal Motion Process:
;;   Interprets the output of the Player Process into commands for the Game Logic Process.
;;   Sends the messsage (move-x 'player +-dx) on every (timer-tick) while (move-left) or
;;       (move-right) is being asserted.
;; Vertical Motion Process:
;;   Represents gravity and the player's attempts to fight gravity by jumping.
;;   Sends (move-y 'player dy) every (timer-tick).
;;   Interprets (jump) messages into upward motion.
;;   When a (y-collision 'player) is detected reset velocity to 0
;; Enemy Process(es):
;;   Asserts the initial position and id as (enemy id (rect posn0 w h)).
;;   Moves by sending (move-x id dx) and (move-y id dy) messages.
;;   Quits when a (kill-enemy id), (defeat), or (level-complete) message is received.
;; Rendering Process:
;;   Tracks and draws the state of the game:
;;   - (game-state ...) messages
;;   - (victory)/(defeat) assertions
;;   Redraws every timer tick.
;; Enemy Spawning Process:
;;   Spawns enemy processes on demand.
;;   Listens for (spawn-enemy spawn) and spawns the argument

(require "./geometry.rkt"
         "./periodic_timer.rkt"
         "./platform_lib.rkt"
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

;; rect * (listof rect) * rect * (hashof symbol -> enemy) * posn
(struct game-state (player env goal enemies level-size) #:transparent)

;; rect * (listof rect) * rect * (listof spawn) * posn
(struct level (player0 env0 goal enemies size) #:transparent)

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
(define (spawn-vertical-motion gravity jump-v max-v)
  (struct v-motion-state (jumping? motion jump-ticks clock) #:transparent)
  (spawn
   (lambda (e s)
     (match-define (v-motion-state jumping? motion-old jump-ticks clock) s)
     (match e
       [(message (jump))
        (transition (v-motion-state #t
                                    (motion jump-v (motion-a motion-old))
                                    0
                                    (add1 clock))
                    #f)]
       [(message (timer-tick))
        (define motion-n
          (motion (min max-v (+ (motion-v motion-old) (motion-a motion-old)))
                  (motion-a motion-old)))
        (define jump-ticks-n
          (if jumping? (add1 jump-ticks) jump-ticks))
        (transition (v-motion-state jumping? motion-n jump-ticks-n)
                    (message (move-y 'player (motion-v motion-old) clock)))]
       [(message (y-collision 'player col-clock))
        (when (and jumping? (< jump-ticks 10))
          (error (format "~v" jump-ticks)))
        (and (equal? col-clock clock)
             (transition (v-motion-state #f
                                         (motion 0 (motion-a motion-old))
                                         0
                                         clock) #f))]
       [_ #f]))
   (v-motion-state #f (motion 0 gravity) 0 0)
   (sub (jump))
   (sub (timer-tick))
   (sub (y-collision 'player ?))))

;; create a clock that sends (timer-tick) every period-ms
(define (spawn-clock period-ms)
  (periodically period-ms (lambda () (message (timer-tick)))))

(define static-detector (compile-projection (static (?!))))
;; matcher -> (listof rect)
(define static-rects-matcher
  (lambda (m)
    (set-map (matcher-project/set m static-detector) car)))

(define enemy-detector (compile-projection (enemy (?!) (?!))))
(define (patch-enemies p)
  (define-values (added removed) (patch-project/set p enemy-detector))
  (values (set-map added (lambda (l) (enemy (first l) (second l))))
          (set-map removed (lambda (l) (enemy (first l) (second l))))))

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

;; (hashof symbol enemy) (listof enemy) -> (hashof symbol enemy)
;; add a bunch of enemies to a hash
(define (hash-add-enemies h enemies)
  (for/fold ([acc h])
            ([e enemies])
    (hash-set acc (enemy-id e) e)))

;; (listof enemy) (listof enemy) (hashof symbol -> enemy) -> (hashof symbol -> enemy)
;; update a hash of enemies with additions and subtractions
(define (update-enemy-hash added-enemies removed-enemies h)
  (define h2 (hash-remove-enemies h removed-enemies))
  (hash-add-enemies h2 added-enemies))

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

;; The game logic process keeps track of the location of the player and the environment.
;; processes move-x and move-y commands from the player and enemies. When a collision
;; along the y-axis occurs it sends a (y-collision id) message with the id of the moving
;; object.
;; Sends the entire state of the game as a message - (message (game-state ...)) - every
;; time it changes.
;; If the player is moving down/enemy is moving up and they collide, send a (kill-enemy id)
;; message. Otherwise if the player and the enemy collide the game is over.
;; Quits and messages (level-complete) if the player reaches the goal
;; Quits and messages (defeat) if the player leaves the map
(define (game-logic-behavior e s)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) s)
  (match-define (posn x-limit y-limit) lsize)
  (match e
    [(message (move-x 'player dx))
     (player-motion-x s dx)]
    [(message (move-y 'player dy clock))
     (player-motion-y s dy)]
    [(message (move-x id dx))
     (enemy-motion-x s id dx)]
    [(message (move-y id dy clock))
     (enemy-motion-y s id dy clock)]
    [(message (jump-request))
     ;; check if there is something right beneath the player
     (and (cdr (move-player-y (game-state-player s) 1 (game-state-env s)))
          (transition s (message (jump))))]
    [(patch p-added p-removed)
     (define removed (static-rects-matcher p-removed))
     (define added (static-rects-matcher p-added))
     (define new-env (append added (remove* removed (game-state-env s))))
     (define-values (enemies-added enemies-removed) (patch-enemies e))
     (define enemies-new (update-enemy-hash enemies-added enemies-removed enemies-old))
     (define next-state (game-state player-old new-env cur-goal enemies-new lsize))
     (transition next-state (message next-state))]
    [_ #f]))

;; game-state num -> action*
;; move the player along the x-axis
(define (player-motion-x gs dx)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (match-define (posn x-limit y-limit) lsize)
  (define level-rect (rect (posn 0 0) x-limit y-limit))
  (define player-n (car (move-player-x player-old dx env-old)))
  (cond
    [(overlapping-rects? player-n cur-goal)
     (quit (list (message (level-complete))))]
    [(not (overlapping-rects? player-n level-rect))
     (quit (list (message (defeat))))]
    [(hit-enemy? enemies-old player-n)  
     (quit (list (message (defeat))))]
    [else
     (define next-state (game-state player-n env-old cur-goal enemies-old lsize))
     (transition next-state (message next-state))]))

;; game-state num -> action*
;; move the player along the y-axis
(define (player-motion-y gs dy clock)
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
     (quit (list (message (level-complete))))]
    [(not (overlapping-rects? player-n level-rect))
     (quit (list (message (defeat))))]
    [(and (not (empty? col-enemies)) (negative? dy))
     (quit (message (defeat)))]
    [else
     (define next-state (game-state player-n env-old cur-goal enemies-new lsize))
     (transition next-state (list kill-messages
                                  (message next-state)
                                  (when col?
                                    (message (y-collision 'player clock)))))]))

;; game-state symbol num -> action*
;; move an enemy along the x-axis
(define (enemy-motion-x gs id dx)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (define maybe-enemy (hash-ref enemies-old id #f))
  ;; the enemy might not be in the hash if it was recently killed
  (and maybe-enemy
       (block
        (match-define (enemy _ e-rect) maybe-enemy)
        (define e-rect-new (car (move-player-x e-rect dx env-old)))
        (define enemies-new (hash-set enemies-old id (enemy id e-rect-new)))
        (define next-state (game-state player-old env-old cur-goal enemies-new lsize))
        (cond
          [(overlapping-rects? player-old e-rect-new)
           (quit (list (message (defeat))))]
          [else (transition next-state (message next-state))]))))

(define (enemy-motion-y gs id dy clock)
  (match-define (game-state player-old env-old cur-goal enemies-old lsize) gs)
  (define maybe-enemy (hash-ref enemies-old id #f))
  ;; the enemy might not be in the hash if it was recently killed
  (and maybe-enemy
       (block
        (match-define (enemy _ e-rect) maybe-enemy)
        (match-define (cons e-rect-new col?) (move-player-y e-rect dy env-old))
        (define enemies-new (hash-set enemies-old id (enemy id e-rect-new)))
        (define player-collision? (overlapping-rects? player-old e-rect-new))
        (cond
          [(and player-collision? (positive? dy))
           (quit (list (message (defeat))))] ;; enemy fell on player
          [player-collision?
           (define enemies-final (hash-remove enemies-new id))
           (define next-state (game-state player-old env-old cur-goal enemies-final lsize))
           (transition next-state (list (message (kill-enemy id))
                                        (message next-state)))]
          [else
           (define next-state (game-state player-old env-old cur-goal enemies-new lsize))
           (transition next-state (list (message next-state)
                                        (when col? (message (y-collision id clock)))))]))))

;; (hashof symbol -> enemy) rect -> bool
(define (hit-enemy? enemies-old player-n)
  (ormap (lambda (e) (overlapping-rects? player-n (enemy-rect e))) (hash-values enemies-old)))

;; rect goal -> spawn
(define (spawn-game-logic player0 goal0 level-size)
  (spawn game-logic-behavior
         (game-state player0 '() goal0 (hash) level-size)
         (sub (move-x ? ?))
         (sub (move-y ? ? ?))
         (sub (static ?))
         (sub (jump-request))
         (sub (enemy ? ?))))

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
  (send dc draw-text text (/ (posn-x canvas-bot-right) 6) (/ (posn-y canvas-bot-right) 4))
  (send dc resume-flush))

(define (draw-victory dc)
  (big-text dc "Victory!" "green"))

(define (draw-defeat dc)
  (big-text dc "Defeat." "red"))

;; draw the state of the game - determined by the last (game-state ...) message
;; received - every (timer-tick).
;; if (victory) is detected draw something special.
(define ((render-behavior dc) e s)
  ;; state is a game-state struct
  (match-define (game-state old-player old-env old-goal old-enemies lsize) s)
  (match e
    [(message (? game-state? new-state))
     (transition new-state '())]
    [(message (victory))
     (draw-victory dc)
     (quit '())]
    [(message (timer-tick))
     (render-game dc old-player old-env old-goal (hash-values old-enemies) lsize)
     #f]
    [_ #f]))

(define (spawn-renderer dc)
  (spawn
   (render-behavior dc)
   (game-state (rect (posn 0 0) 0 0) '() (rect (posn -100 -100) 0 0) (hash) (posn 100 100))
   (sub (game-state ? ? ? ? ?))
   (sub (timer-tick))
   (sub (defeat))
   (sub (victory))
   (sub (level-complete))))

;; level -> (constreeof action)
(define (level->actions l)
  (match-define (level player0 env0 goal0 enemies lsize) l)
  (flatten (list (assert (player player0))
                 (map (lambda (r) (assert (static r))) env0)
                 (map (lambda (e) (assert (spawn-enemy e))) enemies))))

;; state is a (non-empty-listof level), the first of which is the current level
;; need a way to kill all enemies
(define (level-manager-behavior e s)
  (match e
    [(message (defeat))
     (match-define (level player0 env0 goal0 enemies size) (car s))
     (transition s (list (spawn-game-logic player0 goal0 size)
                         (retract (static ?))
                         (retract (player ?))
                         (retract (spawn-enemy ?))
                         (apply patch-seq (flatten (level->actions (car s))))))]
    [(message (level-complete))
     (match (cdr s)
       [(cons next-level _)
        (transition (cdr s) (list (spawn-game-logic (level-player0 next-level)
                                                    (level-goal next-level) (level-size next-level))
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

(define FRAMES-PER-SEC 60)

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
  (struct listener-state (frame-num last-ms) #:transparent)
  (spawn
   (lambda (e s)
     (match-define (listener-state frame-num last-ms) s)
     (match e
       [(message (timer-tick))
        (define now (current-inexact-milliseconds))
        #;(define elapsed-ms (- now last-ms))
        (define elapsed-ms (- now begin-time))
        (define elapsed-s (/ elapsed-ms 1000))
        (define ideal-frames-elapsed (* elapsed-s FRAMES-PER-SEC))
        (define missed-frames (- ideal-frames-elapsed frame-num))
        #;(define game-elapsed (* (/ frame-num FRAMES-PER-SEC) 1000.0))
        #;(printf "elapsed ms: ~v\n" elapsed-ms)
        (define fps (/ frame-num elapsed-s))
        (printf "fps: ~v\n" fps)
        #;(printf "ideal: ~v actual: ~v missed: ~v\n"
                  ideal-frames-elapsed
                  frame-num
                  missed-frames)
        (transition (listener-state (add1 frame-num) now) '())]
       [_ #f]))
   (listener-state 1 (current-inexact-milliseconds))
   (sub (timer-tick))))

#;(spawn-frame-listener)

;; nat nat nat nat (nat symbol -> (U #f (constreeof message))) -> spawn
(define (make-enemy x0 y0 w h mover)
  (define id (gensym 'enemy))
  (spawn
   (lambda (e n)
     (match e
       [(message (or (kill-enemy (== id))
                     (defeat)
                     (level-complete)))
        (quit '())]
       [(message (timer-tick))
        (define maybe-messages (mover n id))
        (transition (add1 n)
                    (if maybe-messages
                        maybe-messages
                        '()))]
       [_ #f]))
   0
   (sub (timer-tick))
   (sub (kill-enemy id))
   (sub (defeat))
   (sub (level-complete))
   (assert (enemy id (rect (posn x0 y0) w h)))))

;; spawn an enemy that travels from (x0, y0) to (x0 + x-dist, y0) then back to
;; (x0, y0) at a rate of dx per clock tick
(define (make-horiz-enemy x0 y0 w h x-dist dx0)
  (define dx (/ (* dx0 24) FRAMES-PER-SEC))
  (define THRESHOLD (/ x-dist dx))
  (make-enemy x0 y0 w h
              (lambda (n id)
                (list (message (move-x id (if (< (modulo n (floor (* 2 THRESHOLD))) THRESHOLD)
                                              dx
                                              (- dx))))))))

;; spawn an enemy that travels from (x0, y0) to (x0, y0 + y-dist) then back to
;; (x0, y0) at a rate of dy per clock tick
(define (make-vert-enemy x0 y0 w h y-dist dy0)
  (define dy (/ (* dy0 24) FRAMES-PER-SEC))
  (define THRESHOLD (/ y-dist dy))
  (make-enemy x0 y0 w h
              (lambda (n id)
                (list (message (move-y id (if (< (modulo n (floor (* 2 THRESHOLD))) THRESHOLD)
                                              dy
                                              (- dy))))))))



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
               (make-horiz-enemy 400 180 20 20 180 3))
         (posn 1000 400)))

(define level1
  (level PLAYER0
         (list (rect (posn 0 200) 600 10))
         GOAL1
         (list (make-horiz-enemy 0 180 20 20 580 4)
               (make-horiz-enemy 0 140 20 20 580 8)
               (make-vert-enemy 50 125 20 20 75 4)
               (make-vert-enemy 100 125 20 20 75 4)
               (make-vert-enemy 150 125 20 20 75 4)
               (make-vert-enemy 200 125 20 20 75 4)
               (make-vert-enemy 250 125 20 20 75 4)
               (make-vert-enemy 300 125 20 20 75 4)
               (make-vert-enemy 350 125 20 20 75 4)
               (make-vert-enemy 400 125 20 20 75 4))
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
        [birdies (for/list ([i (in-range 5)])
                   (make-vert-enemy (+ 160 (* i 200)) (- 650 (* i 80)) 20 20 120 4))])
    (level (make-player 0 750)
           (flatten (list stairs
                          (rect (posn 0 800) 50 200)))
           (make-goal 1100 950)
           birdies
           (posn 2000 1000))))

(define levels (list level0 level1 level2))


(spawn-enemy-spawner)
(spawn-game-logic (level-player0 (car levels))
                  (level-goal (car levels))
                  (level-size (car levels)))
(spawn-level-manager levels)


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

