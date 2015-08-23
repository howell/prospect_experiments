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

(define (dot-behavior keys)
  (match-define (arrow-keys up left down right) keys)
  (lambda (e s)
    (match-define (dot-state me others) s)
    (match e
      [(patch added removed)
       ;; update the position of all shapes
       (define vacated (matcher-project/set removed shape-detector))
       (define moved (matcher-project/set added shape-detector))
       (transition (set-remove (set-union (set-subtract others vacated) moved) me) '())]
      [_ #f])))

(define (spawn-dot shape keys)
  (spawn
   (dot-behavior keys)
   (dot-state shape (set))
   (assert `(shape ,shape))
   (sub `(shape ,?))
   (sub `(key-event ,?) #:meta-level 1)))