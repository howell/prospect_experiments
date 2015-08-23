#lang prospect

(require racket/set)

;; protocol description - Take 2 (process for each dot)
;; key board events are injected into the system using messages of the form ('key-event k)
;; each dot process P asserts its current location as ('shape shape) and listens for the location of every other thing
;; when
;; a drawing process D listens for every location and draws/erases to the screen as postions are asserted and retracted

(struct posn (x y) #:transparent)
;; kind of assumed to be rectangular
(struct shape (top-left x-size y-size) #:transparent)
(struct dot-state (my-shape everyone-else) #:transparent)

(define shape-detector
  (compile-projection `(shape ,(?!))))

(define (dot-behavior e s)
  (match-define (dot-state me others) s)
  (match e
    [(patch added removed)
     ;; update the position of all shapes
     (define vacated (matcher-project/set removed shape-detector))
     (define moved (matcher-project/set added shape-detector))
     (transition (set-union (set-subtract others vacated) moved) '())]
    [_ #f]))