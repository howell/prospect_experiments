#lang prospect

;; every shape process registers with the world by asserting ('register label shape)
;; a shape can move by sending the message ('move label dx dy)
;; the game logic process listens for movements and asserts the actual locations
;; a rendering process listens for the actual locations and draws the map accordingly

(require "./periodic_timer.rkt")

