#lang prospect

;; Protocol description - take 3 (collision detection)
;; key board events are injected into the system using messages of the form ('key-event k)
;; - each dot process has a unique label P and asserts its current location as ('shape P shape).
;;   To move from shape to shape', P retracts ('shape P shape) and asserts ('shape P shape').
;; - A collision detection process listens for ('shape ? ?) assertions and maintains a state of
;;   where everything is. If a collision is detected between dots P and Q, the process sends a
;;   message ('move P dx dy) and ('move Q dx dy) to P and Q.
;; - When a dot P receives a move message it updates its assertions to reflect the new location
;; - a drawing process D listens for every location and draws/erases to the screen as postions
;;   are asserted and retracted