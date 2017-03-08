#lang racket

(require syndicate
         syndicate/drivers/timer)

(provide periodically)

;; nat (thunk (U action (listof action)) -> (listof action)
(define (periodically period-ms thunk)
  (define id (gensym 'after))
  (define begin-time (current-inexact-milliseconds))
  (define (set-timer-message n)
    (message (set-timer id (+ begin-time (* n period-ms)) 'absolute)))
  (actor
         (lambda (e n)
           (and (message? e)
                (let* ([msg (set-timer-message n)]
                       [x (thunk)]
                       [actions
                        (cond
                          [(action? x) (list x msg)]
                          [(and (list? x) (andmap action? x))
                           (cons msg x)]
                          [else (list msg)])])
                  (transition (add1 n) actions))))
         1
         (list (sub (timer-expired id ?))
               (set-timer-message 0))))
