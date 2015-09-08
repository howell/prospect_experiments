#lang prospect

(require prospect/drivers/timer)

(define (periodically period-ms thunk)
  (define id (gensym 'after))
  (define set-timer-message (message (set-timer id period-ms 'relative)))
  (list (spawn/stateless (lambda (e)
                           (if (message? e)
                               (let ([x (thunk)])
                                 (cond
                                   [(action? x) (list x set-timer-message)]
                                   [(and (list? x) (andmap action? x)) (cons set-timer-message x)]
                                   [else (list set-timer-message)]))
                               #f))
                         (sub (timer-expired id ?)))
        set-timer-message))

(spawn-timer-driver)

(periodically 1000 (lambda () (printf "hi\n")))