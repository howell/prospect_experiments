#lang prospect

(require "./periodic_timer.rkt"
         prospect/drivers/timer)

(spawn-timer-driver)
(periodically 1000 (lambda () (printf "hi\n")))