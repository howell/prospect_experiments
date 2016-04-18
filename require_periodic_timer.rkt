#lang syndicate

(require "./periodic_timer.rkt"
         syndicate/drivers/timer)

(spawn-timer-driver)
(periodically 1000 (lambda () (printf "hi\n")))