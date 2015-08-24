#lang prospect

(spawn
 (lambda (e s)
   #f)
 (void)
 (assert 'visible)
 #;(message 'invisible))



(spawn (lambda (e s)
         (match e
           [(? patch/added? p)
            (define matches (matcher-project/set (patch-added p) (compile-projection (?!))))
            (printf "\n\nadded: ~v\n\n" matches)
            (transition s (message 'invisible))]
           [(message k)
            (printf "\n\nmessage ~v\n\n" k)
            #f]
           [_ #f]))
       (void)
       (sub 'visible)
       (sub 'invisible))

(send-ground-message 'invisible)