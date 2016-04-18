#lang syndicate

(require racket/gui)
(require syndicate/drivers/timer)

(define my-canvas%
  (class canvas%
    (init-field key-handler)
    (define/override (on-char event)
      (define key-code (send event get-key-code))
      (if (char? key-code)
          (key-handler (string key-code))
          #f))
    (super-new)))

;; protocol description
;; "World" Processes:
;; - Each world process Pi listens for keyboard messages of the form (key-event Pi k)
;; - On receiving such a message it will retract all current assertions and assert (key-press Pi k)
;; - On receiving an added patch of the form (display k) outputs the string k
;; "Universe" Process
;; - On receiving added patches of the form (key-press P k) it will pick one and assert (display k)
;;   as well as retracting all previous assertions

(define (spawn-world label msg-out)
  (define display-detector (compile-projection `(display ,(?!))))
  (spawn
   (lambda (e s)
     (match e
       [(message (at-meta `(key-event ,(== label) ,k)))
        (transition s (patch-seq (retract `(key-press ,label ,?))
                                 (assert `(key-press ,label ,k))))]
       [(? patch/added? p)
        (match-define (cons k _) (set->list (matcher-project/set/single (patch-added p) display-detector)))
        (msg-out k)
        #f]
       [_ #f]))
   #f
   ;; listen for key event messages from this canvas
   (list (sub `(key-event ,label ,?) #:meta-level 1)
         ;; listen for display messages from universe
         (sub `(display ,?)))))

(define (spawn-universe)
  (define key-press-detector (compile-projection `(key-press ,? ,(?!))))
  (spawn
   (lambda (e s)
     (match e
       [(? patch/added? p)
        (match-define (cons k _) (set->list (matcher-project/set/single (patch-added p) key-press-detector)))
        (transition s (patch-seq (retract `(display ,?))
                                 (assert `(display ,k))))]
       [_ #f]))
   (void)
   (sub `(key-press ,? ,?))))

(define (make-frame)
  (parameterize ((current-eventspace (make-eventspace)))
    (define lbl (symbol->string (gensym "process")))    
    (define frame (new frame%
                       [label lbl]
                       [width 300]
                       [height 200]))
    (define msg (new message%
                     [parent frame]
                     [label " "]
                     [auto-resize #t]))
    (define canvas (new my-canvas%
                        [parent frame]
                        [key-handler (lambda (key)
                                       (printf "key press: ~v ~v\n" lbl key)
                                       (send-ground-message `(key-event ,lbl ,key)))]))
    (send frame show #t)
    (spawn-world lbl (lambda (k) (send msg set-label k)))))

(define (frame-spawner)
  (spawn
   (lambda (e s)
     (match e
       [(message (at-meta 'spawn-frame)) (transition s (list (make-frame)))]
       [_ #f]))
   (void)
   (sub 'spawn-frame #:meta-level 1)))

(spawn-universe)
(make-frame)
(make-frame)
(make-frame)
(make-frame)
(make-frame)
(frame-spawner)

(define (main frames)
  (for ([i (in-range frames)])
    (printf "~v\n" i)))

(main 5)


(define t
  (parameterize ((current-eventspace (make-eventspace)))
    (new timer%
         [notify-callback (lambda ()
                            (printf "\n\ntimer!\n\n")
                            (send-ground-message 'spawn-frame))]
         [interval 10000]
         [just-once? #f])))

#;(define (after msec thunk)
    (define id (gensym 'after))
    (if (zero? msec)
        (thunk)
        (list
         (spawn (lambda (e s) (and (message? e) (quit (thunk))))
                (void)
                (sub (timer-expired id ?)))
         (message (set-timer id msec 'relative)))))

#;(spawn-timer-driver)
#;(after 1000 (lambda () (printf "\n\ntimer!\n\n")))
