#lang prospect

(require racket/gui)
(require "../prospect/prospect/drivers/timer.rkt")

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
  (define key-detector (compile-projection `(key ,(?!))))
  (spawn
     (lambda (e maybe-old-key)
       (match e
         [(message (at-meta (list l k)))
          (if (equal? l label)
              (transition k
                          (if (and maybe-old-key (not (equal? maybe-old-key k)))
                              ;; was this process previously asserting a key?
                              (list (assert `(key ,k)) (retract `(key ,maybe-old-key)))
                              (list (assert `(key ,k)))))
              #f)]
         [(? patch/added? p)
          (match-define (cons k _) (set->list (matcher-project/set/single (patch-added p) key-detector)))
          (msg-out k)
          #f]
         [_ #f]))
     #f
     ;; listen for key event messages from this canvas
     ;; (sub (at-meta `(,lbl ,?)))
     (sub `(,label ,?) #:meta-level 1)
     ;; listen for key messages from processes
     (sub `(key ,?))))

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

(define (spawner)
  (spawn
   (lambda (e s)
     (match e
       [(message (at-meta 'spawn-frame)) (transition s (list (make-frame)))]
       [_ #f]))
   (void)
   (sub 'spawn-frame #:meta-level 1)))


(make-frame)
(make-frame)
(make-frame)
(make-frame)
(make-frame)
(spawner)

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
