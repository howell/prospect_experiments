#lang prospect

(spawn
 (lambda (e s) #f)
 (void)
 (message 0))

#|
Spawned process in world () died with exception:
match-define: no matching clause for '#s(message 0)
  context...:
   /Applications/Racket v6.2.1/collects/racket/match/runtime.rkt:22:0: match:error
   /Users/scaldwell/git/prospect/prospect/patch.rkt:200:0: compose-patch
   /Users/scaldwell/git/prospect/prospect/core.rkt:300:0
   /Users/scaldwell/git/prospect/prospect/core.rkt:259:0: transition-bind
   /Users/scaldwell/git/prospect/prospect/core.rkt:292:2: for-loop
   /Users/scaldwell/git/prospect/prospect/core.rkt:259:0: transition-bind
   /Applications/Racket v6.2.1/collects/racket/private/list.rkt:229:4: foldl
   /Users/scaldwell/git/prospect/prospect/ground.rkt:71:2: await-interrupt
   /Users/scaldwell/git/prospect_experiments/message_inital_action.rkt: [running body]

ground is being polled for changes.
hash-ref: no value found for key
  key: 0
  context...:
   /Users/scaldwell/git/prospect/prospect/trace/stderr.rkt:95:4: loop
|#