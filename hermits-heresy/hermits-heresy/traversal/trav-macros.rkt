#lang racket

(provide with-selection
         in-platform-hills?!
         )

(require "untyped-traversal.rkt"
         "../selection.rkt"
         "../platform-hills.rkt"
         "../chunky-area.rkt"
         "../basics.rkt")

(define-syntax-rule (with-selection [id sel] body ...)
  (HHEXPR
   (and (in-area? (selection-dst-area sel))
        (let* ([p (make-point (xz XXX ZZZ) YYY)]
               [id (selection-ref sel p)])
          #;(when (not id)
              ; TODO but is this check actually beneficial?
              (error "assert fail: in-area? should have handled this"))
          (and id
               (simple? id)
               (begin body ...))))))

(define-syntax-rule (in-phill?! phill)
  (HHEXPR
   (and (in-area? (phill-area phill))
        (<= YYY (lift (phill-y phill)))
        (begin (set-chisel! 'none)
               (set-block! (lift (phill-block phill)))
               (when (= YYY (lift (phill-y phill)))
                 (set-chisel! (phill-chisel phill)))
               #t))))

(define-syntax-rule (in-platform-hills?! phills)
  (HHEXPR
   (cond
     [(in-phill?! (platform-hills-peak phills)) #t]
     [(in-phill?! (platform-hills-peak-border phills)) #t]
     [(in-phill?! (platform-hills-tall phills)) #t]
     [(in-phill?! (platform-hills-tall-border phills)) #t]
     [(in-phill?! (platform-hills-short phills)) #t]
     [(in-phill?! (platform-hills-short-border phills)) #t]
     [#t #f])))
