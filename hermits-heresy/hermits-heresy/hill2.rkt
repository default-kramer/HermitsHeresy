#lang racket

; Platform-style hills.
; Alternates tall,short,tall,short...
; Constant "drift direction" (plan->pict drifts north as you go from east to west).
;
; IDEA: Notice that the mirror image drifts south as you go east to west.
; So if you can implement cornering, which doesn't seem hard,
; you can generate all 4 sides of a plateau!
; (But... with the RNG involved, how would you ensure the corners meet?
;  Maybe by adjusting the linkage depths?
;  Or just regenerate until you get lucky?)
;
; REMEMBER: The original "rectangulator" idea does not drift.
; Should that be a separate algorithm?
; Or should I try to parameterize this code to allow/disallow drift?
;
; OF COURSE - you can randomly choose to implement the linkage in either
; direction (positive or negative) which will allow running-depth to go
; positive or negative. Set a limit to implement a driftless hill!
; When (abs running-depth) reaches this limit, enforce that the linkage
; steers back towards the center (ignore the RNG for this linkage).

(require pict)

; The body is the part of a platform that does not overlap a neighbor
(struct body (width) #:transparent)

; The linkage defines how to connect 2 neighboring platforms
(struct linkage (overlap depth) #:transparent)

(define (generate-plan count)
  (cond
    [(> count 0)
     (let ([w (random 3 7)]
           [o (random 2 5)]
           [d (random 1 3)])
       (list* (body w)
              (linkage o d)
              (generate-plan (sub1 count))))]
    [else (list)]))

(define (plan->pict plan)
  (define (get-color tall?)
    (if tall? "blue" "orange"))
  (define-values (min-depth max-depth)
    (let loop ([plan plan]
               [running-depth 0]
               [min-depth 0]
               [max-depth 0])
      (match plan
        [(list (linkage o depth) more ...)
         (let* ([running-depth (+ depth running-depth)]
                [min-depth (min running-depth min-depth)]
                [max-depth (max running-depth max-depth)])
           (loop more running-depth min-depth max-depth))]
        [(list x more ...)
         (loop more running-depth min-depth max-depth)]
        [(list)
         (values min-depth max-depth)])))
  (define TOTAL_DEPTH (+ 5 (- max-depth min-depth)))
  (define (ADJUST_DEPTH d)
    (- TOTAL_DEPTH d))
  (define (frect width height tall?)
    (filled-rectangle width height
                      #:color (get-color tall?)
                      #:draw-border? #f))
  (define (make-padding width running-depth)
    (blank width (- running-depth min-depth)))
  (define (recurse plan running-depth tall?)
    (match plan
      [(list (body width) more ...)
       (let* ([padding (make-padding width running-depth)]
              [slab (frect width (ADJUST_DEPTH running-depth) tall?)])
         (hc-append (vc-append slab padding)
                    (recurse more running-depth tall?)))]
      [(list (linkage overlap depth) more ...)
       #:when tall?
       (let* ([running-depth (+ running-depth depth)]
              [padding (make-padding overlap running-depth)]
              [FOO 2] ; hard coded "overlap depth"
              [slab1 (frect overlap FOO (not tall?))]
              [slab2 (frect overlap (ADJUST_DEPTH (+ FOO running-depth)) tall?)])
         (hc-append (vc-append slab2 slab1 padding)
                    (recurse more running-depth (not tall?))))]
      [(list (linkage overlap depth) more ...)
       #:when (not tall?)
       (let* ([padding (make-padding overlap running-depth)]
              [FOO (+ 1 (abs depth))] ; hard coded "overlap depth"
              [slab1 (frect overlap FOO tall?)]
              [slab2 (frect overlap (ADJUST_DEPTH (+ FOO running-depth)) (not tall?))]
              [running-depth (+ running-depth depth)])
         (hc-append (vc-append slab2 slab1 padding)
                    (recurse more running-depth (not tall?))))]
      [(list)
       (blank 0 0)]))
  (recurse plan 0 #t))


(define plan
  (generate-plan 10))
plan
(scale (plan->pict plan) 10)


(define (p2 I) (list (body 3)
                     (linkage 4 1)
                     (body 6)
                     (linkage 3 -2)
                     (body 4)
                     (linkage 3 I)
                     (body 4)
                     (linkage 2 -1)
                     (body 5)
                     ))
(scale (plan->pict (p2 1)) 10)
(scale (plan->pict (p2 -1)) 10)
