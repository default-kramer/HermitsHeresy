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

(require pict
         (only-in racket/draw make-color))

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

(define (generate-plan2 need-length)
  (let loop ([len 0])
    (cond
      [(>= len need-length) (list)]
      [else
       (let* ([w (random 3 7)]
              [o (random 2 5)]
              [d (random 1 3)])
         (list* (body w)
                (linkage o d)
                (loop (+ len w o))))])))

(struct slice (width tall short padding) #:transparent)

(define (plan->slices plan)
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
  ; TODO a value of at least 2 is needed to guarantee that
  ; no tall/short/padding values are negative.
  ; But 5 is an arbitrary value to make the pict look reasonable...
  ; Where should this value really be introduces?
  (define TOTAL_DEPTH (+ 12 (- max-depth min-depth)))
  (define (ADJUST_DEPTH d)
    (- TOTAL_DEPTH d))
  (define (get-padding running-depth)
    (- running-depth min-depth))
  (define (recurse plan running-depth tall?)
    (match plan
      [(list (body width) more ...)
       (let* ([depth (ADJUST_DEPTH running-depth)]
              [padding (get-padding running-depth)])
         (cons (slice width
                      (if tall? depth 0)
                      (if tall? 0 depth)
                      padding)
               (recurse more running-depth tall?)))]
      [(list (linkage overlap depth) more ...)
       #:when tall?
       (let* ([running-depth (+ running-depth depth)]
              [padding (get-padding running-depth)]
              [FOO 2] ; TODO hard-coded "overlap depth"
              [short FOO]
              [tall (ADJUST_DEPTH (+ short running-depth))])
         (cons (slice overlap tall short padding)
               (recurse more running-depth (not tall?))))]
      [(list (linkage overlap depth) more ...)
       #:when (not tall?)
       (let* ([padding (get-padding running-depth)]
              [FOO (+ 1 (abs depth))] ; hard coded "overlap depth"
              [short FOO]
              [tall (ADJUST_DEPTH (+ short running-depth))]
              [running-depth (+ running-depth depth)])
         (cons (slice overlap tall short padding)
               (recurse more running-depth (not tall?))))]
      [(list) (list)]))
  (recurse plan 0 #t))


(struct backstop-run (width depth) #:transparent)


(define (slices->backstop slices)
  (define MIN 4) ; backstop must be at least this deep (relative to padding)
  (define TARGET 5) ; backstop should be, on average, this deep (relative to padding)
  (define MAX 7) ; not a true max, but beyond this we always step towards the target
  (define (step slices current-depth leftover-width)
    (define-syntax-rule (retry body)
      ; If we reach a limit, we just try again hoping for luckier RNG.
      (or body body body))
    (match slices
      [(list) (list)]
      [(list the-slice more-slices ...)
       (let* ([anchor (slice-padding the-slice)]
              [lo (+ anchor MIN)]
              [target (+ anchor TARGET)]
              [hi (+ anchor MAX)]
              [remain-w (- leftover-width (slice-width the-slice))])
         (cond
           [(< current-depth lo)
            ; Unacceptable state reached! Hope backtracking works.
            #f]
           [(>= remain-w 0)
            ; We have enough width to cover the current slice.
            ; Consume that slice and recurse, preserving any leftover width.
            (retry (step more-slices current-depth remain-w))]
           [else
            ; We don't have enough width to cover the current slice.
            ; Spend all the width we have towards "thinning" that slice.
            ; Generate a new backstop run and recurse, treating the entire
            ; width of the new run as leftover width.
            (let* ([thinned-slice (struct-copy slice the-slice
                                               [width (- remain-w)])]
                   [run-w (case (random 10)
                            [(0 1) 2]
                            [(2 3 4) 3]
                            [(5 6 7 8) 4]
                            [(9) 5])]
                   [step-dir (if (< (random lo hi) current-depth)
                                 -1
                                 1)]
                   [run-d (+ current-depth step-dir)]
                   [results (retry (step (cons thinned-slice more-slices)
                                         run-d
                                         run-w))])
              (and results
                   (cons (backstop-run run-w run-d)
                         results)))]))]))
  (let ([start (+ TARGET (slice-padding (first slices)))])
    (or (for/or ([i (in-range 10)])
          (step slices start 0))
        ; For more robustness, we should probably loosen the constraints each time...
        ; But this is good enough for now.
        (error "failed to generate backstop"))))

#;(---
   plan     : (listof (or/c body? linkage?))
   slices   : (listof slice?)
   backstop : (listof backstop-run?))
(struct wall (plan slices backstop) #:transparent)

(define (plan->wall plan)
  (let* ([slices (plan->slices plan)]
         [backstop (slices->backstop slices)])
    (wall plan slices backstop)))

(define (slice->pict slice)
  (define w (slice-width slice))
  (define tall (filled-rectangle w (slice-tall slice)
                                 #:color "blue"
                                 #:draw-border? #f))
  (define short (filled-rectangle w (slice-short slice)
                                  #:color "orange"
                                  #:draw-border? #f))
  (define pad (blank w (slice-padding slice)))
  (vc-append tall short pad))

(define (wall->pict wall)
  (define slices (wall-slices wall))
  (define H (match slices
              [(list (slice _ tall short padding) more ...)
               (+ tall short padding)]))
  (define (backstop-run->pict run)
    (define w (backstop-run-width run))
    (define x (backstop-run-depth run))
    (ct-superimpose (filled-rectangle w (max 0 (- H x))
                                      #:color (make-color 0 0 0 0.6)
                                      #:draw-border? #f)
                    (blank w H)))
  (define slice-pict
    (apply hc-append (map slice->pict slices)))
  (define backstop-pict
    (apply hc-append (map backstop-run->pict (wall-backstop wall))))
  (lc-superimpose slice-pict backstop-pict))


(let* ([plan (generate-plan 10)]
       [wall (plan->wall plan)])
  (values plan
          (wall-slices wall)
          (wall-backstop wall)
          (scale (wall->pict wall) 10)))


(define (p2 I)
  (let* ([plan (list (body 3)
                     (linkage 4 1)
                     (body 6)
                     (linkage 3 -2)
                     (body 4)
                     (linkage 3 I)
                     (body 4)
                     (linkage 2 -1)
                     (body 5)
                     )])
    (plan->wall plan)))
(scale (wall->pict (p2 1)) 10)
(scale (wall->pict (p2 -1)) 10)



(define (TODO w h)
  (define (negate plan limit)
    (let loop ([plan plan]
               [limit limit])
      (cond
        [(<= limit 0)
         plan]
        [else
         (match plan
           [(list (linkage o d) more ...)
            (cons (linkage o (- d))
                  (loop more (- limit o)))]
           [(list (body width) more ...)
            (cons (body width)
                  (loop more (- limit width)))]
           [(list) (list)]
           [else
            (error "assert fail:" plan)])])))
  (define (gen-wall len)
    (let ([plan (negate (generate-plan2 len) (/ len 2))])
      (plan->wall plan)))
  (define (picture wall)
    (let* ([p (wall->pict wall)]
           [w (pict-width p)])
      (inset/clip p -10 0)))
  (let* ([side-w1 (gen-wall w)]
         [side-w2 (gen-wall w)]
         [side-h1 (gen-wall h)]
         [side-h2 (gen-wall h)]
         [pict-w1 (picture side-w1)]
         [pict-w2 (picture side-w2)]
         [pict-h1 (picture side-h1)]
         [pict-h2 (picture side-h2)]
         [more-w (max w (pict-width pict-w1) (pict-width pict-w2))]
         [more-h (max h (pict-height pict-h1) (pict-height pict-h2))]
         [pic (blank more-w more-h)]
         [pic (lc-superimpose pic (rotate pict-h1 (* pi 3/2)))]
         [pic (rc-superimpose pic (rotate pict-h2 (* pi 1/2)))]
         [pic (ct-superimpose pic (rotate pict-w1 (* pi 1)))]
         [pic (cb-superimpose pic pict-w2)]
         )
    pic))

(scale (TODO 90 60) 5)

(TODO 80 60)
