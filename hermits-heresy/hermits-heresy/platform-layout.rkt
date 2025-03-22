#lang typed/racket

(provide generate-platform-layout Platform-Layout
         platform-layout-array2d platform-layout-width platform-layout-depth)

(require "ufx.rkt")

(define-syntax-rule (MAIN a ...)
  (void)
  #;(module+ main a ...)
  #;(begin a ...))

{MAIN
 (require typed/pict
          (only-in typed/racket/draw make-color Bitmap%))
 (define-type Pict pict)
 }

; The body is the part of a platform that does not overlap a neighbor.
; Platforms will alternate: tall, short, tall, short...
(struct body ([width : Fixnum]
              [tall? : Boolean])
  #:transparent #:type-name Body)

; The linkage defines how to connect 2 neighboring bodies.
(struct linkage ([overlap : Fixnum]
                 [depth : Fixnum])
  #:transparent #:type-name Linkage)

; More specifically, the Body and Linkage must alternate.
; But this type is good enough.
(define-type Plan (Listof (U Body Linkage)))

(: fxrandom (-> Fixnum Fixnum Fixnum))
(define (fxrandom lo hi)
  (cast (random lo hi) Fixnum))

(: generate-plan (-> Fixnum Boolean Plan))
(define (generate-plan count tall?)
  (cond
    [(> count 0)
     ; Tall platforms look goofy when only 3 wide because 2 of that
     ; width will be used for border, leaving the main platform
     ; having width=1 where it extrudes.
     (let* ([min-width (if tall? 4 3)]
            [w (fxrandom min-width 7)]
            [o (fxrandom 2 5)]
            [d (fxrandom 1 3)])
       (list* (body w tall?)
              (linkage o d)
              (generate-plan (sub1 count) (not tall?))))]
    [else (list)]))

(: generate-plan2 (-> Fixnum Boolean Plan))
(define (generate-plan2 need-length tall?)
  (let loop ([len 0]
             [tall? : Boolean tall?])
    (cond
      [(ufx>= len need-length) (list)]
      [else
       ; Tall platforms look goofy when only 3 wide because 2 of that
       ; width will be used for border, leaving the main platform
       ; having width=1 where it extrudes.
       (let* ([min-width (if tall? 4 3)]
              [w (fxrandom min-width 7)]
              [o (fxrandom 2 5)]
              [d (fxrandom 1 3)])
         (list* (body w tall?)
                (linkage o d)
                (loop (:ufx+ len w o) (not tall?))))])))

(: straighten (-> Plan Fixnum Plan))
; Randomly negates some linkage depths.
; (It is assumed they are all positive and thus "drift" in a constant direction.)
; Will never exceed the original bounding rectangle.
(define (straighten plan max-depth)
  (: loop (-> Plan Fixnum Plan))
  (define (loop plan depth)
    (match plan
      [(list) (list)]
      [(list b more ...)
       #:when (body? b)
       (cons b (loop more depth))]
      [(list (linkage oo dd) more ...)
       (let ([depth-std (ufx+ depth dd)]
             [depth-neg (ufx- depth dd)])
         (define (recurse neg?)
           (if neg?
               (cons (linkage oo (ufx- 0 dd))
                     (loop more depth-neg))
               (cons (linkage oo dd)
                     (loop more depth-std))))
         (cond
           [(< depth-neg 0)
            ; absolutely cannot go negative
            (recurse #f)]
           [(> depth-std max-depth)
            (recurse #t)]
           [else
            (recurse (= 0 (random 2)))]))]
      [else
       (error "assert fail")]))
  (loop plan 0))

; Given a Plan, converts each Body to a Slice and each Linkage to a Slice.
; Also introduces the tall/short alternation, which was implicit in the Plan.
; Here the `tall` and `short` values are the dimensions of just that part
; rather than a total offset.
(struct slice ([width : Fixnum]
               [tall : Fixnum]
               [short : Fixnum]
               [padding : Fixnum])
  #:transparent #:type-name Slice)

(: plan->slices (-> Plan (Listof Slice)))
; This function does not use any RNG.
(define (plan->slices plan)
  (define-values (min-depth max-depth)
    (let loop : (Values Fixnum Fixnum)
      ([plan plan]
       [running-depth : Fixnum 0]
       [min-depth : Fixnum 0]
       [max-depth : Fixnum 0])
      (match plan
        [(list (linkage o depth) more ...)
         (let* ([running-depth (ufx+ depth running-depth)]
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
  (define (ADJUST_DEPTH [d : Fixnum])
    (ufx- TOTAL_DEPTH d))
  (define (get-padding [running-depth : Fixnum])
    (ufx- running-depth min-depth))
  (: recurse (-> Plan Fixnum Boolean (Listof Slice)))
  (define (recurse plan running-depth tall?)
    (match plan
      [(list (body width planned-tall?) more ...)
       (when (not (eq? tall? planned-tall?))
         (error "assert fail? tall/short did not alternate?"))
       (let* ([depth (ADJUST_DEPTH running-depth)]
              [padding (get-padding running-depth)])
         (cons (slice width
                      (if tall? depth 0)
                      (if tall? 0 depth)
                      padding)
               (recurse more running-depth tall?)))]
      [(list (linkage overlap depth) more ...)
       #:when tall?
       (let* ([running-depth (ufx+ running-depth depth)]
              [padding (get-padding running-depth)]
              [FOO 2] ; TODO hard-coded "overlap depth"
              [short FOO]
              [tall (ADJUST_DEPTH (ufx+ short running-depth))])
         (cons (slice overlap tall short padding)
               (recurse more running-depth (not tall?))))]
      [(list (linkage overlap depth) more ...)
       #:when (not tall?)
       (let* ([padding (get-padding running-depth)]
              [FOO (ufx+ 1 (abs depth))] ; hard coded "overlap depth"
              [short FOO]
              [tall (ADJUST_DEPTH (ufx+ short running-depth))]
              [running-depth (ufx+ running-depth depth)])
         (cons (slice overlap tall short padding)
               (recurse more running-depth (not tall?))))]
      [(list) (list)]))
  (recurse plan 0 #t))


; The backstop defines where the platforms end and the peak begins.
(struct backstop-run ([width : Fixnum]
                      [depth : Fixnum])
  #:transparent #:type-name Backstop-Run)


(: generate-backstop (-> (Listof Slice) (Listof Backstop-Run)))
(define (generate-backstop slices)
  (define MIN 4) ; backstop must be at least this deep (relative to padding)
  (define TARGET 5) ; backstop should be, on average, this deep (relative to padding)
  (define MAX 6) ; not a true max, but beyond this we always step towards the target
  (define-syntax-rule (retry (step arg ...))
    (or (step arg ...)
        (step arg ...)
        (step arg ...)
        ; Unlucky RNG, now we have to force the issue:
        (step arg ... #:last-try? #t)))
  (: step (-> (Listof Slice) Fixnum Fixnum [#:last-try? Boolean] (U #f (Listof Backstop-Run))))
  (define (step slices current-depth leftover-width #:last-try? [last-try? #f])
    (match slices
      [(list) (list)]
      [(list the-slice more-slices ...)
       (let* ([anchor (slice-padding the-slice)]
              [lo (ufx+ anchor MIN)]
              [target (ufx+ anchor TARGET)]
              [hi (ufx+ anchor MAX)]
              [remain-w (ufx- leftover-width (slice-width the-slice))])
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
                                               [width (ufx- 0 remain-w)])]
                   [run-w : Fixnum (case (random 10)
                                     [(0 1) 2]
                                     [(2 3 4) 3]
                                     [(5 6 7 8) 4]
                                     [(9) 5]
                                     [else (error "assert fail")])]
                   [step-dir (cond
                               [last-try? 1]
                               [(< (random lo hi) current-depth) -1]
                               [else 1])]
                   [run-d (ufx+ current-depth step-dir)]
                   [results (retry (step (cons thinned-slice more-slices)
                                         run-d
                                         run-w))])
              (and results
                   (cons (backstop-run run-w run-d)
                         results)))]))]))
  (let ([start (ufx+ TARGET (slice-padding (first slices)))])
    (or (retry (step slices start 0))
        (error "failed to generate backstop, `last-try?` logic is broken?"))))


(struct wall ([plan : Plan]
              [slices : (Listof Slice)]
              [backstop : (Listof Backstop-Run)])
  #:transparent #:type-name Wall)

(: generate-wall (-> Plan Wall))
(define (generate-wall plan)
  (let* ([slices (plan->slices plan)]
         [backstop (generate-backstop slices)])
    (wall plan slices backstop)))

{MAIN
 (: slice->pict (-> Slice Pict))
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

 (: wall->pict (-> Wall Pict))
 (define (wall->pict wall)
   (define slices (wall-slices wall))
   (define H (match slices
               [(list (slice _ tall short padding) more ...)
                (+ tall short padding)]))
   (define (backstop-run->pict [run : Backstop-Run])
     (define w (backstop-run-width run))
     (define x (backstop-run-depth run))
     (ct-superimpose (filled-rectangle w (max 0 (- H x))
                                       #:color (make-color 0 0 0 0.6)
                                       #:draw-border? #f)
                     (blank w H)))
   (define slice-pict
     (apply hc-append (map slice->pict slices)))
   (define backstop-pict
     (let ([backstop (wall-backstop wall)])
       (match backstop
         [(list a more ...)
          (apply hc-append (map backstop-run->pict backstop))]
         [else (error "assert fail")])))
   (lc-superimpose slice-pict backstop-pict))


 (let* ([plan (generate-plan 10 #t)]
        [wall (generate-wall plan)])
   (values plan
           (wall-slices wall)
           (wall-backstop wall)
           (scale (wall->pict wall) 10)))


 (define (p2 [I : Fixnum])
   (let* ([plan (list (body 3 #t)
                      (linkage 4 1)
                      (body 6 #f)
                      (linkage 3 -2)
                      (body 4 #t)
                      (linkage 3 I)
                      (body 4 #f)
                      (linkage 2 -1)
                      (body 5 #t)
                      )])
     (generate-wall plan)))
 (scale (wall->pict (p2 1)) 10)
 (scale (wall->pict (p2 -1)) 10)
 }


; A temporary structure, one per pixel.
; Here all dimensions are distance from the edge of the bounding box.
(struct pixel-slice ([padding : Fixnum]
                     [short : Fixnum]
                     [short-counter : Fixnum] ; unused for now, could enable variable short platforms
                     [tall : Fixnum]
                     [backstop : Fixnum])
  #:transparent #:type-name Pixel-Slice)

(: wall->pixel-slices (-> Wall Fixnum (Vectorof Pixel-Slice)))
(define (wall->pixel-slices wall pixel-width)
  (define WWW pixel-width)
  (define vec : (Vectorof Pixel-Slice)
    (make-vector WWW (pixel-slice 0 0 0 0 0)))

  (define slices (wall-slices wall))
  (define backstop (wall-backstop wall))

  (define current-backstop-w : Fixnum 0)
  (define current-backstop-d : Fixnum 0)

  (define current-slice-w : Fixnum 0)
  (define current-slice-pad : Fixnum 0)
  (define current-slice-short : Fixnum 0)
  (define current-slice-tall : Fixnum 0)
  (define short-flag? : Boolean #f)
  (define short-counter : Fixnum 0)

  (for ([i (in-range WWW)])
    (when (ufx= 0 current-backstop-w)
      (match backstop
        [(list (backstop-run w d) more ...)
         (set! backstop more)
         (set! current-backstop-w w)
         (set! current-backstop-d d)]))
    (when (ufx= 0 current-slice-w)
      (match slices
        [(list (slice w tall short pad) more ...)
         (define has-short? (ufx> short 0))
         (when (and (not short-flag?) has-short?)
           (set! short-counter (ufx+ 1 short-counter)))
         (set! short-flag? has-short?)
         (set! slices more)
         (set! current-slice-w w)
         (set! current-slice-pad pad)
         (set! current-slice-short (ufx+ pad short))
         (set! current-slice-tall (:ufx+ pad short tall))]))
    (set! current-backstop-w (ufx+ -1 current-backstop-w))
    (set! current-slice-w (ufx+ -1 current-slice-w))
    (vector-set! vec i (pixel-slice current-slice-pad
                                    current-slice-short
                                    short-counter
                                    current-slice-tall
                                    current-backstop-d)))
  vec)


(define-syntax-rule (define-magic-numbers [id val] ...)
  (begin
    (define id : Byte val)
    ...
    (module+ magic-numbers
      (provide id ...))))

(define-magic-numbers
  ; Warning - the order matters for border detection logic.
  [UNSET 0]
  [EMPTY 1]
  [SHORT 2]
  [SHORT-BORDER 3]
  [TALL 4]
  [TALL-BORDER 5]
  [PEAK 6]
  [PEAK-BORDER 7])

; A platform layout has no elevation or block information yet.
; It just has multiple areas represented in the Bytes array.
(struct platform-layout ([width : Fixnum]
                         [depth : Fixnum]
                         [array2d : Bytes])
  #:type-name Platform-Layout)

(: generate-platform-layout (-> Positive-Fixnum Positive-Fixnum Platform-Layout))
(define (generate-platform-layout w h)
  (define array2d (make-bytes (ufx* w h) UNSET))

  (define (gen-wall [len : Fixnum])
    (let* ([plan (generate-plan2 len #t)]
           [plan (straighten plan 7)])
      (generate-wall plan)))
  (define (sample [i : Integer] [wall : (Vectorof Pixel-Slice)] [j : Integer])
    ; Sample the 4 walls. At most 2 should be relevant (near the corners).
    ; Rely on `min` to ignore irrelevant samples.
    (let* ([slice (vector-ref wall j)]
           [backstop (pixel-slice-backstop slice)])
      (cond
        [(not (< i backstop))
         PEAK]
        [(< i (pixel-slice-padding slice))
         EMPTY]
        [(< i (pixel-slice-short slice))
         SHORT]
        [(< i (pixel-slice-tall slice))
         TALL]
        [else
         PEAK])))
  (let* ([wall-T (gen-wall w)]
         [wall-B (gen-wall w)]
         [wall-L (gen-wall h)]
         [wall-R (gen-wall h)]
         [vec-T (wall->pixel-slices wall-T w)]
         [vec-B (wall->pixel-slices wall-B w)]
         [vec-L (wall->pixel-slices wall-L h)]
         [vec-R (wall->pixel-slices wall-R h)]
         [idx 0])
    ; Populate the array without border logic:
    (define (samp [x : Fixnum] [y : Fixnum])
      (let* ([s1 (sample (+ 0 0 y) vec-T x)]
             [s2 (sample (- h y 1) vec-B x)]
             [s3 (sample (+ 0 0 x) vec-L y)]
             [s4 (sample (- w x 1) vec-R y)]
             [s : Byte (apply min (list s1 s2 s3 s4))])
        s))
    (for ([y : Fixnum (ufx-in-range h)])
      (for ([x : Fixnum (ufx-in-range w)])
        (let* ([s (samp x y)])
          (bytes-set! array2d idx s)
          (set! idx (ufx+ 1 idx)))))
    ; Now make another pass for border detection:
    (define (border? [x : Fixnum] [y : Fixnum] [limit : Fixnum])
      (define (in-bounds? [x : Fixnum] [y : Fixnum])
        (and (ufx>= x 0)
             (ufx>= y 0)
             (ufx< x w)
             (ufx< y h)))
      (define (shorter? [x : Fixnum] [y : Fixnum])
        (or (not (in-bounds? x y))
            (ufx< (samp x y) limit)))
      (or (shorter? (ufx+ -1 x) (ufx+ -1 y))
          (shorter? (ufx+ -1 x) (ufx+ 0 y))
          (shorter? (ufx+ -1 x) (ufx+ 1 y))
          (shorter? (ufx+ 0 x) (ufx+ -1 y))
          (shorter? (ufx+ 0 x) (ufx+ -1 y))
          (shorter? (ufx+ 1 x) (ufx+ -1 y))
          (shorter? (ufx+ 1 x) (ufx+ 0 y))
          (shorter? (ufx+ 1 x) (ufx+ 1 y))))
    (set! idx 0)
    (for ([y : Fixnum (ufx-in-range h)])
      (for ([x : Fixnum (ufx-in-range w)])
        (let ([me (bytes-ref array2d idx)])
          (cond
            [(and (= me SHORT)
                  (border? x y SHORT))
             (bytes-set! array2d idx SHORT-BORDER)]
            [(and (= me TALL)
                  (border? x y TALL))
             (bytes-set! array2d idx TALL-BORDER)]
            [(and (= me PEAK)
                  (border? x y PEAK))
             (bytes-set! array2d idx PEAK-BORDER)]))
        (set! idx (ufx+ 1 idx))))
    ; Done
    (platform-layout w h array2d)))

{MAIN
 (define (TODO3 [w : Positive-Fixnum] [h : Positive-Fixnum])
   (let* ([ph (generate-platform-layout w h)]
          [pixelvec (make-bytes (:ufx* 4 w h))]
          [bytes (platform-layout-array2d ph)])
     (define pixel-idx : Fixnum 0)
     (for ([idx : Fixnum (ufx-in-range (ufx* w h))])
       (let* ([val (bytes-ref bytes idx)]
              [rgb (ufx- 255 (ufx* val 30))])
         (define (put! [byte : Fixnum])
           (bytes-set! pixelvec pixel-idx byte)
           (set! pixel-idx (ufx+ 1 pixel-idx)))
         (put! (case val
                 [(1) 0] ; empty, alpha=0
                 [else 255]))
         (put! (case val
                 [(3 5 7) 255] ; short/tall/peak borders, red=255
                 [else rgb]))
         (put! rgb)
         (put! rgb)))
     (argb-pixels->pict pixelvec (cast w Nonnegative-Integer))))

 (let ([big (TODO3 330 56)])
   (values big
           (cc-superimpose big (TODO3 300 30))))

 (: save-img (-> (U Pict (Instance Bitmap%)) (U Output-Port Path-String) Any))
 (define (save-img img name)
   (cond
     [(pict? img)
      (save-img (pict->bitmap img 'unsmoothed) name)]
     [else
      (send img save-file name 'bmp)]))
 }
