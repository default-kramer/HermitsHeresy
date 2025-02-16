#lang typed/racket

(provide Hill hill? area->hill area->hill2 bitmap->hill
         hill-area hill-elevations
         make-shell make-shell2
         )

(require "area.rkt"
         "chunky-area.rkt"
         "interpolator.rkt"
         "basics.rkt"
         "block.rkt"
         "ufx.rkt"
         racket/flonum
         typed/pict
         (only-in typed/racket/draw Bitmap%))

(struct hill ([area : Area]
              [elevations : (Immutable-HashTable (Pairof Integer Integer) Fixnum)])
  #:type-name Hill #:transparent)

(define (area->hill [area : Area] [elevation-func : (-> XZ Fixnum)])
  (define elevations : (Immutable-HashTable (Pairof Integer Integer) Fixnum)
    (hash))
  (for/area ([xz area])
    (let ([elevation (elevation-func xz)])
      (set! elevations (hash-set elevations (cons (xz-x xz) (xz-z xz)) elevation))))
  (hill area
        elevations))

(define (area->hill2 [area : Area] [bumps : Hill])
  (define (elevation-func [xz : XZ])
    (hash-ref (hill-elevations bumps)
              (cons (xz-x xz) (xz-z xz))
              (lambda () 0)))
  (area->hill area elevation-func))

(: bitmap->hill (->* ((U (Instance Bitmap%) Path-String))
                     (#:semitransparent-handling (U 'ignore 'adjust)
                      #:adjust-y Fixnum
                      #:cutoff-y Fixnum)
                     Hill))
(define (bitmap->hill arg
                      #:semitransparent-handling [semitransparent-handling 'adjust]
                      #:adjust-y [adjust-y 0]
                      #:cutoff-y [cutoff-y 0]
                      )
  ; Semi-transparent pixels were a pain point for the first two users (one of them was me),
  ; so I think it's best that adjusting for this is the default behavior.
  (define bmp (bitmap arg))
  (define width : Fixnum
    (let ([w (pict-width bmp)])
      (or (and (fixnum? w) (cast w Fixnum))
          (error "bad width" w))))
  (define depth : Fixnum
    (let ([h (pict-height bmp)])
      (or (and (fixnum? h) (cast h Fixnum))
          (error "bad height" h))))
  (define pixels (pict->argb-pixels bmp))
  ; Use Pairof here to make sure XZ and Point are handled correctly
  (define elevations (ann (make-hash) (Mutable-HashTable (Pairof Integer Integer) Fixnum)))
  (define all-empty? : Boolean #t)
  (define all-full? : Boolean #t)
  (define semitransparent-warned? : Boolean #f)
  (let ([index 0])
    (for ([z (in-range depth)])
      (for ([x (in-range width)])
        (let* ([alpha (bytes-ref pixels index)]
               [red (bytes-ref pixels (+ 1 index))]
               [green (bytes-ref pixels (+ 2 index))]
               [blue (bytes-ref pixels (+ 3 index))]
               [total (+ red green blue)])
          (set! index (+ 4 index)) ; 4 bytes per pixel
          (cond
            [(> alpha 0)
             (define raw-elevation : Fixnum
               (max 0 (ufx+ adjust-y
                            (max 0 (ufx- 95 (quotient (max red green blue) 2))))))
             (define adjusted-elevation : Fixnum
               (cond
                 [(= alpha 255) raw-elevation]
                 [else
                  (when (not semitransparent-warned?)
                    (set! semitransparent-warned? #t)
                    (show-msg "!! WARNING !! Your hill bitmap contains at least 1 semi-transparent pixel (alpha=~a at ~a,~a).
* ~a
* In file: ~a"
                              alpha x z
                              (ann (case semitransparent-handling
                                     [(adjust) "Elevation is being adjusted, but may not be exactly what you wanted."]
                                     [(ignore) "Elevation is not being adjusted. If you see spikes, this is the most likely cause."])
                                   String)
                              arg))
                  (case semitransparent-handling
                    ; This adjustment formula looks good to me...
                    [(adjust) (cast (round (/ (* raw-elevation alpha) 255)) Fixnum)]
                    [(ignore) raw-elevation])]))
             (when (ufx>= adjusted-elevation cutoff-y)
               (hash-set! elevations (cons x z) adjusted-elevation))
             (set! all-empty? #f)]
            [else
             (set! all-full? #f)])))))
  (when (or all-empty? all-full?)
    (error (format "Expected some fully-transparent pixels and some other pixels, but ~a pixels are fully-transparent."
                   (if all-empty? "all" "zero"))))
  (define the-area
    (build-chunky-area width depth
                       (lambda ([xz : XZ])
                         (hash-ref elevations (cons (xz-x xz) (xz-z xz)) #f))
                       (lambda args #f)))
  (hill the-area (make-immutable-hash (hash->list elevations))))

; TODO this function is duplicated:
(define (neighbors [val : XZ])
  (let ([x (xz-x val)]
        [z (xz-z val)])
    (list (xz (ufx+ 1 x) z)
          (xz (ufx+ -1 x) z)
          (xz x (ufx+ 1 z))
          (xz x (ufx+ -1 z)))))

(define (make-shell [old-hill : Hill] #:y [y : Fixnum])
  (define old-area (hill-area old-hill))
  (define (in-old-area? [xz : XZ])
    (area-contains? old-area xz))
  (define new-elevations : (Immutable-HashTable (Pairof Integer Integer) Fixnum)
    (hash))
  (define new-area
    (let-values ([(end-x end-z) (xz->values (rect-end (area-bounds old-area)))])
      (build-chunky-area
       (ufx+ 1 end-x)
       (ufx+ 1 end-z)
       (lambda ([xz : XZ])
         (and (not (in-old-area? xz))
              (ormap in-old-area? (neighbors xz))
              (let-values ([(x z) (xz->values xz)])
                (set! new-elevations (hash-set new-elevations (cons x z) y))
                #t)))
       (lambda args #f))))
  (println (list "shell" (hash-count new-elevations)))
  (hill new-area new-elevations))

(define (make-shell2 [old-hill : Hill]
                     #:y [y : Fixnum]
                     #:adjuster [adjuster : (-> Fixnum Fixnum Fixnum)])
  (define old-area (hill-area old-hill))
  (define (in-old-area? [xz : XZ])
    (area-contains? old-area xz))
  (define old-elevations (hill-elevations old-hill))
  (define new-elevations : (Immutable-HashTable (Pairof Integer Integer) Fixnum)
    old-elevations)
  (define (get-y [xz : XZ])
    (let-values ([(x z) (xz->values xz)])
      (let ([elev (hash-ref old-elevations (cons x z) #f)])
        (min y (or elev -1)))))
  (define new-area
    (let*-values ([(end-x end-z) (xz->values (rect-end (area-bounds old-area)))]
                  [(start-x start-z) (xz->values (rect-start (area-bounds old-area)))]
                  [(start-x) (ufx+ -3 start-x)]
                  [(start-z) (ufx+ -3 start-z)]
                  [(end-x) (ufx+ 3 end-x)]
                  [(end-z) (ufx+ 3 end-z)]
                  [(new-rect) (make-rect (xz start-x start-z) (xz end-x end-z))]
                  [(foo) (make-interpolator new-rect 6)]
                  [(adjuster) (lambda ([x : Fixnum] [z : Fixnum])
                                (let ([flo (or (interpolate foo (make-xz x z))
                                               (error "assert fail"))])
                                  (cond
                                    [(fl< flo 0.33) 0]
                                    [(fl< flo 0.66) -1]
                                    [else -2])))])
      (build-chunky-area
       (ufx+ 0 end-x)
       (ufx+ 0 end-z)
       (lambda ([xz : XZ])
         (and #t #;(not (in-old-area? xz)) ; obviously wrong, would need to be an elevation test instead
              (let* ([elevs (map get-y (cons xz (neighbors xz)))]
                     [elev (apply max elevs)]
                     ; This ain't bad:
                     #;[elev (ufx+ -1 (ufx+ (random 3) elev))])
                (and (ufx> elev 0)
                     (let*-values ([(x z) (xz->values xz)]
                                   ; but this is better:
                                   [(elev) (ufx+ elev (adjuster x z))])
                       (set! new-elevations (hash-set new-elevations (cons x z) elev))
                       #t)))))
       (lambda args #f))))
  (println (list "shell2" (hash-count new-elevations)))
  (hill new-area new-elevations))