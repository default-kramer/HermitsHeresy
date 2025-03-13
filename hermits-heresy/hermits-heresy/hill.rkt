#lang typed/racket

(provide Hill hill? area->hill area->hill2 bitmap->hill
         make-hill
         bitmap-hill-adjuster
         hill-area hill-elevations)

(require "area.rkt"
         "chunky-area.rkt"
         "basics.rkt"
         "block.rkt"
         "bitmap-sampler.rkt"
         "ufx.rkt"
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
                      #:adjust-y Fixnum)
                     Hill))
(define (bitmap->hill arg
                      #:semitransparent-handling [semitransparent-handling 'adjust]
                      #:adjust-y [adjust-y 0]
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
             (set! all-empty? #f)
             (hash-set! elevations (cons x z) adjusted-elevation)]
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

(struct hill-adjuster ([func : (-> XZ (U #f Fixnum) (U #f Fixnum))])
  #:type-name Hill-Adjuster
  #:property prop:authentic #t
  #:transparent)

(define (standard-hill-adjuster [sampler : Fixnum-Sampler])
  (define func (fixnum-sampler-func sampler))
  (define (adjust [xz : XZ] [val : (U #f Fixnum)])
    (and val
         (ufx+ val (or (func xz) 0))))
  (hill-adjuster adjust))

(: bitmap-hill-adjuster (->* [(U (Instance Bitmap%) Path-String)
                              #:rgb RGB-Spec
                              #:project Project-Spec
                              ]
                             [#:invert? Any
                              #:normalize Normalize-Spec
                              ]
                             Hill-Adjuster))
(define (bitmap-hill-adjuster arg
                              #:rgb rgb-spec
                              #:invert? [rgb-invert? #f]
                              #:normalize [normalize-spec #f]
                              #:project project-spec)
  (standard-hill-adjuster
   (make-bitmap-sampler arg
                        #:rgb rgb-spec
                        #:invert? rgb-invert?
                        #:normalize normalize-spec
                        #:project project-spec)))

(: make-hill (-> Fixnum-Sampler Hill-Adjuster * Hill))
(define (make-hill primary . adjusters)
  (define bounding-rect (fixnum-sampler-bounding-rect primary))
  (define elevations (ann (make-hash) (Mutable-HashTable (Pairof Integer Integer) Fixnum)))

  ; NOMERGE - how do we reproduce the error messaging of the original bitmap->hill
  ; now that we operate on any arbitrary Fixnum-Sampler ??
  ; Do we even need that error messaging now that hill construction is so much simpler?
  (define all-empty? : Boolean #t)
  (define all-full? : Boolean #t)
  (define semitransparent-warned? : Boolean #f)

  (define primary-func (fixnum-sampler-func primary))

  (: apply-adjusters (-> (U #f Fixnum) XZ (Listof Hill-Adjuster)
                         (U #f Fixnum)))
  (define (apply-adjusters sample xz adjusters)
    (match adjusters
      [(list) sample]
      [(list a adjusters ...)
       (let* ([func (hill-adjuster-func a)]
              [sample (func xz sample)])
         (apply-adjusters sample xz adjusters))]))

  ; NOMERGE get rid of this temp logging:
  (define all-funcs (cons primary-func
                          (map hill-adjuster-func adjusters)))
  (println (list "any impersonators?" (ormap impersonator? all-funcs)))

  (for/rect ([#:z z #:x x #:rect bounding-rect])
    (let* ([xz (xz x z)]
           [sample (primary-func xz)]
           [sample (apply-adjusters sample xz adjusters)])
      (when sample
        (hash-set! elevations (cons x z) sample))))

  (define the-area
    ; NOMERGE probably build-chunky-area should ask for a Rect instead of W,H
    ; because it (currently) assumes that W,H is relative to 0,0
    (build-chunky-area (xz-x (rect-end bounding-rect))
                       (xz-z (rect-end bounding-rect))
                       (lambda ([xz : XZ])
                         (hash-ref elevations (cons (xz-x xz) (xz-z xz)) #f))
                       (lambda args #f)))

  (hill the-area (make-immutable-hash (hash->list elevations))))
