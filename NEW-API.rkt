#lang typed/racket

(provide save-dir
         load-stage
         mark-writable
         save-stage!
         fill-area!
         bitmap->area
         area-contains?
         xz
         )

(require (prefix-in dqb: "lib-dqb.rkt")
         (only-in "lib.rkt" point)
         typed/pict
         (only-in typed/racket/draw Bitmap%))

; The Steam directory .../DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/
(define save-dir (make-parameter (ann #f (U #f Path-String))))

(struct stage ([stage : dqb:Stage])
  #:transparent #:type-name Stage)

(struct writable-stage stage ()
  #:transparent #:type-name Writable-Stage)

(: mark-writable (-> Stage Writable-Stage))
(define (mark-writable stage)
  (writable-stage (stage-stage stage)))

; Indicates that the contained value (e.g. an XZ or a Point)
; is relative to the chunk-id.
(struct (A) chunky ([chunk-id : Integer]
                    [val : A])
  #:type-name Chunky #:transparent)

(struct xz ([x : Integer]
            [z : Integer])
  #:type-name XZ #:transparent)

(struct rect ([start : XZ]
              [end : XZ])
  #:type-name Rect #:transparent)

(define-syntax-rule (in-rect/x rect)
  (in-range (xz-x (rect-start rect))
            (xz-x (rect-end rect))))
(define-syntax-rule (in-rect/z rect)
  (in-range (xz-z (rect-start rect))
            (xz-z (rect-end rect))))

(struct area ([bounds : Rect]
              [xzs : (Setof XZ)])
  #:transparent #:type-name Area)

(define (area-contains? [area : Area] [xz : XZ])
  (set-member? (area-xzs area) xz))

(: bitmap->area (-> (U (Instance Bitmap%) Path-String) Area))
(define (bitmap->area arg)
  (define bmp (bitmap arg))
  (define width : Integer
    (let ([w (pict-width bmp)])
      (or (and (integer? w) (cast w Integer))
          (error "bad width" w))))
  (define depth : Integer
    (let ([h (pict-height bmp)])
      (or (and (integer? h) (cast h Integer))
          (error "bad height" h))))
  (define pixels (pict->argb-pixels bmp))
  (define xzs (ann (make-hash) (Mutable-HashTable XZ #t)))
  (let ([index 0])
    (for ([z (in-range depth)])
      (for ([x (in-range width)])
        (let ([alpha (bytes-ref pixels index)])
          (set! index (+ 4 index)) ; 4 bytes per pixel
          (when (> alpha 0)
            (hash-set! xzs (xz x z) #t))))))
  (area (rect (xz 0 0) (xz width depth))
        (list->set (hash-keys xzs))))

(: load-stage (-> (U 'IoA) (U 'B00 'B01 'B02 Path) Stage))
(define (load-stage kind slot)
  (define path (case slot
                 [(B00 B01 B02)
                  (let ([sd (save-dir)]
                        [filename (case kind
                                    [(IoA) "STGDAT01.BIN"]
                                    [else (error "TODO" kind)])])
                    (if sd
                        (build-path sd (~a slot) filename)
                        (error "You must parameterize `save-dir` to load:" slot)))]
                 [else (ann slot Path)]))
  (define stg (dqb:open-stgdat kind path))
  (stage stg))

(define (clear-map! [stage : Writable-Stage]
                    #:above-y [above-y : Integer 0]
                    #:keep-items? [keep-items? : Boolean #f])
  (let ([stg (stage-stage stage)])
    (dqb:clear-map! stg #:above-y above-y #:keep-items? keep-items?)))

(: fill-area! (->* (Writable-Stage Area Integer #:y-max Integer)
                   (#:y-min Integer)
                   Void))
(define (fill-area! stage area block #:y-max y-max #:y-min [y-min : Integer 1])
  (let ([stg (stage-stage stage)]
        [bounds (area-bounds area)]
        [area-xzs (area-xzs area)])
    (for ([x (in-rect/x bounds)])
      (for ([z (in-rect/z bounds)])
        (when (set-member? area-xzs (xz x z))
          (for ([y (in-range y-min (+ 1 y-max))])
            (let ([p (point x y z)])
              (or (dqb:put-block! stg p block)
                  (error "TODO out of range:" p)))))))
    (void)))

(define (save-stage! [stage : Writable-Stage])
  (dqb:save-stgdat! (stage-stage stage)))
