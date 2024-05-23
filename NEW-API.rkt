#lang typed/racket

(provide save-dir
         load-stage
         mark-writable
         save-stage!
         fill-area!
         bitmap->area
         area-contains?
         xz
         put-hill!
         clear-area!
         print-column
         repair-sea!
         clear-stage!
         )

(module+ for-testing
  (provide blocks-hash))

(require (prefix-in dqb: "lib-dqb.rkt")
         (prefix-in lib: (only-in "lib.rkt" point))
         typed/pict
         typed/racket/stream
         (only-in typed/racket/draw Bitmap%))

(module+ test
  (require typed/rackunit))

(: simple-block? (-> Integer Boolean))
(define (simple-block? block)
  ; Seems to be
  ; * mask #x0FFF is the block ID
  ; * mask #xF000 is the chisel status
  (let ([masked (bitwise-and #x0F00 block)])
    (case masked
      ; Blocks placed by the player:
      [(#x800 #xA00) #t]
      ; Blocks placed by the game:
      [(#x000) (not (= 0 block))]
      [else #f])))

; The Steam directory .../DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/
(define save-dir (make-parameter (ann #f (U #f Path-String))))

(struct stage ([stage : dqb:Stage])
  #:transparent #:type-name Stage)

(define (stage-kind [stage : Stage])
  (dqb:stgdat-file-kind (dqb:stage-loaded-from (stage-stage stage))))

; Each inner vector is one row, holding chunk IDs from east to west.
; The outer vector contains all rows from north to south.
; A chunk ID of false indicates out-of-bounds.
(define-type Chunk-Layout (Vectorof (Vectorof (U #f Integer))))

(: get-chunk-layout (-> dqb:Stgdat-Kind Chunk-Layout))
(define (get-chunk-layout kind)
  (case kind
    [(IoA) IoA-chunk-layout]
    [else (error "Unexpected kind:" kind)]))

(: get-addr (-> dqb:Stgdat-Kind Point (U #f Integer)))
(define (get-addr kind point)
  (define (get-address [chunk : Integer] [x : Integer] [y : Integer] [z : Integer])
    (+ #x183FEF0
       (* chunk #x30000)
       (* y 32 32 2)
       (* z 32 2)
       (* x 2)))
  (let*-values ([(x-offset x) (quotient/remainder (point-x point) 32)]
                [(z-offset z) (quotient/remainder (point-z point) 32)])
    (let* ([chunk-layout (get-chunk-layout kind)]
           [row (vector-ref chunk-layout z-offset)]
           [chunk-id (vector-ref row x-offset)])
      (and chunk-id
           (get-address chunk-id x (point-y point) z)))))

(: stage-read (-> Stage Point (U #f Integer)))
(define (stage-read stage point)
  (let ([addr (get-addr (stage-kind stage) point)])
    (and addr
         (let* ([buffer (dqb:stage-buffer (stage-stage stage))]
                [lo (bytes-ref buffer (+ 0 addr))]
                [hi (bytes-ref buffer (+ 1 addr))])
           (bitwise-ior lo (arithmetic-shift hi 8))))))

(: stage-write! (-> Stage Point Integer (U #f Void)))
(define (stage-write! stage point block)
  (let ([addr (get-addr (stage-kind stage) point)])
    (and addr
         (let ([buffer (dqb:stage-buffer (stage-stage stage))])
           (bytes-set! buffer (+ 0 addr) (bitwise-bit-field block 0 8))
           (bytes-set! buffer (+ 1 addr) (bitwise-bit-field block 8 16))))))

; We actually allow mutations to a writable-stage, we just cannot
; allow those changes to be saved and overwrite the original file.
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

; OUCH - constructor is now x z y which is confusing!
; Should hide this... use a generic interface?
(struct point xz ([y : Integer])
  #:type-name Point #:transparent)

(define (make-point [xz : XZ] [y : Integer])
  (point (xz-x xz) (xz-z xz) y))

(define point-x xz-x)
(define point-z xz-z)

(define (neighbors [val : XZ])
  (let ([x (xz-x val)]
        [z (xz-z val)])
    (list (xz (add1 x) z)
          (xz (sub1 x) z)
          (xz x (add1 z))
          (xz x (sub1 z)))))

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
              [contains-func : (-> XZ Any)])
  #:transparent #:type-name Area)

; The following stream code uses way too much memory, why?
#;(define-syntax-rule (in-area area)
    (in-stream (area-xzs area)))

#;(: area-xzs (-> Area (Sequenceof XZ)))
#;(define (area-xzs area)
    (let* ([bounds (area-bounds area)]
           [contains? (area-contains-func area)]
           [x (+ -1 (xz-x (rect-start bounds)))] ; move cursor prior
           [z (xz-z (rect-start bounds))]
           [end-x (xz-x (rect-end bounds))]
           [end-z (xz-z (rect-end bounds))])
      (: advance! (-> (Sequenceof XZ)))
      (define (advance!)
        (set! x (+ 1 x))
        (when (>= x end-x)
          (set! x 0)
          (set! z (+ 1 z)))
        (if (< z end-z)
            (let ([current (xz x z)])
              (if (contains? current)
                  (stream-cons current (advance!))
                  (advance!)))
            (list)))
      (advance!)))

#;{module+ test
    (check-equal? (stream->list (stream-take (area-xzs (stage-full-area 'IoA)) 3))
                  ; 7 OOB chunks means that the first valid X is 7*32 = 224
                  (list (xz 224 0) (xz 225 0) (xz 226 0)))
    }

(define (area-contains? [area : Area] [xz : XZ])
  ((area-contains-func area) xz))

(define (parse-map [rows : (Listof (Listof (U '_ 'X)))])
  (let ([chunk-id -1])
    (for/vector : (Vectorof (Vectorof (U #f Integer)))
      ([row rows])
      (for/vector : (Vectorof (U #f Integer))
        ([cell row])
        (case cell
          [(_) #f]
          [(X) (begin (set! chunk-id (+ 1 chunk-id))
                      chunk-id)]
          [else (error "assert fail")])))))

(define IoA-chunk-layout : Chunk-Layout
  (parse-map '((_ _ _ _ _ _ _ X X X X X X X X _ _ _ X X X _ _ _ _ _ _)
               (_ _ _ _ _ _ X X X X X X X X X X X X X X X X X _ _ _ _)
               (_ _ _ _ X X X X X X X X X X X X X X X X X X X _ _ _ _)
               (_ _ _ X X X X X X X X X X X X X X X X X X X X _ _ _ _)
               (_ _ X X X X X X X X X X X X X X X X X X X X X X X X _)
               (_ X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (_ X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X X X X X X X X X X X X X X _)
               (_ X X X X X X X X X X X X X X X X X X X X X X X _ _ _)
               (_ _ X X X X X X X X X X X X X X X X X X X X X _ _ _ _)
               (_ _ X X X X X X X X X X X X X X X X X X X _ _ _ _ _ _)
               (_ _ _ _ X X X X X X X X X X X X X X X _ _ _ _ _ _ _ _)
               (_ _ _ _ _ _ _ _ _ X X X X X X _ _ _ _ _ _ _ _ _ _ _ _)
               (_ _ _ _ _ _ _ _ _ X X X X X _ _ _ _ _ _ _ _ _ _ _ _ _)
               (_ _ _ _ _ _ _ _ _ _ X X X _ _ _ _ _ _ _ _ _ _ _ _ _ _))))

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
  (define all-empty? : Boolean #t)
  (define all-full? : Boolean #t)
  (let ([index 0])
    (for ([z (in-range depth)])
      (for ([x (in-range width)])
        (let ([alpha (bytes-ref pixels index)])
          (set! index (+ 4 index)) ; 4 bytes per pixel
          (cond
            [(> alpha 0)
             (set! all-empty? #f)
             (hash-set! xzs (xz x z) #t)]
            [else
             (set! all-full? #f)])))))
  (when (or all-empty? all-full?)
    (error "Expected some fully-transparent pixels and some other pixels, but ~a pixels are fully-transparent."
           (if all-empty? "all" "zero")))
  (area (rect (xz 0 0) (xz width depth))
        (let ([set (list->set (hash-keys xzs))])
          (lambda ([xz : XZ]) (set-member? set xz)))))

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

(define (clear-map! [stage : Stage]
                    #:above-y [above-y : Integer 0]
                    #:keep-items? [keep-items? : Boolean #f])
  (let ([stg (stage-stage stage)])
    (dqb:clear-map! stg #:above-y above-y #:keep-items? keep-items?)))

(: fill-area! (->* (Stage Area Integer #:y-max Integer)
                   (#:y-min Integer)
                   Void))
(define (fill-area! stage area block #:y-max y-max #:y-min [y-min : Integer 1])
  (let ([stg (stage-stage stage)]
        [bounds (area-bounds area)])
    (for ([x (in-rect/x bounds)])
      (for ([z (in-rect/z bounds)])
        (when (area-contains? area (xz x z))
          (for ([y (in-range y-min (+ 1 y-max))])
            (let ([p (lib:point x y z)])
              (or (dqb:put-block! stg p block)
                  (error "TODO out of range:" p)))))))
    (void)))

(define (save-stage! [stage : Writable-Stage])
  (dqb:save-stgdat! (stage-stage stage)))

(: rand (All (A) (-> (Vectorof A) A)))
(define (rand vec)
  (vector-ref vec (random (vector-length vec))))

(: find-outskirts (-> Area (-> XZ Any) (Listof XZ)))
(define (find-outskirts area extra-outside?)
  (define (outside? [xz : XZ])
    (or (not (area-contains? area xz))
        (extra-outside? xz)))
  (define (inside? [xz : XZ])
    (not (outside? xz)))
  (define (outskirts? [xz : XZ])
    (and (inside? xz)
         (ormap outside? (neighbors xz))))
  (define result (ann (list) (Listof XZ)))
  (define bounds (area-bounds area))
  (for ([z (in-rect/z bounds)])
    (for ([x (in-rect/x bounds)])
      (let* ([xz (xz x z)])
        (when (outskirts? xz)
          (set! result (cons xz result))))))
  result)

(: put-hill! (->* (Stage Area Integer #:y-max Integer)
                  (#:y-min Integer
                   #:step-start Integer
                   #:step-height Integer
                   #:run-lengths (Vectorof Integer)
                   #:run-heights (Vectorof Integer))
                  Void))
(define (put-hill! stage area block #:y-max y-max
                   #:y-min [y-min 1]
                   #:step-start [step-start y-min]
                   #:step-height [step-height 5]
                   #:run-lengths [run-lengths '#(1 2 2 3 3 4 5)]
                   #:run-heights [run-heights '#(1 2 2 3 3 4 4 5)])
  (define (put-column! [xz : XZ] [y-max : Integer])
    (for ([y (in-range y-min (+ 1 y-max))])
      (let ([p (lib:point (xz-x xz) y (xz-z xz))])
        (or (dqb:put-block! STG p block)
            (error "TODO out of range" p)))))
  (define STG (stage-stage stage))
  (define bounds (area-bounds area))
  (define heights (ann (make-hash) (Mutable-HashTable XZ Integer)))
  (define (done? [xz : XZ])
    (hash-ref heights xz (lambda () #f)))
  (: gen-hill! (-> (Listof XZ) Integer Void))
  (define (gen-hill! outskirts floor)
    (cond
      [(>= floor y-max)
       (void)]
      [(empty? outskirts)
       (void)]
      [else
       (for ([xz outskirts])
         (when (not (done? xz))
           (define height (min (+ floor (rand run-heights))
                               y-max))
           (hash-set! heights xz height)
           (put-column! xz height)))
       (gen-hill! (shuffle (find-outskirts area done?))
                  (+ step-height floor))]))
  ; Fill in sides
  (gen-hill! (find-outskirts area (lambda (xz) #f)) step-start)
  ; Fill in top
  (for ([z (in-rect/z bounds)])
    (for ([x (in-rect/x bounds)])
      (let ([xz (xz x z)])
        (when (and (not (done? xz))
                   (area-contains? area xz))
          (put-column! xz y-max)))))
  (void))

(define (clear-area! [stage : Stage] [area : Area] #:y-min [y-min : Integer 1])
  (define STG (stage-stage stage))
  (define bounds (area-bounds area))
  (for ([z (in-rect/z bounds)])
    (for ([x (in-rect/x bounds)])
      (when (area-contains? area (xz x z))
        (for ([y (in-range y-min 96)])
          (let ([p (lib:point x y z)])
            (or (dqb:put-block! STG p 0)
                (error "TODO out of range" p)))))))
  (void))

(define (print-column [stage : Stage] [xz : XZ])
  (for ([y (in-range 96)])
    (let* ([p (make-point xz y)]
           [val (stage-read stage p)])
      (println (list "y:" y "block:" val)))))

(define (stage-full-area [kind : dqb:Stgdat-Kind])
  (let* ([chunk-layout (get-chunk-layout kind)]
         [depth (* 32 (vector-length chunk-layout))]
         [width (* 32 (vector-length (vector-ref chunk-layout 0)))]
         [bounds (rect (xz 0 0) (xz width depth))])
    (define (contains? [xz : XZ])
      (get-addr kind (make-point xz 0)))
    (area bounds contains?)))

(define-syntax-rule (get-area x stage)
  (case x
    [(all) (stage-full-area (stage-kind stage))]
    [else (ann x Area)]))

(define (repair-sea! [stage : Stage] [where : (U 'all Area)] #:sea-level [sea-level : (U #f Integer) #f])
  ; Notes from IoA testing:
  ; Sea level is at y=31.
  ; This means that if you place a block such that the bottom sits in the sea
  ; and the top is out of the sea, that block is at y=31.
  ;
  ; Ideally this function would also repair all the 24-byte records.
  ; (I strongly suspect there is an "undersea" flag there.)
  ; But for now, too bad, the user would have to manually destroy those items
  ; before using this function and put them back afterwards.
  (define kind (stage-kind stage))
  (define water-level : Integer (or sea-level
                                    (case kind
                                      [(IoA) 31]
                                      [else (error "Unexpected kind" kind)])))
  ; The top-sea and full-sea values that follow are confirmed on IoA.
  ; Other islands might use other values, more investigation needed.
  (define top-sea #x1A4) ; The shallow sea, placed at y = sea level
  (define full-sea #x155) ; Full sea, placed at y < sea level

  (define (vacant? [point : Point])
    ; This probably needs more cases... TBD
    (case (stage-read stage point)
      [(0) #t]
      [else #f]))

  (define area (get-area where stage))
  (define bounds (area-bounds area))
  (for ([z (in-rect/z bounds)])
    (for ([x (in-rect/x bounds)])
      (when (area-contains? area (xz x z))
        (for ([y (in-range (+ 1 water-level))])
          (let ([p (make-point (xz x z) y)])
            (when (vacant? p)
              (stage-write! stage p (if (= y water-level)
                                        top-sea
                                        full-sea))))))))
  (void))

(define (clear-stage! [stage : Stage] [where : (U 'all Area)]
                      #:min-y [min-y : Integer 1]
                      #:keep-items? [keep-items? : Boolean #f])
  (define buffer (dqb:stage-buffer (stage-stage stage)))
  ; Reset count of 24-byte records to zero
  ; (this is probably a 4-byte number but 0xC8000 is the max)
  (when (not keep-items?)
    (bytes-set! buffer #x24E7CD 0)
    (bytes-set! buffer #x24E7CE 0)
    (bytes-set! buffer #x24E7CF 0))
  (define area (get-area where stage))
  (define bounds (area-bounds area))
  (for ([z (in-rect/z bounds)])
    (for ([x (in-rect/x bounds)])
      (when (area-contains? area (xz x z))
        (for ([y (in-range 96)])
          (let ([p (make-point (xz x z) y)])
            (when (and (>= y min-y)
                       (or (not keep-items?)
                           (simple-block? (or (stage-read stage p) 0))))
              (stage-write! stage p 0)))))))
  (void))

(define (blocks-hash [stage : Stage]
                     #:where [where : (U 'all Area) 'all])
  ; For use by automated tests, to avoid adding too many large files into git.
  ; If the hash changes, you might need to use an older version of the code
  ; to export the complete data for diffing.
  (define area (get-area where stage))
  (define bounds (area-bounds area))
  (define hash1 0)
  (define hash2 0)
  (for ([z (in-rect/z bounds)])
    (for ([x (in-rect/x bounds)])
      (when (area-contains? area (xz x z))
        (for ([y (in-range 96)])
          (let* ([p (make-point (xz x z) y)]
                 [block (or (stage-read stage p)
                            (error "assert fail"))])
            ; I think a hash collision would be very unlikely, but I can't prove it.
            ; Using two different hashes seems like it would be much more resistant
            ; to any surprising block patterns that might thwart one of the hashes.
            (set! hash1 (bitwise-and #xFFFFFF (+ block (* hash1 31))))
            (set! hash2 (bitwise-and #xFFFFFF (+ block (* hash2 17)))))))))
  (list hash1 hash2))
