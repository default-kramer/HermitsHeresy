#lang typed/racket

; TODO let's standardize all ranges to be low <= i < hi.
; Otherwise it will cause more confusion than it solves.
; Use naming like y-start and y-end instead of the existing
; y-min and y-max which are usually inclusive on both ends.

(provide save-dir
         load-stage
         item-count
         block
         fill-area!
         bitmap->area
         bitmap->hill
         area->hill area->hill2
         decorate-peaks! chisel simple?
         area-contains?
         xz
         put-hill!
         print-column
         repair-sea!
         clear-area!
         stage->pict stage->pictOLD
         TODO
         create-golem-platforms!
         protected-areas ; WARNING this will probably be slow? And area combiner functions would be better anyway?
         copy-all-save-files!
         save-stage!
         find-block-name
         )

(module+ everything
  (provide stage-buffer
           add-chunk-ids!))

(module+ for-testing
  (provide blocks-hash hill-ref)

  (define (hill-ref [hill : Hill] [loc : (Pairof Fixnum Fixnum)])
    (define locxz (xz (car loc) (cdr loc)))
    (and (area-contains? (hill-area hill) locxz)
         (hash-ref (hill-elevations hill) loc))))

(require (prefix-in zlib: "zlib.rkt")
         "chunk.rkt"
         "basics.rkt"
         "ufx.rkt"
         "config-util.rkt"
         "blockdata/blockdef.rkt"
         racket/hash
         typed/pict
         racket/fixnum
         typed/racket/unsafe
         (only-in typed/racket/draw Bitmap%))

(require/typed racket
               [copy-file (->* (Path-String Path-String)
                               (#:exists-ok? Any)
                               Any)])

(module+ test
  (require typed/rackunit))

(define-syntax-rule (show-msg arg ...)
  (let ()
    (displayln (format arg ...))
    (void)))

(: block (-> Symbol Fixnum))
(define block
  (let ()
    (define already-warned (ann (make-hash) (Mutable-HashTable Symbol #t)))
    (lambda ([sym : Symbol])
      (define blockdef (symbol->blockdef sym))
      (when (not blockdef)
        (error "Unknown block:" sym))
      (define id (blockdef-id blockdef))
      (define ambiguities (symbol->ambiguities sym))
      (when (and ambiguities
                 (not (hash-ref already-warned sym (lambda () #f))))
        (hash-set! already-warned sym #t)
        (show-msg "Choosing (block '~a) to mean ~a, but other values were possible: ~a" sym id ambiguities))
      id)))

#;(define-syntax-rule (define-blocks f [id val IGNORE ...] ...)
    (define (f a)
      (case a
        ; Hmm, so far it looks like many or all blocks can be used without the following #x800 mask
        ; (so having 0 for the hi byte)... and the zero-hi-byte indicates a prebuilt block?
        ; The #x800 seems to indicate a player-placed block?
        [(id) (bitwise-ior #x800 val)]
        ...
        [else (error "Unknown block:" a)])))


; == ERRATA ==
; The following idea that there is a "simple block?" flag is not correct.
; It is just coincidentally true in many cases.
; But there exist at least some block IDs which do not require a 24-byte record
; in the range #x400 - #x800, such as many liquids.
; == END ERRATA ==
; Blocks are saved as 2 bytes.
; One of the bits is a "simple block?" flag.
; A simple block is fully represented by these 2 bytes, while everything else
; (e.g. decorative items) will have extra info stored in a 24-byte record elsewhere.
; After endian adjustment we have this bit layout (applies to simple blocks only):
;   cccc p0ii iiii iiii
; where
;   c - chisel status
;   p - flag (not really: "placed by player?" flag)
;   0 - zero (not really: zero here indicates simple block)
;   i - block ID
; The 4-bit chisel status can be one of:
; * 0 - no chisel
; * 1/3/5/7 - diagonal chisel N/E/S/W, matches (blueprint.chisel_status << 4)
; * 2/4/6/8 - diagonal chisel SW/SE/NW/NE
; * 9/a/b/c - concave chisel NW/SW/SE/NE
; * d/e - flat chisel hi/lo
; The p flag appears to indicate whether the block was created/placed by the game (0)
; or placed by the player (1). I think the huge pyramid the NPCs build also uses a 0 flag.
; And maybe all NPC-placed blocks do?
; And when you use the trowel, the new blocks will have p=0 (confirmed with Seaweed-Styled on my IoA).
;
; Note that not every simple block can be chiseled.
; For example, shallow (top level) seawater is a simple block (stored as #x1A4 aka "A4 01")
; but it cannot be chiseled (actually I haven't tested what happens if you try...)
;
; For future reference, I believe that every non-simple block must have one of the following values:
;  04DE (1246)
;  0537 (1335)
;  0590 (1424)
;  05E9 (1513)
;  0642 (1602)
;  069B (1691)
;  06F4 (1780)
;  074D (1869)
;  07FF (2047)

; This has been replaced by the Sapphire's json data.
; Keeping it here just in case I need to refer to it
#;(define-blocks block
    ; x01 - unbreakable floor of map
    [Earth #x02]
    [Grassy-Earth #x03]
    [Limegrassy-Earth #x04]
    [Tilled-Soil #x05]
    [Clay #x06] ; unsure
    [Mossy-Earth #x07]
    [Chalk #x08]
    [Chunky-Chalk #x09]
    [Obsidian #x0A]
    [Sand #x0B]
    [Sandstone #x0C]
    [Sandy-Sandstone #x0D]
    ; x0E - a reddish block
    [Ash #x0F] ; unsure
    ; x10 - illegal
    ; x11 - purple peat?
    [Accumulated-Snow #x12] ; unsure
    [Snow #x13]
    [Ice #x14]
    [Clodstone #x15]
    [Crumbly-Clodstone #x16]
    [Basalt #x17]
    ; x18 - nothing?
    [Lava #x19]
    [Vault-Wall #x1A] ; Vault Wall that I place saves as #xA7A I think...?
    [Viny-Vault-Wall #x1B]
    ; ======
    [Light-Dolomite #x82]
    [Dark-Dolomite #x83]
    [Stony-Soil #x8D]
    [Seaside-Sand #x92]
    [Arid-Earth #x93]
    [Chert #x95]
    [Chunky-Chert #x99]
    [Spoiled-Soil #x9C]
    [Umber #xD1]
    [Lumpy-Umber #xF1]
    [Seaweed-Styled-Block #x2CC #:name "Seaweed-Styled Block"]
    [Seaside-Scene-Block #x2CE]
    [Old-Skool-Wall-Block #x333 #:name "Old-Skool Wall Block"]
    )

; The precise definition of "simple?" is "any block which can be placed
; into the blockdata without adding extra information anywhere else."
; So any item which requires a 24-byte record is not simple.
; And I think everything else is simple? Time will tell...
(: simple? (-> Integer Boolean))
(define (simple? block)
  (case block
    ; Copied these from the turtle-insect data files:
    [(1246 1335 1424 1513 1602 1691 1780 1869 2047) #f]
    [else #t]))

{module+ test
  (check-true (simple? (block 'Clodstone)))
  (check-false (simple? 2047))
  (check-true (simple? 0)) ; emptiness is simple
  }

(define (chisel [block : Fixnum] [kind : Any])
  (let ([block (ufxand #x7FF block)])
    (case kind
      [(flat-lo) (ufxior #xE000 block)]
      [(flat-hi) (ufxior #xD000 block)]
      [else (error "TODO more chisels..." kind)])))

(define-type Stgdat-Kind (U 'IoA))

(define header-length #x110)

(struct stgdat-file ([kind : Stgdat-Kind]
                     [path : Path])
  #:transparent #:type-name Stgdat-File)

(struct stage ([loaded-from : Stgdat-File]
               [header : Bytes] ; mutable (but we probably won't)
               [buffer : Bytes] ; mutable TODO remove this from core? (just use chunks)
               ; Or maybe keep the buffer to know what the file had when it was loaded.
               [chunks : (Immutable-Vectorof Chunk)]
               )
  #:type-name Stage)

(define (stage-kind [stage : Stage])
  (stgdat-file-kind (stage-loaded-from stage)))

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

(define protected-areas (make-parameter (ann (list) (Listof Area))))

; Each inner vector is one row, holding chunk IDs from east to west.
; The outer vector contains all rows from north to south.
; A chunk ID of false indicates out-of-bounds.
(define-type Chunk-Layout (Vectorof (Vectorof (U #f Integer))))

(: get-chunk-layout (-> Stgdat-Kind Chunk-Layout))
(define (get-chunk-layout kind)
  (case kind
    [(IoA) IoA-chunk-layout]
    [else (error "Unexpected kind:" kind)]))

(: chunk-translate (-> Chunk-Layout XZ (U #f (Chunky XZ))))
(define (chunk-translate chunk-layout xz)
  (let*-values ([(x-offset x) (quotient/remainder (xz-x xz) 32)]
                [(z-offset z) (quotient/remainder (xz-z xz) 32)])
    (let* ([row (vector-ref chunk-layout z-offset)]
           [chunk-id (vector-ref row x-offset)])
      (and chunk-id
           (chunky chunk-id (make-xz x z))))))

(: stage-read (-> Stage Point (U #f Integer)))
(define (stage-read stage point)
  (let* ([chunk-layout (get-chunk-layout (stage-kind stage))]
         [chunky (chunk-translate chunk-layout point)])
    (and chunky
         (let* ([chunks (stage-chunks stage)]
                [chunk (vector-ref chunks (chunky-chunk-id chunky))]
                [xz (chunky-val chunky)])
           (chunk-ref chunk #:x (xz-x xz) #:z (xz-z xz) #:y (point-y point))))))

(: stage-write! (-> Stage Point Integer (U #f Void)))
(define (stage-write! stage point block)
  (define (protected? [area : Area])
    (area-contains? area point))
  (define chunk-layout (get-chunk-layout (stage-kind stage)))
  (define chunky (chunk-translate chunk-layout point))
  (cond
    [(not chunky) #f]
    [(ormap protected? (protected-areas)) #f]
    [else (let* ([chunks (stage-chunks stage)]
                 [chunk (vector-ref chunks (chunky-chunk-id chunky))]
                 [xz (chunky-val chunky)])
            (chunk-set! chunk #:x (xz-x xz) #:z (xz-z xz) #:y (point-y point) #:block block))]))

(define (neighbors [val : XZ])
  (let ([x (xz-x val)]
        [z (xz-z val)])
    (list (xz (ufx+ 1 x) z)
          (xz (ufx+ -1 x) z)
          (xz x (ufx+ 1 z))
          (xz x (ufx+ -1 z)))))

(struct rect ([start : XZ]
              [end : XZ])
  #:type-name Rect #:transparent)

(define (rect-intersection [rects : (Listof Rect)])
  (when (empty? rects)
    ; Maybe it would make sense to return a zero-sized rect...?
    ; Wait until you have at least one motivating example.
    (error "rects cannot be empty"))
  (define-values (min-x max-x min-z max-z)
    (for/fold : (Values Fixnum Fixnum Fixnum Fixnum)
      ([min-x : Fixnum (xz-x (rect-start (first rects)))]
       [max-x : Fixnum (xz-x (rect-end (first rects)))]
       [min-z : Fixnum (xz-z (rect-start (first rects)))]
       [max-z : Fixnum (xz-z (rect-end (first rects)))])
      ([r (cdr rects)])
      (values (max min-x (xz-x (rect-start r)))
              (min max-x (xz-x (rect-end r)))
              (max min-z (xz-z (rect-start r)))
              (min max-z (xz-z (rect-end r))))))
  (rect (xz min-x min-z)
        (xz max-x max-z)))

(define-syntax-rule (in-rect/x rect)
  (ufx-in-range (xz-x (rect-start rect))
                (xz-x (rect-end rect))))
(define-syntax-rule (in-rect/z rect)
  (ufx-in-range (xz-z (rect-start rect))
                (xz-z (rect-end rect))))

(struct area ([bounds : Rect]
              [contains-func : (-> XZ Any)])
  #:transparent #:type-name Area)

(define-syntax-rule (for/area ([xz-id area-expr])
                      body ...)
  (let* ([area (ann area-expr Area)]
         [bounds (area-bounds area)])
    (for ([z : Fixnum (in-rect/z bounds)])
      (for ([x : Fixnum (in-rect/x bounds)])
        (let ([xz-id (xz x z)])
          (when (area-contains? area xz-id)
            body ...))))))

(define (area-contains? [area : Area] [xz : XZ])
  ((area-contains-func area) xz))

(define (area-intersection [areas : (Listof Area)])
  (define bounds (rect-intersection (map area-bounds areas)))
  ; Use Pairof here to make sure XZ and Point are handled correctly
  (define xzs (ann (make-hash) (Mutable-HashTable (Pairof Integer Integer) #t)))
  (for ([z : Fixnum (in-rect/z bounds)])
    (for ([x : Fixnum (in-rect/x bounds)])
      (when (andmap (lambda ([a : Area]) (area-contains? a (xz x z)))
                    areas)
        (hash-set! xzs (cons x z) #t))))
  (define (contains? [xz : XZ])
    (hash-ref xzs (cons (xz-x xz) (xz-z xz)) (lambda () #f)))
  (area bounds contains?))

(define (area-dimensions [area : Area])
  (let* ([bounds (area-bounds area)]
         [start (rect-start bounds)]
         [end (rect-end bounds)])
    (values (ufx- (xz-x end) (xz-x start))
            (ufx- (xz-z end) (xz-z start)))))

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
  (define width : Fixnum
    (let ([w (pict-width bmp)])
      (or (and (fixnum? w) (cast w Fixnum))
          (error "bad width" w))))
  (define depth : Fixnum
    (let ([h (pict-height bmp)])
      (or (and (fixnum? h) (cast h Fixnum))
          (error "bad height" h))))
  (define min-x : Fixnum (ufx+ 1 width))
  (define max-x : Fixnum -1)
  (define min-z : Fixnum (ufx+ 1 depth))
  (define max-z : Fixnum -1)
  (define pixels (pict->argb-pixels bmp))
  ; Use Pairof here to make sure XZ and Point are handled correctly
  (define xzs (ann (make-hash) (Mutable-HashTable (Pairof Integer Integer) #t)))
  (define all-empty? : Boolean #t)
  (define all-full? : Boolean #t)
  (let ([index 0])
    (for ([z : Fixnum (ufx-in-range depth)])
      (for ([x : Fixnum (ufx-in-range width)])
        (let ([alpha (bytes-ref pixels index)])
          (set! index (+ 4 index)) ; 4 bytes per pixel
          (cond
            [(> alpha 0)
             (set! all-empty? #f)
             (set! min-x (min min-x x))
             (set! max-x (max max-x x))
             (set! min-z (min min-z z))
             (set! max-z (max max-z z))
             (hash-set! xzs (cons x z) #t)]
            [else
             (set! all-full? #f)])))))
  (when (or all-empty? all-full?)
    (error (format "Expected some fully-transparent pixels and some other pixels, but ~a pixels are fully-transparent."
                   (if all-empty? "all" "zero"))))
  (area (rect (xz min-x min-z) (xz max-x max-z))
        (let ([set (list->set (hash-keys xzs))])
          (lambda ([xz : XZ])
            (set-member? set (cons (xz-x xz) (xz-z xz)))))))

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

(: bitmap->hill (-> (U (Instance Bitmap%) Path-String) Hill))
(define (bitmap->hill arg)
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
             (set! all-empty? #f)
             (hash-set! elevations (cons x z)
                        (max 0 (- 95 (quotient (max red green blue) 2))))]
            [else
             (set! all-full? #f)])))))
  (when (or all-empty? all-full?)
    (error (format "Expected some fully-transparent pixels and some other pixels, but ~a pixels are fully-transparent."
                   (if all-empty? "all" "zero"))))
  (define the-area
    (area (rect (xz 0 0) (xz width depth))
          (lambda ([xz : XZ])
            (hash-ref elevations (cons (xz-x xz) (xz-z xz)) (lambda () #f)))))
  (hill the-area (make-immutable-hash (hash->list elevations))))

; adjust-y is undocumented, not sure if it belongs here
(define (put-hill! [stage : Stage] [hill : Hill] [block : Integer] #:adjust-y [adjust-y : Fixnum 0])
  (define area (hill-area hill))
  (define elevations (hill-elevations hill))
  (define block-count : Fixnum 0)
  (define item-count : Fixnum 0)
  (for/area ([xz area])
    (let* ([end-y (hash-ref elevations (cons (xz-x xz) (xz-z xz)))]
           [end-y (ufx+ end-y adjust-y)])
      (for ([y : Fixnum (ufx-in-range 1 end-y)])
        (let* ([p (make-point xz y)]
               [cur (stage-read stage p)])
          (if (simple? (or cur 0))
              (begin (stage-write! stage p block)
                     (set! block-count (ufx+ 1 block-count)))
              (set! item-count (ufx+ 1 item-count)))))))
  (show-msg "put-hill! placed ~a blocks, left ~a items intact" block-count item-count))

(define-syntax-rule (dqb2-chunk-start-addr i)
  ; Returns the address of chunk i within the uncompressed buffer
  (ufx+ #x183FEF0 (ufx* i #x30000)))

(define (open-stgdat [kind : Stgdat-Kind] [path : Path])
  (define all-bytes (file->bytes path))
  (define header (subbytes all-bytes 0 header-length))
  (define compressed (subbytes all-bytes header-length (bytes-length all-bytes)))
  ; I'm guessing IoA probably always uncompresses to exactly 163,053,024 bytes (including the header),
  ; and we'll just add some room to spare just in case
  (define buffer-size #xA000000)
  (define buffer (zlib:uncompress compressed buffer-size))

  (define chunks
    (build-vector
     (case kind
       [(IoA) 369]
       [else (error "unexpected kind" kind)])
     (lambda ([i : Index])
       (define chunk (make-empty-chunk))
       (load-chunk! chunk buffer (dqb2-chunk-start-addr i))
       chunk)))
  (stage (stgdat-file kind path) header buffer (vector->immutable-vector chunks)))

(: load-stage (-> (U 'IoA) (U 'B00 'B01 'B02 Path-String) Stage))
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
                 [else (if (string? slot)
                           (string->path slot)
                           (ann slot Path))]))
  (open-stgdat kind path))

(: fill-area! (->* (Stage Area Integer #:y-max Fixnum)
                   (#:y-min Fixnum)
                   Void))
(define (fill-area! stage area block #:y-max y-max #:y-min [y-min 1])
  (for/area ([xz area])
    (for ([y : Fixnum (ufx-in-range y-min (ufx+ 1 y-max))])
      (let* ([p (make-point xz y)])
        (or (stage-write! stage p block)
            (error "TODO out of range:" p)))))
  (void))

(define (save-stage! [stage : Stage])
  (define orig-file (stgdat-file-path (stage-loaded-from stage)))
  (define orig-dir (or (path-only orig-file)
                       (error "assert fail: path-only failed for:" orig-file)))
  (assert-directory-writable orig-dir)
  ; == Now it is safe to write ==
  (define header (stage-header stage))
  (define buffer (stage-buffer stage))
  (define chunks (stage-chunks stage))
  (for ([i (in-range (vector-length chunks))])
    (let ([chunk (vector-ref chunks i)])
      (unload-chunk! chunk buffer (dqb2-chunk-start-addr i))))
  (define compressed (zlib:compress buffer))
  (with-output-to-file orig-file #:exists 'truncate
    (lambda ()
      (write-bytes header)
      (write-bytes compressed)))
  (show-msg "Saved STGDAT file: ~a" orig-file))

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
  (for ([z : Fixnum (in-rect/z bounds)])
    (for ([x : Fixnum (in-rect/x bounds)])
      (let* ([xz (xz x z)])
        (when (outskirts? xz)
          (set! result (cons xz result))))))
  result)

(define (print-column [stage : Stage] [xz : XZ])
  (for ([y (in-range 96)])
    (let* ([p (make-point xz y)]
           [val (stage-read stage p)])
      (println (list "y:" y "block:" val)))))

(define (stage-full-area [kind : Stgdat-Kind])
  (let* ([chunk-layout (get-chunk-layout kind)]
         [depth (ufx* 32 (vector-length chunk-layout))]
         [width (ufx* 32 (vector-length (vector-ref chunk-layout 0)))]
         [bounds (rect (xz 0 0) (xz width depth))])
    (define (contains? [xz : XZ])
      (chunk-translate chunk-layout xz))
    (area bounds contains?)))

(define-syntax-rule (get-area x stage)
  (case x
    [(all) (stage-full-area (stage-kind stage))]
    [else (ann x Area)]))

(define (repair-sea! [stage : Stage] [where : (U 'all Area)] #:sea-level [sea-level : (U #f Fixnum) #f])
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
  (define water-level : Fixnum (or sea-level
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
  (for/area ([xz area])
    (for ([y : Fixnum (ufx-in-range (ufx+ 1 water-level))])
      (let ([p (make-point xz y)])
        (when (vacant? p)
          (stage-write! stage p (if (ufx= y water-level)
                                    top-sea
                                    full-sea))))))
  (void))

(define (item-count [stage : Stage])
  (define buffer (stage-buffer stage))
  (define a (bytes-ref buffer #x24E7CD))
  (define b (bytes-ref buffer #x24E7CE))
  (define c (bytes-ref buffer #x24E7CF))
  (:ufxior (ufxlshift a 0)
           (ufxlshift b 8)
           (ufxlshift c 16)))

(define (clear-area! [stage : Stage] [where : (U 'all Area)]
                     #:y-min [min-y : Fixnum 1]
                     #:keep-items? [keep-items? : Boolean #t])
  ; Reset count of 24-byte records to zero
  ; (this is probably a 4-byte number but 0xC8000 is the max)
  (when (not keep-items?)
    (define buffer (stage-buffer stage))
    (bytes-set! buffer #x24E7CD 1) ; Setting to zero ruins the first item you place??
    (bytes-set! buffer #x24E7CE 0)
    (bytes-set! buffer #x24E7CF 0))
  (define area (get-area where stage))
  (for/area ([xz area])
    (for ([y : Fixnum (ufx-in-range min-y 96)])
      (let ([p (make-point xz y)])
        (when (and (>= y min-y)
                   (or (not keep-items?)
                       (simple-block? (or (stage-read stage p) 0))))
          (stage-write! stage p 0)))))
  (void))

(define (blocks-hash [stage : Stage]
                     #:where [where : (U 'all Area) 'all])
  ; For use by automated tests, to avoid adding too many large files into git.
  ; If the hash changes, you might need to use an older version of the code
  ; to export the complete data for diffing.
  (define area (get-area where stage))
  (define hash1 0)
  (define hash2 0)
  (for/area ([xz area])
    (for ([y (in-range 96)])
      (let* ([p (make-point xz y)]
             [block (or (stage-read stage p)
                        (error "assert fail"))])
        ; I think a hash collision would be very unlikely, but I can't prove it.
        ; Using two different hashes seems like it would be much more resistant
        ; to any surprising block patterns that might thwart one of the hashes.
        (set! hash1 (bitwise-and #xFFFFFF (+ block (* hash1 31))))
        (set! hash2 (bitwise-and #xFFFFFF (+ block (* hash2 17)))))))
  (list hash1 hash2))

; TODO here is the faster iteration logic with chunk skipping.
; Factor this out into a generic iteration mechanism.
(define (stage->pict [stage : Stage] [colorizers : (HashTable Fixnum Integer)])
  ; colorizers maps block IDs to argb values
  (define all-block-ids : (Listof Fixnum)
    (hash-keys colorizers))
  (define (get-argb [block : Fixnum])
    (hash-ref colorizers block (lambda () #f)))
  (define area (stage-full-area (stage-kind stage)))
  (define-values (width depth) (area-dimensions area))
  (define bytes-per-pixel 4)
  (define pict-bytes (make-bytes (* bytes-per-pixel width depth)))
  (define chunk-layout (get-chunk-layout (stage-kind stage)))
  (define chunks-per-row (ufxquotient width 32))
  (define row-of-chunks (ann (make-vector chunks-per-row #f)
                             (Mutable-Vectorof (U #f Chunk))))
  (let loop ([x : Fixnum 0]
             [z : Fixnum 0])
    (cond
      [(ufx= x width)
       (loop 0 (ufx+ 1 z))]
      [(ufx= z depth)
       (void "done")]
      [else
       (when (ufx= 0 (ufxmodulo z 32))
         ; Load next row of chunks.
         ; If the chunk doesn't contain at least one of the block IDs we are looking for,
         ; replace it with #f so we can quickly skip it every time
         (for ([offset : Fixnum (ufx-in-range chunks-per-row)])
           (let* ([chunky (chunk-translate chunk-layout (xz (ufx* offset 32) z))]
                  [chunk : (U #f Chunk)
                         (and chunky
                              (vector-ref (stage-chunks stage) (chunky-chunk-id chunky)))])
             (when chunk
               (define (has? [block : Fixnum])
                 (ufx< 0 (chunk-countof (or chunk (error "assert fail")) block)))
               (when (not (ormap has? all-block-ids))
                 (set! chunk #f)))
             (vector-set! row-of-chunks offset chunk))))
       #;(println (list "xz" x z))
       (let ([chunk (vector-ref row-of-chunks (ufxquotient x 32))])
         (cond
           [(not chunk)
            ; advance x to next chunk
            (loop (ufx+ 32 x) z)]
           [else
            (let* ([chunky (or (chunk-translate chunk-layout (xz x z))
                               (error "assert fail - chunk-layout should prevent us from doing this"))]
                   ; TODO I accidentally shadowed my global xz...
                   ; Maybe chunk-ref should accept a (Chunky XZ) instead?
                   #;[x (xz-x (chunky-val chunky))]
                   #;[z (xz-z (chunky-val chunky))]
                   [argb (ormap get-argb (for/list : (Listof Fixnum)
                                           ([y : Fixnum (ufx-in-range 96)])
                                           (chunk-ref chunk
                                                      #:x (xz-x (chunky-val chunky))
                                                      #:z (xz-z (chunky-val chunky))
                                                      #:y y)))])
              (when argb
                (let ([index (ufx* bytes-per-pixel (ufx+ x (ufx* z width)))])
                  (bytes-set! pict-bytes (ufx+ 0 index) (bitwise-bit-field argb 24 32))
                  (bytes-set! pict-bytes (ufx+ 1 index) (bitwise-bit-field argb 16 24))
                  (bytes-set! pict-bytes (ufx+ 2 index) (bitwise-bit-field argb 08 16))
                  (bytes-set! pict-bytes (ufx+ 3 index) (bitwise-bit-field argb 00 08))))
              (loop (ufx+ 1 x) z))]))]))
  ; loop done, return
  (argb-pixels->pict pict-bytes (cast width Nonnegative-Integer)))

; Keeping this for the destroy-everything project. Passes the whole column back to the caller
(define (stage->pictOLD [stage : Stage]
                        [argb-callback : (-> XZ (Mutable-Vectorof Integer) Integer)])
  (define area (stage-full-area (stage-kind stage)))
  (define-values (width depth) (area-dimensions area))
  (define pict-bytes (make-bytes (* 4 width depth))) ; 4 bytes per pixel
  (define index : Integer 0)
  (define-syntax-rule (++! i) (let ([val i])
                                (set! i (+ 1 i))
                                val))
  (define column (ann (make-vector 96) (Mutable-Vectorof Integer)))
  (for ([z : Fixnum (ufx-in-range depth)])
    (for ([x : Fixnum (ufx-in-range width)])
      (for ([y : Fixnum (ufx-in-range 96)])
        (let ([block (stage-read stage (make-point (xz x z) y))])
          (vector-set! column y (or block 0))))
      (let ([argb (argb-callback (xz x z) column)])
        (bytes-set! pict-bytes (++! index) (bitwise-bit-field argb 24 32))
        (bytes-set! pict-bytes (++! index) (bitwise-bit-field argb 16 24))
        (bytes-set! pict-bytes (++! index) (bitwise-bit-field argb 08 16))
        (bytes-set! pict-bytes (++! index) (bitwise-bit-field argb 00 08)))))
  (argb-pixels->pict pict-bytes (cast width Nonnegative-Integer)))


; Anywhere `peak-block` occurs in the given area, fill that column up to that peak
; with the `fill-block`.
; Is this proc too specific to my use case?
; Maybe I should (provide stage-read stage-write! for/area) and let user code do this:
(define (TODO [stage : Stage] [area : Area] [peak-block : Fixnum] [fill-block : Integer])
  (for/area ([xz area])
    (define peak : Fixnum -1)
    (for ([y : Fixnum (ufx-in-range 95 -1 -1)])
      (when (ufx= peak-block (or (stage-read stage (make-point xz y)) 0))
        (set! peak y)))
    (when peak
      (for ([y : Fixnum (ufx-in-range 1 peak)])
        (stage-write! stage (make-point xz y) fill-block))))
  (void))

; For me, probably irrelevant for the world at large:
(define (create-golem-platforms! [stage : Stage] [area : Area] [block : Integer])
  (for/area ([xz area])
    (for ([y : Fixnum '(30 40 50 60 70 80 90)])
      (let ([p (make-point xz y)])
        (when (= 0 (or (stage-read stage p) 1))
          (stage-write! stage p block)))))
  (void))

; Copying items can cause a CMNDAT-STGDAT mismatch.
; So it's safer to copy everything, and then remove/overwrite what you don't want.
; (I think the mismatch only happens when you add new storage. If you delete
;  existing storage from the blockdata, it seems the orphaned CMNDAT data is
;  automatically cleaned up. Should confirm this.)
(define (copy-all-save-files! #:from [from : (U 'B00 'B01 'B02)] #:to [to : (U 'B00 'B01 'B02)])
  (define sd (or (save-dir)
                 (error "You must parameterize `save-dir`")))
  (define to-dir : Path
    (build-path sd (~a to)))
  (assert-directory-writable to-dir)
  ; == Now it is safe to write ==
  ; We definitely don't want to copy *every* file.
  ; For example, copying the config file would be a disaster!
  ; I might be missing some files here:
  (define known-files '(AUTOCMNDAT.BIN
                        AUTOSTGDAT.BIN
                        CMNDAT.BIN
                        SCSHDAT.BIN
                        STGDAT01.BIN
                        STGDAT02.BIN
                        STGDAT03.BIN
                        STGDAT04.BIN
                        STGDAT05.BIN
                        STGDAT06.BIN
                        STGDAT07.BIN
                        STGDAT08.BIN
                        STGDAT09.BIN
                        STGDAT10.BIN
                        STGDAT11.BIN
                        STGDAT12.BIN
                        STGDAT13.BIN
                        STGDAT14.BIN
                        STGDAT15.BIN
                        STGDAT16.BIN))
  (define from-dir : Path
    (build-path sd (~a from)))
  (define count 0)
  (for ([file known-files])
    (define from-path (build-path from-dir (~a file)))
    (when (file-exists? from-path)
      (define to-path (build-path to-dir (~a file)))
      (copy-file from-path to-path #:exists-ok? #t)
      (set! count (add1 count))))
  (show-msg "Copied ~a files from ~a to ~a" count from-dir to-dir))

(: decorate-peaks! (-> Stage Area (-> XZ Fixnum Fixnum) Void))
(define (decorate-peaks! stage area callback)
  (for/area ([xz area])
    (let loop ([y : Fixnum 95])
      (let* ([pt (make-point xz y)]
             [existing (stage-read stage pt)]
             [below (stage-read stage (make-point xz (ufx+ y -1)))])
        (cond
          [(or (not below) (not existing)) ; entire column is off the map
           (void)]
          [(not (ufx= 0 below))
           (when (ufx= 0 existing)
             (let* ([new (callback xz below)])
               (stage-write! stage pt new)))]
          [(= 1 y)
           (void)]
          [else
           (loop (ufx+ y -1))]))))
  (void))

(: add-chunk-ids! (-> Stage Void))
; For hacking/investigation
(define (add-chunk-ids! stage)
  (define (getblock [chunk-id : Fixnum])
    (case (ufxmodulo chunk-id 7)
      [(0) (block 'Light-Dolomite)]
      [(1) (block 'Dark-Dolomite)]
      [(2) (block 'Stony-Soil)]
      [(3) (block 'Seaside-Sand)]
      [(4) (block 'Arid-Earth)]
      [(5) (block 'Chert)]
      [(6) (block 'Umber)]
      [else (error "assert fail")]))
  (define chunks (stage-chunks stage))
  (define layout (get-chunk-layout (stage-kind stage)))
  (for ([row layout])
    (for ([chunk-id row])
      (when (fixnum? chunk-id)
        (let ([chunk (vector-ref chunks chunk-id)]
              [y : Fixnum 50])
          (for ([x : Fixnum (ufx-in-range 32)])
            (for ([z : Fixnum (ufx-in-range 32)])
              (chunk-set! chunk #:x x #:z z #:y y #:block (getblock chunk-id))))))))
  (void))

(: find-block-name (-> (U String Symbol) Void))
(define (find-block-name name)
  (define results (find-block (~a name) 'auto))
  (define exact (first results))
  (define others (second results))
  (cond
    [(pair? exact)
     (displayln "Exact Matches:")
     (for ([sym exact])
       (displayln (format "  (block '~a)" sym)))]
    [(pair? others)
     (displayln "Possible Matches:")
     (for ([item others])
       (displayln (format "  (block '~a)" (first item))))]
    [else
     (displayln (format "No matches found for ~a" name))])
  (void))

; SQLite is super fast (even without indexes!) once the data has been loaded,
; but the following code takes about 3 minutes to load my IoA.
; It loads almost 13M records, so that's ~67k inserts per second which seems
; almost as fast as SQLite can go. So maybe not the silver bullet I expected.
; (Ooh, or maybe I actually want a daemon to watch my save directory for
;  changes and load the data. It could automatically create backups too.)
; Anyway... if I want to use pattern matching from Racket I would have to bypass
; SQLite anyway so let's hold off for now.
#;{module+ sqlite
    (provide stage->db)

    (require typed/db)

    (define (stage->db [stage : Stage])
      (define conn (sqlite3-connect #:database 'memory))
      (query-exec conn "pragma journal_mode = OFF")
      (query-exec conn "PRAGMA synchronous = OFF")
      (query-exec conn "create table cell (x int, y int, z int, masked_block int, block int)")
      (define inserter (prepare conn "insert into cell(x,y,z,masked_block,block) values(?,?,?,?,?)"))
      (query-exec conn "begin transaction")
      (for/area ([xz (get-area 'all stage)])
        (let ([x (xz-x xz)]
              [z (xz-z xz)])
          (when (= 400 x)
            #;(query-exec conn "end transaction")
            #;(query-exec conn "begin transaction")
            (println (list "Z" z)))
          (for ([y (in-range 96)])
            (let ([block (or (stage-read stage (make-point xz y)) 0)])
              (when (not (= block 0))
                (let ([masked-block (bitwise-and block #x7FF)])
                  (query-exec conn inserter ; "insert into cell(x,y,z,masked_block,block) values(?,?,?,?,?)"
                              x y z masked-block block)))))))
      (query-exec conn "end transaction")
      conn)
    }
