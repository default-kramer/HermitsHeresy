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
         stage->pict
         create-golem-platforms!
         copy-all-save-files!
         save-stage!
         find-block-name
         protect-area!
         traverse
         traverse-lambda
         )

(module+ everything
  (provide stage-buffer
           add-chunk-ids!))

(module+ for-testing
  (provide blocks-hash hill-ref make-rect area-bounds xz)

  (define (hill-ref [hill : Hill] [loc : (Pairof Fixnum Fixnum)])
    (define locxz (xz (car loc) (cdr loc)))
    (and (area-contains? (hill-area hill) locxz)
         (hash-ref (hill-elevations hill) loc))))

(require (prefix-in t: "traversal/traversal.rkt")
         "block.rkt"
         "chunk.rkt"
         "chunky-area.rkt"
         "area.rkt"
         "hill.rkt"
         "basics.rkt"
         "ufx.rkt"
         "config-util.rkt"
         "blockdata/blockdef.rkt"
         "stage.rkt"
         typed/pict
         typed/racket/unsafe
         (only-in typed/racket/draw Bitmap%))

(require/typed racket
               [copy-file (->* (Path-String Path-String)
                               (#:exists-ok? Any)
                               Any)])

(unsafe-require/typed (prefix-in ut: "traversal/untyped-traversal.rkt")
                      [ut:in-area-vector (Parameterof Any)]
                      [ut:in-area-index-assigner (Parameterof Any)])

{module+ test
  (require typed/rackunit)
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

(define (neighbors [val : XZ])
  (let ([x (xz-x val)]
        [z (xz-z val)])
    (list (xz (ufx+ 1 x) z)
          (xz (ufx+ -1 x) z)
          (xz x (ufx+ 1 z))
          (xz x (ufx+ -1 z)))))


(: bitmap->area (-> (U (Instance Bitmap%) Path-String) Chunky-Area))
(define (bitmap->area arg)
  (bitmap->chunky-area arg))

; adjust-y is undocumented, not sure if it belongs here
(define (put-hill! [stage : Stage] [hill : Hill] [block : Integer] #:adjust-y [adjust-y : Fixnum 0])
  (define area (hill-area hill))
  (define elevations (hill-elevations hill))
  (define block-count : Fixnum 0)
  (define item-count : Fixnum 0)
  (define protected-area (unbox (stage-protected-area stage)))
  (for/area ([xz area])
    (define proof (unprotected? protected-area xz))
    (when proof
      (let* ([end-y (hash-ref elevations (cons (xz-x xz) (xz-z xz)))]
             ; Are we missing a +1 here?
             ; I don't think so, because y is 0-based so when hill-elevation is 96
             ; we will fill up to 95 which is correct... I think.
             [end-y (ufx+ end-y adjust-y)])
        (for ([y : Fixnum (ufx-in-range 1 end-y)])
          (let* ([p (make-point xz y)]
                 [cur (stage-read stage p)])
            (if (simple? (or cur 0))
                (begin (stage-write! stage proof p block)
                       (set! block-count (ufx+ 1 block-count)))
                (set! item-count (ufx+ 1 item-count))))))))
  (show-msg "put-hill! placed ~a blocks, left ~a items intact" block-count item-count))



(: load-stage (-> Stgdat-Kind (U 'B00 'B01 'B02 Path-String) Stage))
(define (load-stage kind slot)
  (define path (case slot
                 [(B00 B01 B02)
                  (let ([sd (save-dir)]
                        [filename (kind->filename kind)])
                    (if sd
                        (build-path sd (~a slot) filename)
                        (error "You must parameterize `save-dir` to load:" slot)))]
                 [else (if (string? slot)
                           (string->path slot)
                           (ann slot Path))]))
  (define stgdat (open-stgdat kind path))
  (show-msg "loaded stage: ~a" path)
  stgdat)

(: fill-area! (->* (Stage Area Integer #:y-max Fixnum)
                   (#:y-min Fixnum)
                   Void))
(define (fill-area! stage area block #:y-max y-max #:y-min [y-min 1])
  (define protected-area (unbox (stage-protected-area stage)))
  (for/area ([xz area])
    (define proof (unprotected? protected-area xz))
    (when proof
      (for ([y : Fixnum (ufx-in-range y-min (ufx+ 1 y-max))])
        (let* ([p (make-point xz y)])
          (or (stage-write! stage proof p block)
              (error "TODO out of range:" p))))))
  (void))

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
  (error "TODO: redo as traversal")
  #;(begin
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
      (define protected-area (unbox (stage-protected-area stage)))
      (for/area ([xz area])
        (define proof (unprotected? protected-area xz))
        (when proof
          (for ([y : Fixnum (ufx-in-range (ufx+ 1 water-level))])
            (let ([p (make-point xz y)])
              (when (vacant? p)
                (stage-write! stage proof p (if (ufx= y water-level)
                                                top-sea
                                                full-sea)))))))
      (void)))

(define (clear-area! [stage : Stage] [where : (U 'all Area)]
                     #:y-min [min-y : Fixnum 1]
                     #:keep-items? [keep-items? : Boolean #t])
  (error "TODO remove or redo as traversal")
  #;(begin
      ; Reset count of 24-byte records to zero
      ; (this is probably a 4-byte number but 0xC8000 is the max)
      (when (not keep-items?)
        (define buffer (stage-buffer stage))
        (bytes-set! buffer #x24E7CD 1) ; Setting to zero ruins the first item you place??
        (bytes-set! buffer #x24E7CE 0)
        (bytes-set! buffer #x24E7CF 0))
      (define area (get-area where stage))
      (define protected-area (unbox (stage-protected-area stage)))
      (for/area ([xz area])
        (define proof (unprotected? protected-area xz))
        (when proof
          (for ([y : Fixnum (ufx-in-range min-y 96)])
            (let ([p (make-point xz y)])
              (when (and (>= y min-y)
                         (or (not keep-items?)
                             (simple-block? (or (stage-read stage p) 0))))
                (stage-write! stage proof p 0))))))
      (void)))

(define (blocks-hash [stage : Stage]
                     #:where [where : (U 'all #;Area) 'all])
  ; For use by automated tests, to avoid adding too many large files into git.
  ; If the hash changes, you might need to use an older version of the code
  ; to export the complete data for diffing.
  (define area
    (and (eq? where 'all)
         (chunk-layout->chunky-area (stage-chunk-layout stage))))
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
  (define chunk-layout (stage-chunk-layout stage))
  (define width (ufx* 32 (vector-length (vector-ref chunk-layout 0))))
  (define depth (ufx* 32 (vector-length chunk-layout)))
  (define bytes-per-pixel 4)
  (define pict-bytes (make-bytes (* bytes-per-pixel width depth)))
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

; For me, probably irrelevant for the world at large:
(define (create-golem-platforms! [stage : Stage] [area : Area] [block : Integer])
  (define protected-area (unbox (stage-protected-area stage)))
  (for/area ([xz area])
    (define proof (unprotected? protected-area xz))
    (when proof
      (for ([y : Fixnum '(30 40 50 60 70 80 90)])
        (let ([p (make-point xz y)])
          (when (= 0 (or (stage-read stage p) 1))
            (stage-write! stage proof p block))))))
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
  ; Also, SCSHDAT.BIN should never be modified from outside the game.
  (define known-files '(AUTOCMNDAT.BIN
                        AUTOSTGDAT.BIN
                        CMNDAT.BIN
                        STGDAT01.BIN ; IoA
                        STGDAT02.BIN ; Furrowfield
                        STGDAT03.BIN ; Khrumbul-Dun
                        STGDAT04.BIN ; Moonbrooke
                        STGDAT05.BIN ; Malhalla
                        STGDAT06.BIN ; ?? Malhalla Final Battle ??
                        STGDAT07.BIN ; broken?
                        STGDAT08.BIN ; broken?
                        STGDAT09.BIN ; Angler's Isle
                        STGDAT10.BIN ; Skelkatraz
                        STGDAT11.BIN ; broken?
                        STGDAT12.BIN ; BT1
                        STGDAT13.BIN ; BT2
                        STGDAT14.BIN ; Battle Atoll, debug only
                        STGDAT15.BIN ; ?? Tutorial ship ??
                        STGDAT16.BIN ; BT3
                        ))
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
  (define protected-area (unbox (stage-protected-area stage)))
  (for/area ([xz area])
    (define proof (unprotected? protected-area xz))
    (when proof
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
                 (stage-write! stage proof pt new)))]
            [(= 1 y)
             (void)]
            [else
             (loop (ufx+ y -1))])))))
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
  (define layout (stage-chunk-layout stage))
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

(: traverse (-> Stage t:Traversal
                [#:force-unsound-optimization? Boolean]
                [#:respect-bedrock? Boolean]
                Any))
(define (traverse stage trav
                  #:force-unsound-optimization? [optimize? #f]
                  #:respect-bedrock? [respect-bedrock? #t])
  ; In the future, I should be able to analyze the code and determine the areas that
  ; are relevant to optimization. For now, I'll just add this secret optional arg
  ; which allows the caller to say "you can skip any XZ that is not contained by
  ; any of the areas appearing in this traversal."
  (define cannot-optimize? : Boolean (not optimize?))

  (define start-y : Fixnum (if respect-bedrock? 1 0))

  (define areas (filter chunky-area? (t:traversal-areas trav)))
  (define hills (filter hill? (t:traversal-areas trav)))
  ; It's important that hills come before areas here. Read on...
  (define hills-and-areas (append hills areas))

  ; This vector will hold the boolean status for each area and hill.
  ; This is what the untyped code will read from.
  ; The areas can be tested once per xz, but hills need to test
  ; the Y coordinate also (only when potentially inside the hill).
  (define area-contains-vec
    (ann (make-vector (length hills-and-areas) #f)
         (Mutable-Vectorof Boolean)))

  ; Once per xz, we will update this vector to hold the elevation of each hill.
  ; This vector is not shared with untyped code; we use it internally to test
  ; the Y coordinate and update the area-contains-vec for each hill.
  ; The index of the hills should match in both of these vectors.
  ; That's why we sorted the hills to come before areas.
  (define hill-elevation-vec
    (ann (make-vector (length hills) 0)
         (Mutable-Vectorof Fixnum)))

  ; Assigns a zero-based index to each of the areas in the traversal.
  ; This should be lifted so it only gets called once per area before
  ; the traversal starts.
  (define (traversal-area->index area)
    (: go (-> Integer (Listof Any) Integer))
    (define (go i areas)
      (cond
        [(empty? areas) (error "assert fail: cannot assign key to" area)]
        [(eq? area (car areas)) i]
        [(go (+ 1 i) (cdr areas))]))
    (go 0 hills-and-areas))

  (define full-area (chunk-layout->chunky-area (stage-chunk-layout stage)))

  (define (check-bedrock [xz : XZ])
    (or (not respect-bedrock?)
        (ufx= 1 (or (stage-read stage (make-point xz 0)) -1))))

  (parameterize ([ut:in-area-index-assigner traversal-area->index]
                 [ut:in-area-vector area-contains-vec])
    (define args (t:make-empty-argbox))
    (define callback ((t:traversal-callback-maker trav) args))
    (when (impersonator? callback)
      ; Slowdown observed from 11s to 13s on the command line,
      ; and it gets worse in DrRacket. So this is worth it IMO.
      (error "assert fail: callback is an impersonator"))
    (define protected-area (unbox (stage-protected-area stage)))
    (for/area ([xz full-area])
      (define proof (and (check-bedrock xz)
                         (unprotected? protected-area xz)))
      (when proof

        ; Update the area-contains? statuses for each area
        (define in-any-area? : Boolean #f)
        (let ([i : Fixnum 0])
          (for ([hill hills])
            (let* ([key (cons (xz-x xz) (xz-z xz))]
                   [elevation (hash-ref (hill-elevations hill) key #f)])
              ; Setting elevation to -1 means we will never be inside it
              (vector-set! hill-elevation-vec i (or elevation -1))
              (set! in-any-area? (or in-any-area? (and elevation #t)))
              (set! i (ufx+ 1 i))))
          (for ([area areas])
            (let ([result (chunky-area-contains? area xz)])
              (vector-set! area-contains-vec i result)
              (set! in-any-area? (or in-any-area? result))
              (set! i (ufx+ 1 i)))))

        (when (or cannot-optimize? in-any-area?)
          (define-values (x z) (xz->values xz))
          (t:set-argbox-x! args x)
          (t:set-argbox-z! args z)
          (for ([y : Fixnum (ufx-in-range start-y 96)])
            (let ([i : Fixnum 0])
              (for ([hill hills])
                (let ([elev (vector-ref hill-elevation-vec i)])
                  ; y is zero-based, elevation is 1-based, I think?? Confirm
                  (vector-set! area-contains-vec i (ufx< y elev)))
                (set! i (ufx+ 1 i))))
            (define p (make-point xz y))
            (define block : Fixnum
              (or (stage-read stage p)
                  (error "TODO is this possible?
If so, just do an area-intersect with the stage full area, right?")))
            (t:set-argbox-y! args y)
            (t:set-argbox-block! args block)
            (callback)
            (stage-write! stage proof p (t:argbox-block args))
            ))))
    (show-msg "traverse ignored ~a attempts to overwrite an item"
              (t:argbox-skipped-item-count args))
    (void)))

(: traverse-lambda (-> Stage (-> t:Argbox Any) Any))
(define (traverse-lambda stage callback)
  (define args (t:make-empty-argbox))
  (define protected-area (unbox (stage-protected-area stage)))
  (define full-area (chunk-layout->chunky-area (stage-chunk-layout stage)))
  (for/area ([xz full-area])
    (define proof (unprotected? protected-area xz))
    (when proof
      (t:set-argbox-x! args (xz-x xz))
      (t:set-argbox-z! args (xz-z xz))
      (for ([y : Fixnum (ufx-in-range 96)])
        (define p (make-point xz y))
        (define block : Fixnum
          (or (stage-read stage p)
              (error "TODO is this possible?
If so, just do an area-intersect with the stage full area, right?")))
        (t:set-argbox-y! args y)
        (t:set-argbox-block! args block)
        (callback args)
        (stage-write! stage proof p (t:argbox-block args))
        )))
  (show-msg "traverse ignored ~a attempts to overwrite an item"
            (t:argbox-skipped-item-count args))
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
