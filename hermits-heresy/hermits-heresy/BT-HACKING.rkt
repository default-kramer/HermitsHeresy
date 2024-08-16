#lang typed/racket

(module+ for-testing
  (provide get-layout print-runs))

(require "chunk.rkt"
         "basics.rkt"
         "ufx.rkt"
         typed/pict
         (prefix-in zlib: "zlib.rkt"))

(define-type Pict pict)

(define length/any (ann length (-> (Listof Any) Index))) ; is this type just better?

; BEGIN DUPLICATE CODE

(define-syntax-rule (dqb2-chunk-start-addr i)
  ; Returns the address of chunk i within the uncompressed buffer
  (ufx+ #x183FEF0 (ufx* i #x30000)))

(define header-length #x110)

(define (open-stgdat [path : Path])
  (define all-bytes (file->bytes path))
  (define header (subbytes all-bytes 0 header-length))
  (define compressed (subbytes all-bytes header-length (bytes-length all-bytes)))
  ; I'm guessing IoA probably always uncompresses to exactly 163,053,024 bytes (including the header),
  ; and we'll just add some room to spare just in case
  (define buffer-size #xA000000)
  (define buffer (zlib:uncompress compressed buffer-size))
  ;(define buffer-length (bytes-length buffer))
  buffer)

; END DUPLICATE CODE

(: get-bedrock-chunks (-> Path-String (Listof Chunk)))
(define (get-bedrock-chunks pathstr)
  (define path : Path
    (if (string? pathstr)
        (string->path pathstr)
        (ann pathstr Path)))
  (define buffer (open-stgdat path))
  (let loop ([chunks : (Listof Chunk) (list)]
             [i : Fixnum 0])
    (define start-addr (dqb2-chunk-start-addr i))
    (define end-addr (ufx+ start-addr (:ufx* 2 32 32))) ; just read y=0
    (define chunk (make-empty-chunk))
    (define block-count (load-chunk! chunk buffer (dqb2-chunk-start-addr i) end-addr))
    (if (ufx= 0 block-count) ; empty chunk means we are done
        (reverse chunks)
        (loop (cons chunk chunks)
              (ufx+ 1 i)))))

(: chunk->pict (-> Chunk Pict))
(define (chunk->pict chunk)
  (define bytes-per-pixel 4)
  (define argb #xFF000000)
  (define pict-bytes (make-bytes (* bytes-per-pixel 32 32)))
  (for ([x : Fixnum (ufx-in-range 32)])
    (for ([z : Fixnum (ufx-in-range 32)])
      (let ([data (chunk-ref chunk #:x x #:z z #:y 0)])
        (when (not (= 0 data))
          (let ([index (ufx* bytes-per-pixel (ufx+ x (ufx* z 32)))])
            (bytes-set! pict-bytes (ufx+ 0 index) (bitwise-bit-field argb 24 32))
            (bytes-set! pict-bytes (ufx+ 1 index) (bitwise-bit-field argb 16 24))
            (bytes-set! pict-bytes (ufx+ 2 index) (bitwise-bit-field argb 08 16))
            (bytes-set! pict-bytes (ufx+ 3 index) (bitwise-bit-field argb 00 08)))))))
  (argb-pixels->pict pict-bytes 32))

#;(define-type Chunk-Layout (Vectorof (Vectorof (U #f Integer))))

(define list-of-0 (make-list 32 (ann 0 Fixnum)))
(define list-of-31 (make-list 32 (ann 31 Fixnum)))

; Represents one side of a chunk. The list will have 32 items, where 'X means
; bedrock (or any block) is present and #f means it's vacant.
(define-type Profile (Listof (U 'X #f)))

(: side-profile (-> Chunk (U 'top 'right 'bottom 'left) Profile))
(define (side-profile chunk side)
  (define-syntax-rule (go x-range z-range)
    (for/list : Profile
      ([x : Fixnum x-range]
       [z : Fixnum z-range])
      (let ([block (chunk-ref chunk #:x x #:z z #:y 0)])
        (if (ufx= 0 block)
            #f
            'X))))
  (case side
    [(top) (go (ufx-in-range 32) list-of-0)]
    [(bottom) (go (ufx-in-range 32) list-of-31)]
    [(left) (go list-of-0 (ufx-in-range 32))]
    [(right) (go list-of-31 (ufx-in-range 32))]))

(define (profile-count [profile : Profile])
  (length (filter identity profile)))

(: profile-intersect (-> Profile Profile Profile))
(define (profile-intersect aa bb)
  (for/list : Profile ([a aa]
                       [b bb])
    (and a b)))

(: split-into-runs (-> (Listof Chunk) (Listof (Listof Chunk))))
(define (split-into-runs chunks)
  (let loop ([chunks : (Listof Chunk) chunks]
             [runs : (Listof (Listof Chunk)) (list)]
             [current-run : (Listof Chunk) (list)])
    (cond
      [(empty? chunks)
       (if (empty? current-run)
           (reverse runs)
           (reverse (cons (reverse current-run) runs)))]
      [(empty? current-run)
       (define chunk (first chunks))
       (loop (cdr chunks) runs (list chunk))]
      [else
       (define chunk (first chunks))
       (define my-left (side-profile chunk 'left))
       (define prev-right (side-profile (first current-run) 'right))
       (define max-score (max (profile-count my-left)
                              (profile-count prev-right)))
       (define score (profile-count (profile-intersect my-left prev-right)))
       (define jaggedness (- max-score score))
       (define start-new-run?
         (cond
           [(= score 0) #t]
           [(> jaggedness 2) #t]
           [else #f]))
       (if start-new-run?
           (loop (cdr chunks)
                 (cons (reverse current-run) runs)
                 (list chunk))
           (loop (cdr chunks)
                 runs
                 (cons chunk current-run)))])))

; Maybe the rule is if two runs can fit into a row then they always do?
; Maybe I should actually model a run as allowing lack of a chunk in a certain space...
; No, let's keep "run" to mean the same thing and introduce a new type "row"
(define-type Row (Listof (U #f Chunk)))

(: run->row-possibilities (-> (Listof Chunk) Integer (Listof Row)))
(define (run->row-possibilities run max-width)
  (define shortness (- max-width (length run)))
  (for/list : (Listof Row)
    ([i (in-range (+ 1 shortness))])
    (let ([front (make-list i #f)]
          [back (make-list (- shortness i) #f)])
      (ann (append front run back) Row))))

(define (show-row [row : Row])
  (map (lambda (item) (if item 'X '_)) row))

(define (calc-chunk-layout [chunks : (Listof Chunk)])
  (define runs (split-into-runs chunks))
  (define max-width (apply max (map length/any runs)))
  
  ; WARNING - THIS ALGORITHM IS INCOMPLETE
  ; Remember this is possible!
  #;(define small-coral-cay : Chunk-Layout
      (parse-map '((X X X _ X X X)
                   (X X X X X X X)
                   (X X X X X X X)
                   (X X X X X X X)
                   (X X X X X X X)
                   (X X X X X X X)
                   (X X X X X _ _)
                   (_ X X _ _ _ _)
                   (_ X X _ _ _ _))))
  ; So we also need to generate possibilities when you could put two runs into the same row.

  ; This algorithm generates all plausible solutions expecting to invalidate
  ; all but one of them as it progresses.
  ; When a run's width matches the max width, there is only one possibility.
  ; But, for example, if a run's width is 5 and the max width is 7 there are
  ; multiple possibilities that must be tried:
  ;   [X X X X X _ _] or [_ X X X X X _] or [_ _ X X X X X]
  ;
  ; Each time we generate a new row, we use do profile comparisons.
  ; The top profile of each chunk in the newest (bottom-most) row is compared
  ; against the bottom profile of the corresponding chunk from the previous row.
  (define (valid? [row-accum : (Listof Row)])
    (match row-accum
      [(list rowJ rowI more ...)
       (when (not (= (length rowJ)
                     (length rowI)))
         (error "assert fail - all rows must have the same width!"))
       (define-syntax-rule (lognot expr msg)
         (let ([ret expr])
           (when (not ret)
             (println msg))
           ret))
       (define result
         (for/and : Boolean
           ([chunkI rowI]
            [chunkJ rowJ])
           (cond
             [(and (not chunkI) (not chunkJ)) ; both vacant is fine
              #t]
             [(not chunkI)
              (define profJ (side-profile chunkJ 'top))
              (lognot (< (profile-count profJ) 16) ; TODO what's a good tolerance?
                      "because top is vacant but bottom is full...")]
             [(not chunkJ)
              (define profI (side-profile chunkI 'bottom))
              (lognot (< (profile-count profI) 16) ; TODO what's a good tolerance?
                      "because bottom is vacant but top is full...")]
             [else
              (define profI (side-profile chunkI 'bottom))
              (define profJ (side-profile chunkJ 'top))
              (define max-score (max (profile-count profI)
                                     (profile-count profJ)))
              (define score (profile-count (profile-intersect profI profJ)))
              (define jaggedness (- max-score score))
              (cond
                [(= max-score 0)
                 (println (list "TODO is this right? allowing" (show-row rowI) (show-row rowJ)))
                 #t]
                [else
                 (lognot (< jaggedness 5) ; Probably too tolerant, see comments on the a1i60xvhuw test
                         (list "too jagged:" jaggedness " ..."))])])))
       (when (not result)
         (println (list "INVALIDATING" (show-row rowI) (show-row rowJ))))
       result]
      ; When we don't have two rows yet, it's valid
      [else #t]))

  ; This is the main loop that generates possiblities and invalidates them.
  (: loop (-> (Listof (Listof Chunk)) (Listof Row) (U #f (Listof Row))))
  (define (loop remaining-runs row-accum)
    (match remaining-runs
      [anything
       #:when (not (valid? row-accum))
       #f]
      [(list (list a b)
             (list c d))
       ; The dock area is always the last 4 chunks and always in this position:
       ; [_ X X _ _ ...]
       ; [_ X X _ _ ...]
       ; This is extremely helpful!
       ; (Although I think sometimes there is a totally empty row between the main island
       ;  and the dock. But this won't matter unless the user tries to build something that
       ;  spans that gap, and the result will make it obvious that they can't do that.)
       (let ([dock-top-row : Row
                           (cons #f (cons a (cons b (make-list (- max-width 3) #f))))]
             [dock-bot-row : Row
                           (cons #f (cons c (cons d (make-list (- max-width 3) #f))))])
         (reverse (cons dock-bot-row (cons dock-top-row row-accum))))]
      [(list run more-runs ...)
       (cond
         [(= max-width (length run))
          (loop more-runs (cons (ann run Row) row-accum))]
         [else
          ; TODO we also need to handle the case when two runs belong in the same row
          ; But for now skip it
          (define (recurse [row : Row])
            (println (list "trying possi:" (show-row row)))
            (loop more-runs (cons row row-accum)))
          (ormap recurse (run->row-possibilities run max-width))])]))

  (define result (loop runs (list)))
  (and result
       (map show-row result)))

{module+ for-testing
  (define (get-layout [path : Path])
    (let* ([chunks (get-bedrock-chunks path)])
      (calc-chunk-layout chunks)))

  (define (print-runs [path : Path])
    (let* ([chunks (get-bedrock-chunks path)]
           [runs (split-into-runs chunks)])
      (for ([run runs])
        (println (map chunk->pict run)))))
  }

#;{begin
    (define-syntax-rule (print-runs chunks)
      (let ([runs (split-into-runs chunks)])
        (for ([run runs])
          (println (map chunk->pict run)))))

    (define chunks12 (get-bedrock-chunks "Soggy/01/STGDAT12.BIN"))
    (print-runs chunks12)
    (calc-chunk-layout chunks12)

    (define chunks13 (get-bedrock-chunks "Soggy/01/STGDAT13.BIN"))
    (print-runs chunks13)
    (calc-chunk-layout chunks13)

    (define chunks16 (get-bedrock-chunks "Soggy/01/STGDAT16.BIN"))
    (print-runs chunks16)
    (calc-chunk-layout chunks16)
    }
