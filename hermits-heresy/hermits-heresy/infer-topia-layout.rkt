#lang typed/racket

(provide infer-topia-layout)

; Uses the shape of the bedrock to infer the chunk layout.
; This algorithm works in two steps:
; 1) Detect "runs", sequences of chunks that belong
;    next to each other in the same row.
;    This also gives us the width of the layout, because there will always
;    be at least one full-width run.
; 2) Arrange the runs into "rows".
;    This is a pretty classic FP algorithm where we process the runs sequentially
;    and any time there is an ambiguity we explore all possibilities, trusting
;    that only one of them will end up being valid.
; The definition of "valid" is a little loose, but the basic idea is that
; you compare the adjacent sides of the chunks and if there are too many
; mismatches then those two sides must not belong together.
;
; I have a feeling that this algorithm is more complicated than it needs to be.
; Now that I have it working, I wonder if I even need to detect runs at all?
; I suspect I could simply process each chunk sequentially and try just 2 possibilities:
;  * place the chunk at the current position
;  * place a spacer are the current position (keep the chunk)
; This assumes we know the width, which the "run" concept reliably detects,
; but I also think we can determine the width from the number of chunks
;  (e.g. N < count < M implies Medium size which is always 10 wide).


(module+ for-testing
  (provide get-layout print-runs))

(require "chunk.rkt"
         "basics.rkt"
         "ufx.rkt"
         (only-in "layouts.rkt" parse-map)
         typed/pict
         (prefix-in zlib: "zlib.rkt"))

(module+ test (require typed/rackunit))

(define-type Pict pict)

(define length/any (ann length (-> (Listof Any) Index))) ; is this type just better?

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
           [(> jaggedness 4) #t]
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


; Given some runs, returns all possible rows such that:
; * The row contains all of the given runs in the same order
; * The width of each row equals the requested width
; * There is at least one #f ("spacer") between any two runs
(: run-combiner (All (A) (-> (Listof (Listof A)) Integer (Listof (Listof (U #f A))))))
; The generic type is for testability.
(define (run-combiner runs width)
  (define-type ROW (Listof (U #f A))) ; When A=Chunk then ROW=Row
  (: recurse (-> ROW (Listof (Listof A)) (Listof #f) (Listof ROW)))
  (define (recurse row-accum runs spacers)
    (match (list runs spacers)
      [(list (list)
             leftover-spacers)
       (let ([row (ann (append row-accum leftover-spacers) ROW)])
         (list row))]
      [(list (list run)
             (list))
       ; Final run without any trailing spacers is the special case.
       ; (During normal recursion we require at least one trailing spacer)
       (let ([row (ann (append row-accum run) ROW)])
         (list row))]
      [(list (list run more-runs ...)
             (list spacer more-spacers ...))
       (let ([expanded-row (ann (append row-accum run (list spacer)) ROW)])
         (append
          ; recursion option 1: actually build a new row
          (recurse expanded-row more-runs more-spacers)
          ; recursion option 2: just consume one spacer
          (recurse (append row-accum (list spacer)) runs more-spacers)))]
      [else ; need a spacer but we need one
       (list)]))
  (define num-spacers-needed (- width (length (flatten runs))))
  (cond
    [(< num-spacers-needed 0) (list)]
    [else
     (define spacers (make-list num-spacers-needed #f))
     (recurse (list) runs spacers)]))

; Specializes the generic function with the types we actually want.
(: runs->rows (-> (Listof (Listof Chunk)) Integer (Listof Row)))
(define (runs->rows runs width)
  (run-combiner runs width))

{module+ test
  (check-equal? (run-combiner '((x1 x2 x3) (x4 x5 x6)) 8)
                '([x1 x2 x3 #f x4 x5 x6 #f]
                  [x1 x2 x3 #f #f x4 x5 x6]
                  [#f x1 x2 x3 #f x4 x5 x6]))

  (check-equal? (run-combiner '((x1 x2 x3) (x4 x5 x6)) 7)
                '([x1 x2 x3 #f x4 x5 x6]))

  (check-equal? (run-combiner '((x1 x2 x3) (x4 x5 x6)) 6)
                (list))

  (check-equal? (run-combiner '((x1 x2)) 5)
                '([x1 x2 #f #f #f]
                  [#f x1 x2 #f #f]
                  [#f #f x1 x2 #f]
                  [#f #f #f x1 x2]))
  }

(define (show-row [row : Row])
  (map (lambda (item) (if item 'X '_)) row))

(define (calc-chunk-layout [chunks : (Listof Chunk)])
  (define runs (split-into-runs chunks))
  (define max-width (apply max (map length/any runs)))
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
           #;(when (not ret)
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
       (lognot result
               (list "INVALIDATING" (show-row rowI) (show-row rowJ)))
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
      [else
       (: take-runs (-> Integer (U #f (Listof Row))))
       (define (take-runs [take-count : Integer])
         (cond
           [(< (length remaining-runs) take-count)
            #f]
           [else
            (define-values (runs more-runs)
              (split-at remaining-runs take-count))
            (define (try-row [row : Row])
              (loop more-runs (cons row row-accum)))
            (define row-possis (runs->rows runs max-width))
            (define too-wide? ; stop recursion, it would also be too wide
              (empty? row-possis))
            (and (not too-wide?)
                 (or (ormap try-row row-possis)
                     (take-runs (+ 1 take-count))))]))
       (take-runs 1)]))

  (define result (loop runs (list)))
  (and result
       (map show-row result)))

(: infer-topia-layout (-> (Listof Chunk) (U #f Chunk-Layout)))
(define (infer-topia-layout chunks)
  (define result (calc-chunk-layout chunks))
  (and result (parse-map result)))

{module+ for-testing
  (define (get-layout [chunks : (Listof Chunk)])
    (calc-chunk-layout chunks))

  (define (print-runs [chunks : (Listof Chunk)])
    (let* ([runs (split-into-runs chunks)])
      (for ([run runs])
        (println (map chunk->pict run)))))
  }
