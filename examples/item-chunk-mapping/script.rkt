#lang racket

(require "../../NEW-API.rkt"
         "../../chunk.rkt"
         (submod "../../NEW-API.rkt" everything)
         (only-in file/sha1 bytes->hex-string)
         racket/fixnum)

(save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

{begin ;module+ main
  ;(copy-everything! #:from 'B02 #:to 'B00)
  (define B00 (mark-writable (load-stage 'IoA 'B00)))
  (println "loaded stage")
  (define buffer (stage-buffer B00))
  (define CHUNKS (stage-chunks B00))
  (define LAYOUT (get-chunk-layout (stage-kind B00)))

  (define (bytestr buffer start count [blip-size 4])
    (cond
      [(> count blip-size)
       (let ([blip (bytestr buffer start blip-size)])
         (string-append blip " " (bytestr buffer (fx+ start blip-size) (fx- count blip-size))))]
      [else
       (bytes->hex-string (subbytes buffer start (fx+ start count)))]))

  (define nothing-24 (make-bytes 24 0))

  (define (offset-24B i)
    ; Maybe IoA has a few reserved items? So the player-modifiable start
    ; address is a bit later than #x24E7D1 maybe??
    ; This would mean the first 12 records are special?
    (fx+ #;#x24E8F1 #x24E7D1 (fx* i 24)))

  (define (clear-24B! i)
    ; Hmm, the game will load and the item will be gone, but it looks like
    ; the record is still considered active and might be orphaned forever...
    (bytes-copy! buffer (offset-24B i) nothing-24))

  ; Appears to contain only:
  ; * chunk offsets
  ; * defrag index
  (define (offset-4B i)
    (fx+ (fx+ #x150E7D1 (fx* i 4))))

  (define (get-chunk-id xidx zidx)
    (let ([len (vector-length LAYOUT)])
      (and (fx> zidx -1)
           (fx> xidx -1)
           (fx< zidx (vector-length LAYOUT))
           (let ([row (vector-ref LAYOUT zidx)])
             (and (fx< xidx (vector-length row))
                  (vector-ref row xidx))))))

  (define (get-block-id chunk-id x z y)
    (let ([chunk (vector-ref CHUNKS chunk-id)])
      (chunk-ref chunk #:x x #:z z #:y y)))

  (struct record (i item-id xz y dir
                    chunk-id block-id LOC
                    str24 str4
                    ;;;; defrag-index
                    ; Appears to be used for defragmentation?
                    ; When you load a save file, the game will re-order the
                    ; records such that the array index matches this index.
                    ;
                    ; MY HOPE - We can load all the items, manipulate them,
                    ; and write them back from index 1-N with defrag-index 1-N.
                    ; (Inclusive ranges. Assumes index 0 is special/reserved.)
                    ; Should probably also fill (N+1) thru Last with the incrementing
                    ; defrag index and zeroes for the rest (chunk offset).
                    defrag-index
                    ) #:transparent)
  (struct record-B (chunk-id) #:transparent)
  (define (get-24B i)
    (define addr (offset-24B i))
    (define b8 (bytes-ref buffer (fx+ 8 addr)))
    (define b9 (bytes-ref buffer (fx+ 9 addr)))
    (define (parseit)
      (let* ([temp b8]
             [item-id temp]
             [temp b9]
             [item-id (fx+ item-id (fx* 256 (fxand #x1F temp)))]
             [dx (fxrshift temp 5)]
             [temp (bytes-ref buffer (fx+ 10 addr))]
             [dx (+ dx (fxlshift (fxand temp 3) 3))]
             [y (fxrshift temp 2)]
             [temp (bytes-ref buffer (fx+ 11 addr))]
             [dir (case (fxand temp #xC0)
                    ; I think I have this set up such that it it shows the
                    ; direction the builder was facing when the item was placed
                    [(#x00) 'N]
                    [(#x40) 'E]
                    [(#x80) 'S]
                    [(#xC0) 'W]
                    [else (list 'bad-dir temp)])]
             [y (fx+ y (fxlshift (fxand temp 1) 6))]
             [dz (fxrshift (fxand temp #x3E) 1)]
             ; chunk ID comes from a different record:
             [chunk-addr (offset-4B i)]
             [loc (bytes-ref buffer (fx+ 0 chunk-addr))]
             [temp (bytes-ref buffer (fx+ 1 chunk-addr))]
             [loc (fx+ loc (fx* 256 (fxand temp #x0F)))]
             [defrag-index (fxrshift (fxand temp #xF0) 4)]
             ; Okay, this -18 and -23 seems to work on the IoA
             ; but it probably indicates that I am missing something...
             ; Anyway, these give the x and z offsets of the chunk within
             ; the 27x19 chunk layout map (see `parse-map`)
             [xidx (fx+ -18 (fxmodulo loc 64))]
             [zidx (fx+ -23 (fxquotient loc 64))]
             [chunk-id (get-chunk-id xidx zidx)]
             [block-id (and chunk-id
                            (get-block-id chunk-id dx dz y))]
             [temp (bytes-ref buffer (fx+ 2 chunk-addr))]
             [defrag-index (fxior defrag-index (fxlshift temp 4))]
             [temp (bytes-ref buffer (fx+ 3 chunk-addr))]
             [defrag-index (fxior defrag-index (fxlshift temp 12))])
        (record i item-id (xz dx dz) y dir
                chunk-id block-id (list loc xidx zidx)
                (bytestr buffer addr 24)
                (bytestr buffer chunk-addr 4)
                defrag-index)))
    (cond
      #;[(and (fx= 0 b8)
              (fx= 0 b9)
              (fx= 0 (bytes-ref buffer (fx+ 16 addr)))
              (fx= 0 (bytes-ref buffer (fx+ 17 addr)))
              (fx= 0 (bytes-ref buffer (fx+ 18 addr)))
              (fx= 0 (bytes-ref buffer (fx+ 19 addr)))
              (fx= 0 (bytes-ref buffer (fx+ 20 addr)))
              (fx= 0 (bytes-ref buffer (fx+ 21 addr)))
              (fx= 0 (bytes-ref buffer (fx+ 22 addr)))
              (fx= 0 (bytes-ref buffer (fx+ 23 addr))))
         #f]
      ;[(and (fx= 0 b8) (fx= 0 b9)) #f]
      [else (parseit)]))
  (define (active-records [print? #t])
    (define expect-count (+ (item-count B00) -1)) ; The first record is special
    (define actual-count 0)
    (for ([i (in-range #xC8000)])
      ;#:break (= actual-count expect-count)
      (let ([record (get-24B i)])
        (when (and record
                   (not (= 0 (or (record-item-id record) 0)))
                   #;(member (record-block-id record)
                             '(1246 1335 1424 1513 1602 1691 1780 1869 2047)))
          (set! actual-count (+ 1 actual-count))
          (when print? (println record)))))
    (list actual-count expect-count))
  (define (print-em-all [start 0])
    (for ([i (in-range start (fx- #xC8000 start))]) ; (+ 1 (item-count B00)))])
      (let ([result (get-24B i)])
        (when (and result
                   (member (record-block-id result)
                           '(1246 1335 1424 1513 1602 1691 1780 1869 2047)))
          (println result)))))

  (define (change-dir i dir)
    ; WARNING - will crash if the item dimensions is not 1x1x1
    (let* ([items (stage-items B00)]
           [old (vector-ref items i)]
           [new (struct-copy item old [direction dir])])
      (vector-set! items i new)))

  (define (dump-items)
    (with-output-to-file "24b.txt" #:exists 'truncate
      (lambda () (print-items B00))))

  ; NOTE TO SELF:
  ; Put a single stairs item on each chunk, so they should appear at y=51.
  ; Go in order (W to E, N to S) and you should see the the strange math
  ; actually line up with the IoA-chunk-layout.
  (println (item-count B00))
  ;(print-em-all 2293)
  }
