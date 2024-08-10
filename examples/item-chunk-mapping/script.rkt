#lang racket

(require "../../NEW-API.rkt"
         (submod "../../NEW-API.rkt" everything)
         racket/fixnum)

(save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

{begin ;module+ main
  (define B00 (mark-writable (load-stage 'IoA 'B00)))
  (println "loaded stage")
  (define buffer (stage-buffer B00))

  (define (bytestr buffer start count)
    (define bytes (for/list ([i (in-range start (fx+ start count))])
                    (bytes-ref buffer i)))
    ; if count is e.g. 3, make the string "~x ~x ~x"
    (define str (list->string (cdr (flatten (make-list count (list #\space #\~ #\x))))))
    (apply format (cons str bytes))
    ;""
    )

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

  (define (offset-4B i)
    (fx+ (fx+ #x150E7D1 (fx* i 4))))

  (struct record (i item-id xz y dir
                    LOC
                    str24 str4 counter4) #:transparent)
  (struct record-B (chunk-id) #:transparent)
  (define (get-24B i)
    (define addr (offset-24B i))
    (define (parseit)
      (let* ([temp (bytes-ref buffer (fx+ 8 addr))]
             [item-id temp]
             [temp (bytes-ref buffer (fx+ 9 addr))]
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
             [counter4 (bytes-ref buffer (fx+ 2 chunk-addr))]
             [temp (bytes-ref buffer (fx+ 3 chunk-addr))]
             [counter4 (fx+ counter4 (fx* 256 temp))])
        (record i item-id (xz dx dz) y dir
                (list loc
                      ; Okay, this -18 and -23 seems to work on the IoA
                      ; but it probably indicates that I am missing something...
                      ; Anyway, these give the x and z offsets of the chunk within
                      ; the 27x19 chunk layout map (see `parse-map`)
                      (fx+ -18 (fxmodulo loc 64))
                      (fx+ -23 (fxquotient loc 64)))
                (bytestr buffer addr 24)
                (bytestr buffer chunk-addr 4)
                counter4)))
    (cond
      #;[(and (fx= 0 (bytes-ref buffer (fx+ 0 addr)))
              (fx= 0 (bytes-ref buffer (fx+ 1 addr)))
              (fx= 0 (bytes-ref buffer (fx+ 2 addr)))
              (fx= 0 (bytes-ref buffer (fx+ 3 addr))))
         #f]
      [else (parseit)]))
  (define (print-em-all [start 0])
    (for ([i (in-range start (item-count B00))])
      (let ([result (get-24B i)])
        (when result (println result)))))
  ; NOTE TO SELF:
  ; Put a single stairs item on each chunk, so they should appear at y=51.
  ; Go in order (W to E, N to S) and you should see the the strange math
  ; actually line up with the IoA-chunk-layout.
  (print-em-all 2293)
  }
