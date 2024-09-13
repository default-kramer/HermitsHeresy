#lang typed/racket

(provide Stgdat-Kind Stage
         stage-kind kind->filename
         stage-read stage-write!
         open-stgdat save-stage!
         stage-protected-area unprotected? protect-area!
         stage-chunk-layout stage-buffer stage-chunks
         item-count)

(require "basics.rkt"
         (only-in "block.rkt" show-msg)
         "chunk.rkt"
         "area.rkt"
         "chunky-area.rkt"
         "ufx.rkt"
         "infer-topia-layout.rkt"
         "config-util.rkt"
         (prefix-in layout: "layouts.rkt")
         (prefix-in zlib: "zlib.rkt"))

(define-type Stgdat-Kind (U 'IoA
                            'Furrowfield
                            'Khrumbul-Dun
                            'Moonbrooke
                            'Malhalla
                            'Anglers-Isle
                            'Skelkatraz
                            'BT1
                            'BT2
                            'BT3))

(: kind->filename (-> Stgdat-Kind String))
(define (kind->filename kind)
  (case kind
    [(IoA) "STGDAT01.BIN"]
    [(Furrowfield) "STGDAT02.BIN"]
    [(Khrumbul-Dun) "STGDAT03.BIN"]
    [(Moonbrooke) "STGDAT04.BIN"]
    [(Malhalla) "STGDAT05.BIN"]
    [(Anglers-Isle) "STGDAT09.BIN"]
    [(Skelkatraz) "STGDAT10.BIN"]
    [(BT1) "STGDAT12.BIN"]
    [(BT2) "STGDAT13.BIN"]
    [(BT3) "STGDAT16.BIN"]))

(: get-chunk-layout (-> Stgdat-Kind (U #f Chunk-Layout)))
(define (get-chunk-layout kind)
  (case kind
    [(BT1 BT2 BT3) #f]
    [(IoA) layout:IoA]
    [(Furrowfield) layout:Furrowfield]
    [(Khrumbul-Dun) layout:Khrumbul-Dun]
    [(Moonbrooke) layout:Moonbrooke]
    [(Malhalla) layout:Malhalla]
    [(Anglers-Isle) layout:Anglers-Isle]
    [(Skelkatraz) layout:Skelkatraz]))

(define header-length #x110)

(struct stgdat-file ([kind : Stgdat-Kind]
                     [path : Path])
  #:transparent #:type-name Stgdat-File)

(struct stage ([loaded-from : Stgdat-File]
               [original-file-size : Integer]
               [header : Bytes] ; mutable (but we probably won't)
               [buffer : Bytes] ; mutable TODO remove this from core? (just use chunks)
               ; Or maybe keep the buffer to know what the file had when it was loaded.
               [chunks : (Immutable-Vectorof Chunk)]
               [protected-area : (Boxof Chunky-Area)]
               [chunk-layout : Chunk-Layout]
               )
  #:type-name Stage)

(: protect-area! (-> Stage Area Area))
(define (protect-area! stage area)
  (define chunky-area
    (cond
      [(chunky-area? area) area]
      [else (error "TODO need to remove legacy area code, or convert to chunky area here...")]))
  (define previous (unbox (stage-protected-area stage)))
  (when (not (eq? previous empty-chunky-area))
    (error "TODO I need to do an area-union here..."))
  (set-box! (stage-protected-area stage) chunky-area)
  previous)

(define (stage-kind [stage : Stage])
  (stgdat-file-kind (stage-loaded-from stage)))

(: stage-read (-> Stage Point (U #f Fixnum)))
(define (stage-read stage point)
  (let* ([chunk-layout (stage-chunk-layout stage)]
         [chunky (chunk-translate chunk-layout point)])
    (and chunky
         (let* ([chunks (stage-chunks stage)]
                [chunk (vector-ref chunks (chunky-chunk-id chunky))]
                [xz (chunky-val chunky)])
           (chunk-ref chunk #:x (xz-x xz) #:z (xz-z xz) #:y (point-y point))))))

; Get the compiler to help me remember to check the XZ coordinate against
; the stage's protected area.
(define-type Unprotected-Proof 'Unprotected-Proof)

(: unprotected? (-> Chunky-Area XZ (U #f Unprotected-Proof)))
(define (unprotected? area xz)
  (and (not (chunky-area-contains? area xz))
       'Unprotected-Proof))

(: stage-write! (-> Stage Unprotected-Proof Point Integer (U #f Void)))
(define (stage-write! stage proof point block)
  (define chunk-layout (stage-chunk-layout stage))
  (define chunky (chunk-translate chunk-layout point))
  (cond
    [(not chunky) #f]
    [else (let* ([chunks (stage-chunks stage)]
                 [chunk (vector-ref chunks (chunky-chunk-id chunky))]
                 [xz (chunky-val chunky)])
            (chunk-set! chunk #:x (xz-x xz) #:z (xz-z xz) #:y (point-y point) #:block block))]))

(define (read-stgdat [path : Path-String])
  (define all-bytes (file->bytes path))
  (define orig-size (bytes-length all-bytes))
  (define header (subbytes all-bytes 0 header-length))
  (define compressed (subbytes all-bytes header-length (bytes-length all-bytes)))
  ; I'm guessing IoA probably always uncompresses to exactly 163,053,024 bytes (including the header),
  ; and we'll just add some room to spare just in case
  (define buffer-size #xA000000)
  (define buffer (zlib:uncompress compressed buffer-size))
  (values header buffer orig-size))

{module+ for-testing
  (provide get-bedrock-chunks)
  (: get-bedrock-chunks (-> Path-String (Listof Chunk)))
  ; Loads chunks but just y=0 to save time. For BT layout tests.
  (define (get-bedrock-chunks path)
    (define-values (header buffer file-size)
      (read-stgdat path))
    (define chunk-length-bytes (:ufx* 2 32 32)) ; just read y=0
    (read-chunks buffer #f chunk-length-bytes))
  }

(define chunk-length-bytes : Fixnum #x30000)

(define-syntax-rule (dqb2-chunk-start-addr i)
  ; Returns the address of chunk i within the uncompressed buffer
  (ufx+ #x183FEF0 (ufx* i chunk-length-bytes)))

(: read-chunks (-> Bytes (U #f Integer) Fixnum (Listof Chunk)))
(define (read-chunks buffer expect-chunk-count chunk-length-bytes)
  (define stop-count (and expect-chunk-count
                          (sub1 expect-chunk-count)))
  (define buffer-length (bytes-length buffer))
  (let loop ([chunks : (Listof Chunk) (list)]
             [i : Fixnum 0])
    (define start-addr (dqb2-chunk-start-addr i))
    (define end-addr
      (let ([end-addr (ufx+ start-addr chunk-length-bytes)])
        (cond
          [(<= end-addr buffer-length)
           end-addr]
          [(= 699 i (or stop-count -1))
           ; Silly special case for last chunk, just read up to the end of the file.
           ; This is needed for Moonbrooke https://github.com/default-kramer/HermitsHeresy/discussions/7
           buffer-length]
          [else
           (error "Uncompressed file is smaller than expected! No data for chunk:" i)])))
    (define chunk (make-empty-chunk))
    (define block-count (load-chunk! chunk buffer (dqb2-chunk-start-addr i) end-addr))
    (cond
      [(and stop-count
            (= i stop-count))
       (reverse (cons chunk chunks))]
      [(and (not stop-count)
            (ufx= 0 block-count))
       (reverse chunks)]
      [else
       (loop (cons chunk chunks)
             (ufx+ 1 i))])))

(: open-stgdat (-> Stgdat-Kind Path Stage))
(define (open-stgdat kind path)
  (define-values (header buffer orig-size)
    (read-stgdat path))
  (define predefined-layout (get-chunk-layout kind))
  (define expect-chunk-count (and predefined-layout
                                  (chunk-count predefined-layout)))
  (define chunk-list (read-chunks buffer expect-chunk-count chunk-length-bytes))
  (define layout (or predefined-layout
                     (infer-topia-layout chunk-list)))
  (when (not layout)
    (error "Failed to infer buildertopia layout from bedrock!
  Assuming you have not modified the bedrock, please include
  the island-generation password in a bug report."))
  (stage (stgdat-file kind path)
         orig-size
         header
         buffer
         (apply vector-immutable chunk-list)
         (box empty-chunky-area)
         layout))

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

  ; https://github.com/default-kramer/HermitsHeresy/issues/12
  ; Until this bug is solved, we need to warn them if the compressed size
  ; has increased. Fortunately, it seems that DQB2 does not optimize for the
  ; smallest possible file size, which gives us some headroom to increase the
  ; complexity of the data while still reducing the compressed size.
  (define orig-size (stage-original-file-size stage))
  (define new-size (+ (bytes-length header)
                      (bytes-length compressed)))
  (show-msg "Saved STGDAT file: ~a" orig-file)
  (show-msg "* Original size: ~a, new size: ~a" orig-size new-size)
  (when (> new-size orig-size)
    (show-msg "!!!!! WARNING !!!!!")
    (show-msg "* File size has increased!")
    (show-msg "* DQB2 might not read the entire file.")
    (show-msg "* Be extra vigilant for errors, and make sure the ship still works.")
    (show-msg "* Consider splitting your work into smaller edits if errors are present.")))

(define (item-count [stage : Stage])
  (define buffer (stage-buffer stage))
  (define a (bytes-ref buffer #x24E7CD))
  (define b (bytes-ref buffer #x24E7CE))
  (define c (bytes-ref buffer #x24E7CF))
  (:ufxior (ufxlshift a 0)
           (ufxlshift b 8)
           (ufxlshift c 16)))
