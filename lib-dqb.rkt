#lang typed/racket

(provide open-stgdat save-stgdat!
         block
         create-floor!
         place-topography!
         clear-map!
         put-block!
         print-block-stats
         IoA-find-ring
         IoA-get-special-locs)

(require "lib.rkt")

(module+ test
  (require typed/rackunit))

(define-type Stgdat-Kind (U 'IoA))

(define header-length #x110)

(struct stgdat-file ([kind : Stgdat-Kind]
                     [path : Path])
  #:transparent #:type-name Stgdat-File)

(struct stage ([loaded-from : Stgdat-File]
               [header : Bytes] ; mutable (but we probably won't)
               [buffer : Bytes] ; mutable
               )
  #:type-name Stage)

(define (stage-kind [stage : Stage])
  (stgdat-file-kind (stage-loaded-from stage)))

(define (chunk-count [stage : Stage])
  (case (stage-kind stage)
    [(IoA) 369]
    [else (error "TODO")]))

; TODO anything built-in to Racket for this?
(define temp-dir "C:\\Users\\kramer\\AppData\\Local\\Temp")

; TODO these need to be configurable
(define backup-path "C:\\DQB2dumps\\HH-Backups")
(define zipper-path "C:\\Users\\kramer\\Documents\\code\\HermitsHeresy\\DQB2ZipUtil\\bin\\Debug\\net8.0\\DQB2ZipUtil.exe")

(define (open-stgdat [kind : Stgdat-Kind] [path : Path])
  (define temp-file (build-path temp-dir (format "dqb2~a.in.hhtemp" (random 999999))))
  (define decompress
    (let-values ([(a b c d)
                  (subprocess #f #f #f ; #f
                              zipper-path
                              "-d" path temp-file)])
      a))
  (define finished? (sync/timeout 10 decompress))
  (when (not finished?)
    (error "Decompression failed")) ; TODO should show exit code and stdout/stderr
  (let* ([all-bytes (file->bytes temp-file)]
         [header (subbytes all-bytes 0 header-length)]
         [buffer (subbytes all-bytes header-length (- (bytes-length all-bytes) header-length))]
         [info (stgdat-file kind path)])
    ; TODO delete temp file here
    (stage info header buffer)))

(define (save-stgdat! [stage : Stage])
  (define temp-file (build-path temp-dir (format "dqb2~a.out.hhtemp" (random 999999))))
  (let ([header (stage-header stage)]
        [buffer (stage-buffer stage)]
        [port (open-output-file temp-file #:exists 'truncate)])
    (write-bytes header port)
    (write-bytes buffer port)
    (close-output-port port))
  (define orig-file (stgdat-file-path (stage-loaded-from stage)))
  ; TODO copy orig-file to backups dir before overwriting
  (define compress
    (let-values ([(a b c d)
                  (subprocess #f #f #f ; #f
                              zipper-path
                              "-c" temp-file orig-file)])
      a))
  (define finished? (sync/timeout 10 compress))
  (when (not finished?)
    (error "Compression failed")) ; TODO should show exit code and stdout/stderr
  (println (format "WROTE TO: ~a" orig-file))
  (void))

(define (TODO [rows : (Listof (Listof (U '_ 'X)))])
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

(define IoA-chunk-layout (TODO '((_ _ _ _ _ _ _ X X X X X X X X _ _ _ X X X _ _ _ _ _ _)
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

(: IoA-abs->rel (-> Integer Integer (U #f (List Integer Integer Integer))))
(define (IoA-abs->rel x z)
  (let*-values ([(x-offset x) (quotient/remainder x 32)]
                [(z-offset z) (quotient/remainder z 32)])
    (let* ([row (vector-ref IoA-chunk-layout z-offset)]
           [chunk-id (vector-ref row x-offset)])
      (and chunk-id
           (list chunk-id x z)))))

(: IoA-rel->abs (-> Integer Integer Integer (U #f XZ)))
(define IoA-rel->abs
  (let ([ht (ann (make-hash) (Mutable-HashTable Integer XZ))])
    (for ([z (in-range (vector-length IoA-chunk-layout))])
      (let ([row (vector-ref IoA-chunk-layout z)])
        (for ([x (in-range (vector-length row))])
          (let ([chunk-id (vector-ref row x)])
            (when chunk-id
              (hash-set! ht chunk-id (cons x z)))))))
    (define (func [chunk-id : Integer] [x : Integer] [z : Integer])
      (let ([xz (hash-ref ht chunk-id (lambda () #f))])
        (and xz (cons (+ x (* 32 (car xz)))
                      (+ z (* 32 (cdr xz)))))))
    func))

(: IoA-chunky->point (-> Integer Integer Integer Integer Point))
(define (IoA-chunky->point chunk x y z)
  (let* ([xz (IoA-rel->abs chunk x z)])
    (or (and xz
             (point (car xz) y (cdr xz)))
        (error "Invalid chunk coords" chunk x y z))))

(define (get-address [chunk : Integer] [x : Integer] [y : Integer] [z : Integer])
  (+ #x183FEF0
     (* chunk #x30000)
     (* y 32 32 2)
     (* z 32 2)
     (* x 2)))

(define (get-address2 [p : Point])
  (let ([chunky (IoA-abs->rel (point-x p) (point-z p))])
    (and chunky
         (get-address (first chunky) (second chunky) (point-y p) (third chunky)))))

(define (IoA:point [chunk : Integer] [x : Integer] [y : Integer] [z : Integer])
  (let ([xz (IoA-rel->abs chunk x z)])
    (or (and xz (point (car xz) y (cdr xz)))
        (error "Invalid coord:" '(chunk x y z)))))

; Here x and z are "within the chunk" (and therefore < 32) because it was
; easiest to enter them that way.
(define-syntax-rule (define-special-locs all-id loc-func [id (chunk x y z) ...] ...)
  (begin
    (define all-id
      (list (IoA:point chunk x y z)
            ...
            ...))
    (define (loc-func [arg : Symbol])
      (case arg
        [(id) (list (IoA:point chunk x y z) ...)]
        ...
        [else #f]))))

(: IoA-get-special-locs (-> Symbol (U #f (Listof Point))))
(: IoA-special-locs (Listof Point))
(define-special-locs IoA-special-locs IoA-get-special-locs
  [blue-tablet (55 19 53 14) (55 20 53 14) (55 21 53 14)
               (55 19 54 14) (55 20 54 14) (55 21 54 14)
               (55 19 55 14) (55 20 55 14) (55 21 55 14)
               (55 19 56 14) (55 20 56 14) (55 21 56 14)])

(: IoA-special-locs-multi (-> (Listof Symbol) (Listof Point)))
(define (IoA-special-locs-multi ids)
  (if (empty? ids)
      (list)
      (append (or (IoA-get-special-locs (car ids))
                  (error "Invalid special loc ID:" (car ids)))
              (IoA-special-locs-multi (cdr ids)))))

(module+ test
  (define (special-addresses [id : Symbol])
    (map get-address2 (or (IoA-get-special-locs id)
                          (error "assert fail"))))
  (check-equal? (special-addresses 'blue-tablet)
                (list #x22AAA96 #x22AAA98 #x22AAA9A
                      #x22AB296 #x22AB298 #x22AB29A
                      #x22ABA96 #x22ABA98 #x22ABA9A
                      #x22AC296 #x22AC298 #x22AC29A))
  )

(define (block-put! [buffer : Bytes] [addr : Integer] [block : Integer])
  (bytes-set! buffer (+ 0 addr) (bitwise-bit-field block 0 8))
  (bytes-set! buffer (+ 1 addr) (bitwise-bit-field block 8 16)))

(define (create-floor! [stage : Stage]
                       #:y [y : Integer]
                       #:block [block : Integer]
                       #:y-start [y-start : Integer y]
                       #:chunk-filter [chunk-filter : (U #f (Listof Integer)) #f])
  (let* ([buffer (stage-buffer stage)])
    (for ([chunk (in-range (chunk-count stage))])
      (when (or (not chunk-filter)
                (member chunk chunk-filter))
        (for ([y (in-range y-start (+ 1 y))])
          (let ([addr (get-address chunk 0 y 0)])
            (for ([xz (in-range (* 32 32))])
              (block-put! buffer addr block)
              (set! addr (+ 2 addr))))))))
  (void))

(define (clear-map! [stage : Stage]
                    #:above-y [above-y : Integer 0]
                    #:keep-items? [keep-items? : Boolean #f]
                    #:add-chunk-ids? [add-chunk-ids? : Boolean #f])
  (define buffer (stage-buffer stage))
  ; Reset count of 24-byte records to zero
  ; (this is probably a 4-byte number but 0xC8000 is the max)
  (when (not keep-items?)
    (bytes-set! buffer #x24E7CD 0)
    (bytes-set! buffer #x24E7CE 0)
    (bytes-set! buffer #x24E7CF 0))
  (for ([chunk (in-range (chunk-count stage))])
    (let ([addr (get-address chunk 0 0 0)])
      (for ([y (in-range 96)])
        (for ([xz (in-range (* 32 32))])
          (when (and (> y above-y)
                     (or (not keep-items?)
                         (case (bytes-ref buffer (+ 1 addr))
                           [(0 8) #t] ; UNSURE: it's a simple block
                           [else #f])))
            (block-put! buffer addr 0))
          (set! addr (+ 2 addr))))))
  (when add-chunk-ids?
    (for ([chunk (in-range (chunk-count stage))])
      (let*-values ([(fifties ones) (quotient/remainder chunk 50)]
                    [(tens ones) (quotient/remainder ones 10)]
                    [(x) 10]
                    [(z) 10]
                    [(stripe?) #t])
        (define (inc-x!)
          (let ([inc (if (= 0 (modulo x 13)) 2 1)])
            (set! x (+ x inc))))
        ; 50s
        (for ([_ (in-range fifties)])
          (for ([y (in-range (+ 1 above-y)
                             (+ 6 above-y))])
            (let ([addr (get-address chunk x y z)])
              (bytes-set! buffer (+ 0 addr) (if stripe? 3 4)) ; grassy/limegrassy
              (bytes-set! buffer (+ 1 addr) 0)))
          (set! stripe? (not stripe?))
          (inc-x!))
        ; 10s
        (for ([y (in-range (+ 1 above-y)
                           (+ 1 tens above-y))])
          (let ([addr (get-address chunk x y z)])
            (bytes-set! buffer (+ 0 addr) (if stripe? 3 4)) ; grassy/limegrassy
            (bytes-set! buffer (+ 1 addr) 0)))
        ; 1s
        (set! x 10)
        (set! z (+ 2 z))
        (for ([_ (in-range ones)])
          (let ([addr (get-address chunk x (+ 1 above-y) z)])
            (bytes-set! buffer (+ 0 addr) 10) ; obsidian
            (bytes-set! buffer (+ 1 addr) 0))
          (inc-x!)))))
  (void))

(define (point-neighbors [p : Point])
  (map (lambda ([xz : XZ]) (point (car xz) (point-y p) (cdr xz)))
       (neighbors (point->xz p))))

(define (print-block-stats [stage : Stage]
                           #:min-y [min-y : Integer]
                           #:max-y [max-y : Integer])
  (define buffer (stage-buffer stage))
  (for ([chunk (in-range (chunk-count stage))])
    (for ([y (in-range min-y (+ 1 max-y))])
      (for ([x (in-range 32)])
        (for ([z (in-range 32)])
          (let* ([addr (get-address chunk x y z)]
                 [a (bytes-ref buffer addr)]
                 [b (bytes-ref buffer (+ 1 addr))])
            (when (not (= 0 a b))
              (println (cons a b)))))))))

(define (IoA-find-ring [stage : Stage] [block : Integer]
                       #:min-y [min-y : Integer 0] #:max-y [max-y : Integer 95])
  (define block-lo (bitwise-bit-field block 0 8))
  (define block-hi (bitwise-bit-field block 8 16))
  (println (list block-lo block-hi))
  (define buffer (stage-buffer stage))
  (define (is-block? [p : Point])
    (let ([addr (get-address2 p)])
      (and addr
           (= block-lo (bytes-ref buffer addr))
           (= block-hi (bytes-ref buffer (+ 1 addr))))))
  (define (test-ring! [start : Point] [seen : (Mutable-HashTable Point #t)])
    (: go (-> Point (Listof Point) (U #f Ring)))
    (define (go p accum)
      (set! accum (cons p accum))
      (cond
        [(and (equal? p start)
              (not (= 1 (length accum))))
         #;(println (list "GOT BACK TO START" accum))
         (points->ring accum)]
        [(not (is-block? p))
         (hash-set! seen p #t)
         #f]
        [(hash-ref seen p (lambda () #f))
         #f]
        [else
         (hash-set! seen p #t)
         (let loop ([tries (point-neighbors p)])
           (match tries
             [(list) #f]
             [(list a rest ...)
              (or (go a accum)
                  (loop rest))]))]))
    (go start (list)))
  (let ([seen (ann (make-hash) (Mutable-HashTable Point #t))]
        [result (ann #f (U #f Ring))])
    (let loop : (U #f Ring)
      ([chunk : Integer 0]
       [y : Integer min-y]
       [x : Integer 0]
       [z : Integer 0])
      (let ([p (IoA-chunky->point chunk x y z)])
        (when (not (hash-ref seen p (lambda () #f)))
          (when (is-block? p)
            #;(println (list "FOUND STARTER" p))
            (set! result (test-ring! p seen)))
          (hash-set! seen p #t))
        (set! z (+ 1 z))
        (when (= z 32)
          (set! z 0)
          (set! x (+ 1 x)))
        (when (= x 32)
          (set! x 0)
          (set! y (+ 1 y)))
        (when (> y max-y)
          (set! y min-y)
          (set! chunk (+ 1 chunk)))
        (cond
          [result result]
          [(>= chunk (chunk-count stage)) #f]
          [else (loop chunk y x z)])))))

(define (place-topography! [stage : Stage] [top : Topography]
                           #:x [x : Integer]
                           #:z [z : Integer]
                           #:y-start [y-start : Integer]
                           #:y-end [y-end : Integer]
                           #:block [block : Integer])
  (define warned? : Boolean #f)
  (let* ([buffer (stage-buffer stage)]
         [peaks : (Immutable-HashTable XZ Integer)
                (topography-peaks top)]
         [keys (hash-keys peaks)])
    (for ([xz keys])
      (let* ([top-dy (hash-ref peaks xz)]
             [x (+ x (car xz))]
             [z (+ z (cdr xz))]
             [chunky (IoA-abs->rel x z)])
        (if (not chunky)
            (when (not warned?)
              (set! warned? #t)
              (println (format "Topography extends out of bounds! Skipping X,Z=~a,~a and possibly more..." x z)))
            (let* ([chunk-id (first chunky)]
                   [chunk-x (second chunky)]
                   [chunk-z (third chunky)]
                   [y-start (+ y-start top-dy)])
              ; TODO I think this loop will incorrectly handle negative ranges...
              ; (We should do nothing maybe with a warning, instead of converting to a positive range)
              (for ([y (in-range (min y-start y-end)
                                 (+ 1 (max y-start y-end)))])
                (let ([addr (get-address chunk-id chunk-x y chunk-z)])
                  (block-put! buffer addr block)))))))))

(define (put-block! [stage : Stage] [point : Point] [block : Integer])
  (let ([chunky (IoA-abs->rel (point-x point) (point-z point))])
    (and chunky
         (let ([buffer (stage-buffer stage)]
               [chunk-id (first chunky)]
               [chunk-x (second chunky)]
               [chunk-z (third chunky)])
           (let ([addr (get-address chunk-id chunk-x (point-y point) chunk-z)])
             (block-put! buffer addr block))))))

(define-syntax-rule (define-blocks f [id val] ...)
  (define (f a)
    (case a
      [(id) (bitwise-ior #x800 val)]
      ...
      [else (error "Unknown block:" a)])))

; For example, Stony-Soil has a "value" of 9c and could be written to file as 9c 08
; The 08 means "no chisel" (and maybe "placed by user"? because 9c 00 also works).
; Chisel status can be one of (all in hex):
; * 08 - no chisel
; * 18/38/58/78 - diagonal chisel N/E/S/W, matches (blueprint.chisel_status << 4) | 8
; * 28/48/68/88 - diagonal chisel SW/SE/NW/NE
; * 98/a8/b8/c8 - concave chisel NW/SW/SE/NE
; * d8/e8 - flat chisel hi/lo
; * 0a - no chisel, for a different set of blocks?
;   And then presumably 1a/3a/5a/7a diagonal chisel, da/ea flat chisel, etc...
(define-blocks block
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
  [Arid-Earth #x93]
  [Chert #x95]
  [Chunky-Chert #x99]
  [Spoiled-Soil #x9C]
  [Umber #xD1]
  [Lumpy-Umber #xF1])
