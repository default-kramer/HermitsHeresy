#lang typed/racket

(provide open-stgdat save-stgdat!
         create-floor!
         place-topography!
         clear-map!)

(require (submod "lib.rkt" typed))

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


(define (get-address [chunk : Integer] [x : Integer] [y : Integer] [z : Integer])
  (+ #x183FEF0
     (* chunk #x30000)
     (* y 32 32 2)
     (* z 32 2)
     (* x 2)))

(define (create-floor! [stage : Stage] #:y [y : Integer])
  (let* ([buffer (stage-buffer stage)])
    (for ([chunk (in-range (chunk-count stage))])
      (let ([addr (get-address chunk 0 y 0)])
        (for ([xz (in-range (* 32 32))])
          (bytes-set! buffer addr 11)
          (set! addr (+ 1 addr))
          (bytes-set! buffer addr #xD8)
          (set! addr (+ 1 addr))))))
  (void))

(define (clear-map! [stage : Stage]
                    #:floor-y [floor-y : Integer 0]
                    #:add-chunk-ids? [add-chunk-ids? : Boolean #f])
  (define buffer (stage-buffer stage))
  ; Reset count of 24-byte records to zero
  ; (this is probably a 4-byte number but 0xC8000 is the max)
  (bytes-set! buffer #x24E7CD 0)
  (bytes-set! buffer #x24E7CE 0)
  (bytes-set! buffer #x24E7CF 0)
  (for ([chunk (in-range (chunk-count stage))])
    (let ([addr (get-address chunk 0 0 0)])
      (for ([y (in-range 96)])
        (for ([xz (in-range (* 2 32 32))])
          (when (> y floor-y)
            (bytes-set! buffer addr 0))
          (set! addr (+ 1 addr))))))
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
          (for ([y (in-range (+ 1 floor-y)
                             (+ 6 floor-y))])
            (let ([addr (get-address chunk x y z)])
              (bytes-set! buffer (+ 0 addr) (if stripe? 3 4)) ; grassy/limegrassy
              (bytes-set! buffer (+ 1 addr) 0)))
          (set! stripe? (not stripe?))
          (inc-x!))
        ; 10s
        (for ([y (in-range (+ 1 floor-y)
                           (+ 1 tens floor-y))])
          (let ([addr (get-address chunk x y z)])
            (bytes-set! buffer (+ 0 addr) (if stripe? 3 4)) ; grassy/limegrassy
            (bytes-set! buffer (+ 1 addr) 0)))
        ; 1s
        (set! x 10)
        (set! z (+ 2 z))
        (for ([_ (in-range ones)])
          (let ([addr (get-address chunk x (+ 1 floor-y) z)])
            (bytes-set! buffer (+ 0 addr) 10) ; obsidian
            (bytes-set! buffer (+ 1 addr) 0))
          (inc-x!)))))
  (void))

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
                  (bytes-set! buffer (+ 0 addr) (bitwise-bit-field block 0 8))
                  (bytes-set! buffer (+ 1 addr) (bitwise-bit-field block 8 16))))))))))
