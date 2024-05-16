#lang typed/racket

(provide open-stgdat save-stgdat!
         create-floor!
         place-topography!
         clear-map!
         put-block!
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
                       #:y-start [y-start : Integer y])
  (let* ([buffer (stage-buffer stage)])
    (for ([chunk (in-range (chunk-count stage))])
      (for ([y (in-range y-start (+ 1 y))])
        (let ([addr (get-address chunk 0 y 0)])
          (for ([xz (in-range (* 32 32))])
            (block-put! buffer addr block)
            (set! addr (+ 2 addr)))))))
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
