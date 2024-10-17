#lang racket

(require hermits-heresy
         (submod hermits-heresy undocumented)
         (only-in hermits-heresy/stage stage-buffer)
         racket/fixnum
         )

(save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

(copy-all-save-files! #:from 'B02 #:to 'B00)

(define stage (load-stage 'IoA 'B00))
(define buf (stage-buffer stage))

(define chunk-length-bytes #x30000)

(define-syntax-rule (dqb2-chunk-start-addr i)
  ; Returns the address of chunk i within the uncompressed buffer
  (+ #x183FEF0 (* i chunk-length-bytes)))

(define (enable-more-chunks! buf [chunk-count 700])
  (println (list "expanding to" chunk-count))
  ; On the IoA, the last chunk has ID=368 at 0x24dc7d-0x24dc7e.
  (define start-chunk-id 369)
  (define start-addr-lo #x24dc7f)

  (for ([i (in-range (- chunk-count start-chunk-id))])
    (let* ([addr-lo (+ start-addr-lo (* 2 i))]
           [addr-hi (+ 1 addr-lo)]
           [val-lo (bytes-ref buf addr-lo)]
           [val-hi (bytes-ref buf addr-hi)])
      (cond
        [(and (= val-lo 255)
              (= val-hi 255))
         (define fresh-chunk-id (+ start-chunk-id i))
         (bytes-set! buf addr-lo (fxand fresh-chunk-id #xFF))
         (bytes-set! buf addr-hi (fxrshift fresh-chunk-id 8))]
        [else
         (error "already has chunk ID!" val-lo val-hi)])))

  ; Update count
  (bytes-set! buf #x24e7c5 (fxand chunk-count #xFF))
  (bytes-set! buf #x24e7c6 (fxrshift chunk-count 8))

  ; place bedrock everywhere
  (for ([chunk (in-range chunk-count)])
    (let ([addr (dqb2-chunk-start-addr chunk)])
      (for ([i (in-range (* 32 32))])
        (bytes-set! buf (+ addr (* i 2)) 1))))

  ; place sand in the new chunks up to y=35
  (for ([chunk (in-range 369 chunk-count)])
    (let ([addr (dqb2-chunk-start-addr chunk)])
      (for ([i (in-range (* 32 32) (* 32 32 35))])
        (bytes-set! buf (+ addr (* i 2)) 11))))
  )

#;(begin
    (enable-more-chunks! buf)
    (save-stage! stage))
