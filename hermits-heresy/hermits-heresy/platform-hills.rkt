#lang typed/racket

(provide make-platform-hills
         platform-hills-peak platform-hills-peak-border
         platform-hills-tall platform-hills-tall-border
         platform-hills-short platform-hills-short-border
         phill-area phill-y phill-block phill-chisel)

(require "platform-layout.rkt"
         "basics.rkt"
         "block.rkt"
         "chisel.rkt"
         "chunky-area.rkt"
         "area.rkt"
         "ufx.rkt"
         (prefix-in magic: (submod "platform-layout.rkt" magic-numbers)))

(struct phill ([area : Area]
               [y : Fixnum]
               [block : Fixnum]
               [chisel : Chisel])
  #:transparent #:type-name PHill)

(struct platform-hills ([peak : PHill]
                        [peak-border : PHill]
                        [tall : PHill]
                        [tall-border : PHill]
                        [short : PHill]
                        [short-border : PHill])
  #:transparent #:type-name Platform-Hills)

(: make-platform-hills (-> Platform-Layout
                           [#:x Fixnum]
                           [#:z Fixnum]
                           [#:wall-block (U Symbol Fixnum)]
                           [#:wall-chisel Chisel]
                           [#:platform-block (U Symbol Fixnum)]
                           [#:peak-y Fixnum]
                           [#:tall-y Fixnum]
                           [#:short-y Fixnum]
                           Platform-Hills))
(define (make-platform-hills layout
                             #:x [dx 0]
                             #:z [dz 0]
                             #:wall-block [wall-block 'Umber]
                             #:wall-chisel [wall-chisel 'flat-lo]
                             #:platform-block [platform-block 'Seeded-Mossy-Spoiled-Soil]
                             #:peak-y [peak-y 40]
                             #:tall-y [tall-y -4]
                             #:short-y [short-y -2])
  (define WW (platform-layout-width layout))
  (define HH (platform-layout-depth layout))
  (define bounding-rect (make-rect (xz dx dz)
                                   (xz (ufx+ dx WW) (ufx+ dz HH))))
  (define array2d (platform-layout-array2d layout))

  (: build-area (-> Fixnum Area))
  (define (build-area val)
    (build-chunky-area
     bounding-rect
     (lambda (xz)
       (let* ([x (xz-x xz)]
              [z (xz-z xz)]
              [x (ufx- x dx)]
              [z (ufx- z dz)]
              [idx (ufx+ x (ufx* WW z))])
         (and (ufx>= x 0)
              (ufx< x WW)
              (ufx>= z 0)
              (ufx< z HH)
              (= val (bytes-ref array2d idx)))))
     (lambda args (void))))

  (define (target-val kind) (case kind
                              [(peak-border) magic:PEAK-BORDER]
                              [(peak) magic:PEAK]
                              [(tall-border) magic:TALL-BORDER]
                              [(tall) magic:TALL]
                              [(short-border) magic:SHORT-BORDER]
                              [(short) magic:SHORT]
                              [else
                               (error "invalid kind:" kind)]))

  (define (make [kind : Any] [peak : Fixnum] [block-id : (U Symbol Fixnum)] [chisel : Chisel])
    (phill (build-area (target-val kind))
           peak
           (if (symbol? block-id)
               (block block-id)
               block-id)
           chisel))

  (let* ([tall-y (if (negative? tall-y)
                     (ufx+ peak-y tall-y)
                     tall-y)]
         [short-y (if (negative? short-y)
                      (ufx+ tall-y short-y)
                      short-y)])
    (platform-hills (make 'peak peak-y platform-block 'none)
                    (make 'peak-border peak-y wall-block wall-chisel)
                    (make 'tall tall-y platform-block 'none)
                    (make 'tall-border tall-y wall-block wall-chisel)
                    (make 'short short-y platform-block 'none)
                    (make 'short-border short-y wall-block wall-chisel))))
