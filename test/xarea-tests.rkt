#lang racket

(require hermits-heresy
         (only-in "../hermits-heresy/hermits-heresy/transformed-area.rkt"
                  make-xarea xarea-ref)
         (only-in "../hermits-heresy/hermits-heresy/NEW-API.rkt"
                  area-contains? xz)
         pict
         rackunit)

; All images will be 100x100 pixels.
(define W 100)
(define H 100)

; In this original area, the point 23,34 is the top left corner which
; remains true after rotation and reflection.
(define orig-area (bitmap->area "images/transform-orig.bmp"))

(define (check-xarea expect-path transforms)
  ; The expect-path identifies a manually constructed bitmap that corresponds to
  ; the transformed area.
  (let ([xarea (make-xarea orig-area transforms)]
        [expect-area (bitmap->area expect-path)])
    (for ([z (in-range H)])
      (for ([x (in-range W)])
        (let* ([new-coord (xz x z)]
               [msg (format "for transformed coord: ~a" new-coord)])
          (cond
            [(area-contains? expect-area new-coord)
             (let ([orig-coord (xarea-ref xarea new-coord)])
               (check-not-false orig-coord msg)
               (check-true (area-contains? orig-area orig-coord) msg))]
            [else
             (check-false (xarea-ref xarea new-coord) msg)]))))))

(define (xarea->pict xarea)
  (define pixels (make-bytes (* 4 W H)))
  (define idx 0)
  (for ([z (in-range H)])
    (for ([x (in-range W)])
      (when (xarea-ref xarea (xz x z))
        (bytes-set! pixels idx 255)
        (bytes-set! pixels (+ 1 idx) 255))
      (set! idx (+ 4 idx))))
  (argb-pixels->pict pixels W))


(check-xarea "images/transform-translate.bmp" '((translate -5 -10)))
(check-xarea "images/transform-translate.bmp" '((rotate 360)
                                                (translate -5 -10)
                                                (rotate 720)))

; 90 degrees, no translation
(check-xarea "images/transform-90.bmp" '((rotate 90)))
(check-xarea "images/transform-90.bmp" '((rotate 450))) ; 90 plus 360
(check-xarea "images/transform-90.bmp" '((rotate -990))) ; -270 minus 720

; 180 degrees, translated to 1,1
(check-xarea "images/transform-180.bmp" '((rotate 180)
                                          (translate-to 1 1)))
(check-xarea "images/transform-180.bmp" '((translate -22 -33)
                                          (rotate 540)))
(check-xarea "images/transform-180.bmp" '((rotate 270)
                                          (translate -22 -33)
                                          (rotate -90)))

; 270 degrees, translated to 1,1
(check-xarea "images/transform-270.bmp" '((rotate 270)
                                          (translate-to 1 1)))
(check-xarea "images/transform-270.bmp" '((translate -22 -33)
                                          (rotate -90)))
(check-xarea "images/transform-270.bmp" '((rotate 180)
                                          (translate -22 -33)
                                          (rotate 90)))

; mirror-x + 0
; mirror-x is equivalent to mirror-z and a 180 rotation
(check-xarea "images/transform-mirror0.bmp" '(mirror-x))
(check-xarea "images/transform-mirror0.bmp" '(mirror-z
                                              (rotate 180)))
(check-xarea "images/transform-mirror0.bmp" '((rotate -540)
                                              mirror-z))

; mirror-x + 90
(check-xarea "images/transform-mirror90.bmp" '(mirror-x
                                               (translate-to 1 1)
                                               (rotate 90)))
(check-xarea "images/transform-mirror90.bmp" '(mirror-z
                                               (rotate 270)
                                               (translate-to 1 1)))

; mirror-x + 180
(check-xarea "images/transform-mirror180.bmp" '(mirror-x
                                                (translate-to 1 1)
                                                (rotate 180)))
(check-xarea "images/transform-mirror180.bmp" '(mirror-z
                                                (rotate 360)
                                                (translate-to 1 1)))

; mirror-x + 270
(check-xarea "images/transform-mirror270.bmp" '(mirror-x
                                                (translate-to 1 1)
                                                (rotate 270)))
(check-xarea "images/transform-mirror270.bmp" '(mirror-z
                                                (rotate 90)
                                                (translate-to 1 1)))
