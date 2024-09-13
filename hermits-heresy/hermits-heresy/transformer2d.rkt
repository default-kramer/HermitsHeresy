#lang typed/racket

(provide rotate0 rotate90 rotate180 rotate270
         reflect+0 reflect+90 reflect+180 reflect+270)

(require "ufx.rkt")

(module+ test (require typed/rackunit))

; This may seem backwards...
; These transformers accept the transformed coordinates and return the
; original coordinates. This is to allow a pattern like this:
#;(when (in-area? transformed-area)
    (let-values ([(x2 z2) (transformer-func current-x current-z)])
      (let ([blk (selection-ref selection x2 z2 current-y)])
        (when (and blk (simple? blk))
          (set-block! blk)))))

(define-type Transformer2D
  (-> Fixnum Fixnum Fixnum Fixnum (Values Fixnum Fixnum)))

(: rotate0 Transformer2D)
(: rotate90 Transformer2D)
(: rotate180 Transformer2D)
(: rotate270 Transformer2D)
(: reflect+0 Transformer2D)
(: reflect+90 Transformer2D)
(: reflect+180 Transformer2D)
(: reflect+270 Transformer2D)

(define (rotate0 x z W H)
  (values x z))

(define (rotate90 x z W H)
  (values z
          (ufx- H x)))

(define (rotate180 x z W H)
  (values (ufx- W x)
          (ufx- H z)))

(define (rotate270 x z W H)
  (values (ufx- W z)
          x))

(define (reflect+0 x z W H)
  (values (ufx- W x)
          z))

(define (reflect+90 x z W H)
  (values (ufx- W z)
          (ufx- H x)))

(define (reflect+180 x z W H)
  (values x
          (ufx- H z)))

(define (reflect+270 x z W H)
  (values z x))

{module+ test
  (define-syntax-rule (dotest orig f transformed)
    (let* ([listo : (Listof (Listof Any)) 'orig]
           [listx : (Listof (Listof Any)) 'transformed]
           [H (length listo)]
           [W (length (first listo))]
           [dim (ann (max H W) Fixnum)]
           [H (ufx+ H -1)]
           [W (ufx+ W -1)])
      (define (ref [lst : (Listof (Listof Any))] [x : Integer] [z : Integer])
        (and (>= x 0)
             (>= z 0)
             (< z (length lst))
             (let ([row (list-ref lst z)])
               (and (< x (length row))
                    (list-ref row x)))))
      (for ([z : Fixnum (ufx-in-range dim)])
        (for ([x : Fixnum (ufx-in-range dim)])
          (let-values ([(x2 z2) (f x z W H)])
            (let ([item (ref listx x z)]
                  [item2 (ref listo x2 z2)])
              ;(println (list (list x z item) (list x2 z2 item2)))
              (check-equal? item item2)))))
      (void)))

  (dotest [[A B C]
           [D E F]]
          rotate0
          [[A B C]
           [D E F]])

  (dotest [[A B C]
           [D E F]]
          rotate90
          [[D A]
           [E B]
           [F C]])

  (dotest [[A B C]
           [D E F]]
          rotate180
          [[F E D]
           [C B A]])

  (dotest [[A B C]
           [D E F]]
          rotate270
          [[C F]
           [B E]
           [A D]])

  (dotest [[A B C]
           [D E F]]
          reflect+0
          [[C B A]
           [F E D]])

  (dotest [[A B C]
           [D E F]]
          reflect+90
          [[F C]
           [E B]
           [D A]])

  (dotest [[A B C]
           [D E F]]
          reflect+180
          [[D E F]
           [A B C]])

  (dotest [[A B C]
           [D E F]]
          reflect+270
          [[A D]
           [B E]
           [C F]])
  }
