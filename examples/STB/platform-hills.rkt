#lang racket

(require hermits-heresy)

(define this-dir (syntax-source #'here))

(define-syntax-rule (define-hills [id seed arg ...] ...)
  (begin
    (provide id ...)
    (define id
      (let ([filename (format "~a/../shared-hills/~a.bmp" this-dir 'id)])
        (with-absolute-seed [seed]
          (make-platform-hills filename arg ...))))
    ...))

(define-hills
  ; == resort border west ==
  [resort-border-west-hi 1 #:peak-y 54]
  [resort-border-west-lo 2 #:peak-y 44]
  ; To avoid draw distance problems when looking from the NE corner to the SW
  ; corner, lower the hill as it goes south.
  [resort-border-west-extra-lo 3 #:peak-y 39 #:tall-y -3]

  ; == resort border north ==
  [resort-border-north-lo 4 #:peak-y 44]
  [resort-border-north-hi 5 #:peak-y 54]

  ; == dock borders ==
  [dock-border-north 6 #:peak-y 46 #:tall-y -5 #:short-y -4]
  [dock-enclave-west 7 #:peak-y 42]
  [dock-enclave-south 8 #:peak-y 42]
  )
