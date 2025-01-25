#lang racket

(require "blockdef.rkt"
         (submod "blockdef.rkt" for-json)
         json)

(define js-expr
  (for/list ([bd all-blockdefs])
    (let* ([sym (blockdef-symbol bd)]
           [id (blockdef-id bd)]
           [primary-id (blockdef-id (symbol->blockdef sym))]
           [liquid? (blockdef-liquid? bd)]
           [star? (blockdef-star? bd)]
           [dyes (blockdef-dyes bd)])
      (hash 'Name (format "~a" sym)
            'BlockId id
            'PrimaryBlockId primary-id
            'IsLiquid liquid?
            'Dyes (map (lambda (dye) (hash 'Color (format "~a" (car dye))
                                           'BlockId (cdr dye)))
                       dyes)
            ))))

;js-expr

(define json-str
  (with-output-to-string
    (lambda () (write-json js-expr #:indent 2))))

(displayln json-str)
