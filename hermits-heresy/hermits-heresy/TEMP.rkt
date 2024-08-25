#lang typed/racket

(provide show-msg block)

(require "blockdata/blockdef.rkt")

(define-syntax-rule (show-msg arg ...)
  (let ()
    (displayln (format arg ...))
    (void)))

(: block (-> Symbol Fixnum))
(define block
  (let ()
    (define already-warned (ann (make-hash) (Mutable-HashTable Symbol #t)))
    (lambda ([sym : Symbol])
      (define blockdef (symbol->blockdef sym))
      (when (not blockdef)
        (error "Unknown block:" sym))
      (define id (blockdef-id blockdef))
      (define ambiguities (symbol->ambiguities sym))
      (when (and ambiguities
                 (not (hash-ref already-warned sym (lambda () #f))))
        (hash-set! already-warned sym #t)
        (show-msg "Choosing (block '~a) to mean ~a, but other values were possible: ~a" sym id ambiguities))
      id)))
