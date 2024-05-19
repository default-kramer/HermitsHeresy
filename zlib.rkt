#lang typed/racket

(provide compress uncompress)

(require/typed "zlib-untyped.rkt"
               [compress (-> Bytes Bytes)]
               [uncompress (-> Bytes Integer Bytes)])
