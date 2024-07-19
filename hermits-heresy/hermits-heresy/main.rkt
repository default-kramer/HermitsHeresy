#lang racket

(provide save-dir
         bitmap->hill
         put-hill!
         load-stage
         copy-all-save-files!
         save-stage!
         block
         get-template-image
         save-template-image
         )

(module+ undocumented
  (provide area->hill2
           bitmap->area
           chisel
           decorate-peaks!
           protected-areas
           simple?
           stage->pict
           ))

(require "NEW-API.rkt"
         "images.rkt")
