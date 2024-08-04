#lang racket

(provide save-dir
         bitmap->hill
         put-hill!
         load-stage
         copy-all-save-files!
         save-stage!
         block
         find-block-name
         get-template-image
         save-template-image
         protect-stage!
         )

(module+ undocumented
  (provide area->hill2
           bitmap->area
           chisel
           decorate-peaks!
           simple?
           stage->pict
           ))

(require "NEW-API.rkt"
         "images.rkt")
