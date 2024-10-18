#lang racket

{module+ test
  (require "../hermits-heresy/hermits-heresy/NEW-API.rkt"
           (submod "../hermits-heresy/hermits-heresy/NEW-API.rkt" for-testing)
           rackunit)

  (define-syntax-rule (load kind path)
    (load-stage kind (string->path path)))

  ; 2024-05-23 looks correct to my eyes
  ; 2024-10-18 I'm not sure anymore; I definitely did not understand liquids at that
  ; time and I still kind of don't. Hmm...
  #;(let ([stage (load 'IoA "saves/01/STGDAT01.bin")])
      (check-equal? (blocks-hash stage) '(8305176 9691690))
      (clear-area! stage 'all #:keep-items? #f)
      (repair-sea! stage 'all)
      (check-equal? (blocks-hash stage) '(5119984 1574160)))
  }
