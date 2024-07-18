#lang typed/racket

(provide assert-directory-writable)

(require typed/json)

(: assert-directory-writable (-> Path-String #t))
(define (assert-directory-writable dir)
  (define config-file (build-path dir "hermits-heresy.config.json"))
  (when (not (file-exists? config-file))
    (error "Directory is not writable, config file is missing:" config-file))
  (define json (file->string config-file))
  (case (writable? json)
    [(yes) #t]
    [(no) (error "Directory is marked as not writable per:" config-file)]
    [(bad-json) (error "Directory is not writable, config file is not valid json:" config-file)]))

(: writable? (-> String (U 'yes 'no 'bad-json)))
(define (writable? json)
  (with-handlers ([exn:fail:read? (lambda (x) 'bad-json)])
    (define jx (string->jsexpr json))
    (or (and (hash? jx)
             (case (hash-ref jx 'writable (lambda () #f))
               [(#t) 'yes]
               [else 'no]))
        'no)))

{module+ test
  (require typed/rackunit)
  (check-equal? (writable? "{\"writable\": 0}") 'no)
  (check-equal? (writable? "{\"writable\": false}") 'no)
  (check-equal? (writable? "{\"writable\": \"false\"}") 'no)
  (check-equal? (writable? "{\"writable\": true}") 'yes)
  (check-equal? (writable? "{\"something\": true}") 'no)
  (check-equal? (writable? "bad json") 'bad-json)
  }
