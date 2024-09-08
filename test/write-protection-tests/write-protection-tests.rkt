#lang racket

(require hermits-heresy)

{module+ test
  (require rackunit)

  (define here (current-directory))
  (save-dir here)

  ; We gitignore .BIN files so we only have to keep one copy in source control,
  ; and before running these tests we copy that file into all subdirectories:
  (for ([subdir '(B00 B01 B02 missing-config-file)])
    (copy-file (build-path here "STGDAT01.BIN")
               (build-path here (~a subdir) "STGDAT01.BIN")
               #:exists-ok? #t))

  ; B00 is writable
  (let ([stage (load-stage 'IoA (build-path here "B00" "STGDAT01.BIN"))])
    (check-not-exn  (lambda () (save-stage! stage))))

  ; B01 is not writable
  (let ([stage (load-stage 'IoA (build-path here "B01" "STGDAT01.BIN"))])
    (check-exn #rx"Directory is marked as not writable per:"
               (lambda () (save-stage! stage)))
    (check-exn #rx"Directory is marked as not writable per:"
               (lambda () (copy-all-save-files! #:from 'B00 #:to 'B01))))

  ; B02 has bad json
  (let ([stage (load-stage 'IoA (build-path here "B02" "STGDAT01.BIN"))])
    (check-exn #rx"Directory is not writable, config file is not valid json:"
               (lambda () (save-stage! stage)))
    (check-exn #rx"Directory is not writable, config file is not valid json:"
               (lambda () (copy-all-save-files! #:from 'B00 #:to 'B02))))

  ; Cannot write when config file is missing
  (let ([stage (load-stage 'IoA (build-path here "missing-config-file" "STGDAT01.BIN"))])
    (check-exn #rx"Directory is not writable, config file is missing:"
               (lambda () (save-stage! stage))))

  ; Test that copy-all-save-files! does not trample the config file or anything else
  (let ([msg (with-output-to-string
               (lambda () (copy-all-save-files! #:from 'B01 #:to 'B00)))]
        [B00-dnc (file->string (build-path here "B00" "DO-NOT-COPY.BIN"))]
        [B01-dnc (file->string (build-path here "B01" "DO-NOT-COPY.BIN"))]
        ; The SCSHDAT.BIN file should never be modified outside of DQB2.
        ; This holds photos and uploaded island info for the save slot
        ; which is expected to match what is on the official servers.
        [B00-scsh (file->string (build-path here "B00" "SCSHDAT.BIN"))]
        [B01-scsh (file->string (build-path here "B01" "SCSHDAT.BIN"))])
    (check-true (string-contains? msg "Copied 1 files from"))
    (check-equal? B00-dnc "B00")
    (check-equal? B01-dnc "B01")
    (check-equal? B00-scsh "B00")
    (check-equal? B01-scsh "B01"))
  }