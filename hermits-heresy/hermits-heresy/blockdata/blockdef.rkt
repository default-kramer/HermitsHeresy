#lang typed/racket

(provide Dye Blockdef symbol->blockdef fixnum->blockdef
         (struct-out blockdef)
         find-block
         symbol->ambiguities)

(require "blockdef-raw.rkt"
         "../ufx.rkt")

(module+ test (require typed/rackunit))

(require/typed levenshtein [string-levenshtein (-> String String Integer)])

(define all-blockdefs : (Listof Blockdef)
  (let ([temp (for/list : (Listof (U #f Blockdef))
                ([i : Fixnum (ufx-in-range 2047)])
                (fixnum->blockdef i))])
    (filter blockdef? temp)))

(define all-symbols (remove-duplicates (map blockdef-symbol all-blockdefs)))

(define ambiguous-symbols : (Immutable-HashTable Symbol (Listof Fixnum))
  (let* ([groups : (Listof (Listof Blockdef))
                 (group-by blockdef-symbol all-blockdefs)]
         [groups (filter (lambda ([grp : (Listof Any)])
                           (not (empty? (cdr grp))))
                         groups)]
         [pairs : (Listof (Pairof Symbol (Listof Fixnum)))
                (map (lambda ([grp : (Listof Blockdef)])
                       (let ([a (blockdef-symbol (first grp))]
                             [b (map blockdef-id grp)])
                         (ann (cons a b)
                              (Pairof Symbol (Listof Fixnum)))))
                     groups)])
    (make-immutable-hash pairs)))

(: symbol->ambiguities (-> Symbol (U #f (Listof Fixnum))))
(define (symbol->ambiguities sym)
  (hash-ref ambiguous-symbols sym (lambda () #f)))


(: find-block (-> String (U #t #f 'auto) (List (Listof Symbol) (Listof (List Symbol Integer)))))
; Returns (list exact-matches other-matches)
(define (find-block name-in include-approx?)
  (define (normalize [str : String])
    (let* ([str (string-replace str "-" "")]
           [str (string-replace str " " "")])
      (string-downcase str)))
  (define want (normalize name-in))
  (define want-len (string-length want))

  ; Needed because raw usage of levenshtein says "pioson" is closer to "snow"
  ; than to "Poison-Shallow-Block" because of the string length difference.
  (define (repeated-levenshtein [s : Symbol])
    (define str (normalize (~a s)))
    (define str-len (string-length str))
    (if (< str-len want-len)
        (string-levenshtein want str)
        (let ([best : Integer 999])
          (for ([start : Integer (in-range (- str-len want-len))])
            (let* ([trimmed (substring str start (+ start want-len))]
                   [dist (string-levenshtein want trimmed)])
              (set! best (min best dist))))
          best)))

  (define exact-matches
    (for/fold : (Listof Symbol)
      ([result : (Listof Symbol) (list)])
      ([sym all-symbols])
      (if (string-contains? (normalize (~a sym)) want)
          (cons sym result)
          result)))
  (define do-approx? (case include-approx?
                       [(auto) (empty? exact-matches)]
                       [else (ann include-approx? Boolean)]))
  (define approx-matches : (Listof (List Symbol Integer))
    (if (not do-approx?)
        (list)
        (let* ([calc (map (lambda ([s : Symbol])
                            (list s (repeated-levenshtein s)))
                          (filter (lambda (s) (not (member s exact-matches)))
                                  all-symbols))]
               [calc (sort calc (lambda ([x : (List Symbol Integer)]
                                         [y : (List Symbol Integer)])
                                  (< (second x) (second y))))]
               [best-score (match calc
                             [(list (list sym score) more ...)
                              score]
                             [else 999])])
          (if (< best-score 999)
              (takef calc (lambda ([x : (List Symbol Integer)])
                            (= best-score (second x))))
              (list)))))
  (list exact-matches approx-matches))

{module+ test
  (let* ([result (find-block "posion" 'auto)]
         [exact (first result)]
         [approx (second result)])
    (check-equal? exact (list))
    (check-equal? approx '((Poisonous-Peat 2)
                           (Poison-shallow-block 2)
                           (Poison-surface-block 2)
                           (Poison-full-block 2)
                           (Poison-Full-Block 2)
                           (Poison-Shallow-Block 2)
                           (Poison-Surface-Block 2)
                           (Poison-Small-Block 2))))
  }
