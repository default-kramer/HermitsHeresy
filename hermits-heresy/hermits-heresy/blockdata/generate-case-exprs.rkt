#lang racket

; Provide nothing.
; Just manually copy-paste the end result into a macro elsewhere.
; (Of course I could do this during macro expansion, but I'd
;  rather see the diff in source control whenever I update it.)

(require json
         racket/struct)

(module+ test (require rackunit))

(define str (file->string "Sapphire645-Tiles.json"))
(define data (hash-ref (string->jsexpr str) 'Tiles))

(struct jblock (id name) #:transparent)

(define (hash->jblock h)
  (jblock (hash-ref h 'Id)
          (hash-ref h 'Name)))

(define jblocks (map hash->jblock data))

(set! jblocks (filter (lambda (b) (> (jblock-id b) 0)) jblocks))
(set! jblocks (sort jblocks (lambda (b1 b2) (< (jblock-id b1) (jblock-id b2)))))

(define (jblock->symbol jb)
  (let* ([name (jblock-name jb)]
         [name (string-replace name "(" "")]
         [name (string-replace name ")" "")]
         [name (string-replace name " " "-")]
         [name (string-replace name "*" "")])
    (string->symbol name)))

(struct block (id symbol lower-name liquid? star? dyes) #:transparent)

(define (jblock->block jb)
  (define lower-name (string-downcase (jblock-name jb)))
  (define liquid? (ormap (lambda (str) (string-contains? lower-name str))
                         (list "clear water"
                               "sea water"
                               "bottomless swamp"
                               "muddy water"
                               "filthy water"
                               "hot water"
                               "lava shallow"
                               "lava surface"
                               "lava full"
                               "liquid lava"
                               "poison shallow"
                               "poison surface"
                               "poison full"
                               "poison small"
                               "plasma shallow"
                               "plasma surface"
                               "plasma full"
                               "plasma small"
                               )))
  (define star? (string-suffix? (jblock-name jb) "*"))
  (block (jblock-id jb)
         (jblock->symbol jb)
         lower-name
         liquid?
         star?
         (list) ; dyes, will be parsed later
         ))

(define blocks (map jblock->block jblocks))

(set! blocks (filter (lambda (blk)
                       ; skip blocks I don't understand for now
                       (not (member (block-symbol blk) '(Default-Block
                                                         Air?
                                                         White-Unused-Block
                                                         Item))))
                     blocks))


(define (get-name+dye lower-name)
  (define patterns '(("{green}" . green)
                     ("{blue}" . blue)
                     ("{yellow}" . yellow)
                     ("{black}" . black)
                     ("{white}" . white)
                     ("{red}" . red)
                     ("{pink}" . pink)
                     ("{purple}" . purple)))
  (ormap (lambda (pattern)
           (let* ([suffix (car pattern)]
                  [dye (cdr pattern)])
             (and (string-suffix? lower-name suffix)
                  (list (string-trim (string-replace lower-name suffix ""))
                        dye))))
         patterns))

(define dyes
  (map (lambda (blk)
         (let* ([result (get-name+dye (block-lower-name blk))])
           (and result
                (cons blk result))))
       blocks))
(set! dyes (filter identity dyes))

(set! blocks
      (let ([temp (flatten dyes)])
        (filter (lambda (blk) (not (member blk temp))) blocks)))

(define (get-dyes blk)
  (define name (block-lower-name blk))
  (define matches (filter (lambda (item) (equal? name (second item))) dyes))
  (map (lambda (item) (cons (third item)
                            (block-id (first item))))
       matches))

(set! blocks
      (map (lambda (blk) (struct-copy block blk
                                      [dyes (get-dyes blk)]))
           blocks))

; Just do some sanity checks
{module+ test
  (define (get-block id)
    (ormap (lambda (blk) (and (= id (block-id blk)) blk)) blocks))

  (let ([sand (get-block 11)])
    (check-equal? (block-symbol sand) 'Sand)
    (check-equal? (block-dyes sand) (list)))

  (let ([vintage (get-block 822)])
    (check-equal? (block-symbol vintage) 'Vintage-Wall)
    (check-equal? (block-dyes vintage) '((white . 850)
                                         (black . 851)
                                         (purple . 852)
                                         (pink . 853)
                                         (red . 854)
                                         (green . 855)
                                         (yellow . 856)
                                         (blue . 857))))

  (let ([golemite (get-block 61)])
    (check-equal? (block-symbol golemite) 'Golemite)
    (check-equal? (block-star? golemite) #t)
    (check-equal? (block-dyes golemite) (list)))
  }


; Generate an s-expr to be pasted into a `case` expression.
(define output (for/list ([block blocks])
                 (struct->list block)))
