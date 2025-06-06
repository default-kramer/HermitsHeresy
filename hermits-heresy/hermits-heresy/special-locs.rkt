#lang typed/racket

(require "basics.rkt")

(define-syntax-rule (define-special-locs id
                      (-LIST- (-XZ- x z) y blockid)
                      ...)
  (begin
    (provide id)
    (define id : (Listof Point)
      (list (make-point (xz x z) y)
            ...))))

{module+ IoA

  ; The flat 3x3 hammer decorating the ground on the hermit's mountain.
  ; Indestructible.
  ; Block = 2047, Item = 1410
  (define-special-locs hermits-hammer
    (list (xz 414 284) 74 2047)
    (list (xz 415 284) 74 2047)
    (list (xz 416 284) 74 2047)
    (list (xz 414 285) 74 2047)
    (list (xz 415 285) 74 2047)
    (list (xz 416 285) 74 2047)
    (list (xz 414 286) 74 2047)
    (list (xz 415 286) 74 2047)
    (list (xz 416 286) 74 2047))

  ; Indestructible.
  (define-special-locs blue-tablet
    (list (xz 371 110) 53 2047)
    (list (xz 371 110) 54 2047)
    (list (xz 371 110) 55 2047)
    (list (xz 371 110) 56 2047)
    (list (xz 372 110) 53 2047)
    (list (xz 372 110) 54 2047)
    (list (xz 372 110) 55 2047)
    (list (xz 372 110) 56 2047)
    (list (xz 373 110) 53 2047)
    (list (xz 373 110) 54 2047)
    (list (xz 373 110) 55 2047)
    (list (xz 373 110) 56 2047))

  ; Indestructible.
  ; Block = 2047, Item = 2565
  (define-special-locs green-tablet
    (list (xz 519 114) 44 2047)
    (list (xz 519 114) 45 2047)
    (list (xz 519 114) 46 2047)
    (list (xz 519 114) 47 2047)
    (list (xz 520 114) 44 2047)
    (list (xz 520 114) 45 2047)
    (list (xz 520 114) 46 2047)
    (list (xz 520 114) 47 2047)
    (list (xz 521 114) 44 2047)
    (list (xz 521 114) 45 2047)
    (list (xz 521 114) 46 2047)
    (list (xz 521 114) 47 2047))

  ; Indestructible.
  ; Block = 2047, Item = 2569
  (define-special-locs red-tablet
    (list (xz 553 287) 43 2047)
    (list (xz 553 287) 44 2047)
    (list (xz 553 287) 45 2047)
    (list (xz 553 287) 46 2047)
    (list (xz 554 287) 43 2047)
    (list (xz 554 287) 44 2047)
    (list (xz 554 287) 45 2047)
    (list (xz 554 287) 46 2047)
    (list (xz 555 287) 43 2047)
    (list (xz 555 287) 44 2047)
    (list (xz 555 287) 45 2047)
    (list (xz 555 287) 46 2047))

  ; The 16x16 area that you can build your flag on.
  ; Indestructible.
  (define-special-locs flag-floor
    (list (xz 405 251) 90 26)
    (list (xz 406 251) 90 26)
    (list (xz 407 251) 90 26)
    (list (xz 408 251) 90 28)
    (list (xz 409 251) 90 26)
    (list (xz 410 251) 90 26)
    (list (xz 411 251) 90 26)
    (list (xz 412 251) 90 26)
    (list (xz 413 251) 90 26)
    (list (xz 414 251) 90 26)
    (list (xz 415 251) 90 28)
    (list (xz 416 251) 90 27)
    (list (xz 417 251) 90 26)
    (list (xz 418 251) 90 26)
    (list (xz 419 251) 90 26)
    (list (xz 420 251) 90 26)
    (list (xz 405 252) 90 26)
    (list (xz 406 252) 90 401)
    (list (xz 407 252) 90 401)
    (list (xz 408 252) 90 401)
    (list (xz 409 252) 90 401)
    (list (xz 410 252) 90 401)
    (list (xz 411 252) 90 401)
    (list (xz 412 252) 90 401)
    (list (xz 413 252) 90 401)
    (list (xz 414 252) 90 401)
    (list (xz 415 252) 90 401)
    (list (xz 416 252) 90 401)
    (list (xz 417 252) 90 401)
    (list (xz 418 252) 90 401)
    (list (xz 419 252) 90 401)
    (list (xz 420 252) 90 28)
    (list (xz 405 253) 90 26)
    (list (xz 406 253) 90 401)
    (list (xz 407 253) 90 28)
    (list (xz 408 253) 90 26)
    (list (xz 409 253) 90 26)
    (list (xz 410 253) 90 26)
    (list (xz 411 253) 90 26)
    (list (xz 412 253) 90 27)
    (list (xz 413 253) 90 26)
    (list (xz 414 253) 90 26)
    (list (xz 415 253) 90 26)
    (list (xz 416 253) 90 26)
    (list (xz 417 253) 90 26)
    (list (xz 418 253) 90 28)
    (list (xz 419 253) 90 401)
    (list (xz 420 253) 90 28)
    (list (xz 405 254) 90 26)
    (list (xz 406 254) 90 401)
    (list (xz 407 254) 90 28)
    (list (xz 408 254) 90 27)
    (list (xz 409 254) 90 26)
    (list (xz 410 254) 90 26)
    (list (xz 411 254) 90 26)
    (list (xz 412 254) 90 28)
    (list (xz 413 254) 90 26)
    (list (xz 414 254) 90 26)
    (list (xz 415 254) 90 26)
    (list (xz 416 254) 90 26)
    (list (xz 417 254) 90 28)
    (list (xz 418 254) 90 28)
    (list (xz 419 254) 90 401)
    (list (xz 420 254) 90 26)
    (list (xz 405 255) 90 26)
    (list (xz 406 255) 90 401)
    (list (xz 407 255) 90 26)
    (list (xz 408 255) 90 26)
    (list (xz 409 255) 90 401)
    (list (xz 410 255) 90 401)
    (list (xz 411 255) 90 26)
    (list (xz 412 255) 90 28)
    (list (xz 413 255) 90 28)
    (list (xz 414 255) 90 26)
    (list (xz 415 255) 90 401)
    (list (xz 416 255) 90 401)
    (list (xz 417 255) 90 26)
    (list (xz 418 255) 90 26)
    (list (xz 419 255) 90 401)
    (list (xz 420 255) 90 26)
    (list (xz 405 256) 90 28)
    (list (xz 406 256) 90 401)
    (list (xz 407 256) 90 26)
    (list (xz 408 256) 90 28)
    (list (xz 409 256) 90 401)
    (list (xz 410 256) 90 401)
    (list (xz 411 256) 90 26)
    (list (xz 412 256) 90 26)
    (list (xz 413 256) 90 28)
    (list (xz 414 256) 90 26)
    (list (xz 415 256) 90 401)
    (list (xz 416 256) 90 401)
    (list (xz 417 256) 90 28)
    (list (xz 418 256) 90 26)
    (list (xz 419 256) 90 401)
    (list (xz 420 256) 90 26)
    (list (xz 405 257) 90 28)
    (list (xz 406 257) 90 401)
    (list (xz 407 257) 90 28)
    (list (xz 408 257) 90 26)
    (list (xz 409 257) 90 26)
    (list (xz 410 257) 90 26)
    (list (xz 411 257) 90 401)
    (list (xz 412 257) 90 401)
    (list (xz 413 257) 90 401)
    (list (xz 414 257) 90 401)
    (list (xz 415 257) 90 26)
    (list (xz 416 257) 90 26)
    (list (xz 417 257) 90 28)
    (list (xz 418 257) 90 28)
    (list (xz 419 257) 90 401)
    (list (xz 420 257) 90 28)
    (list (xz 405 258) 90 26)
    (list (xz 406 258) 90 401)
    (list (xz 407 258) 90 26)
    (list (xz 408 258) 90 26)
    (list (xz 409 258) 90 26)
    (list (xz 410 258) 90 26)
    (list (xz 411 258) 90 401)
    (list (xz 412 258) 90 27)
    (list (xz 413 258) 90 28)
    (list (xz 414 258) 90 401)
    (list (xz 415 258) 90 26)
    (list (xz 416 258) 90 26)
    (list (xz 417 258) 90 26)
    (list (xz 418 258) 90 27)
    (list (xz 419 258) 90 401)
    (list (xz 420 258) 90 28)
    (list (xz 405 259) 90 26)
    (list (xz 406 259) 90 401)
    (list (xz 407 259) 90 27)
    (list (xz 408 259) 90 26)
    (list (xz 409 259) 90 28)
    (list (xz 410 259) 90 28)
    (list (xz 411 259) 90 401)
    (list (xz 412 259) 90 26)
    (list (xz 413 259) 90 26)
    (list (xz 414 259) 90 401)
    (list (xz 415 259) 90 26)
    (list (xz 416 259) 90 26)
    (list (xz 417 259) 90 26)
    (list (xz 418 259) 90 27)
    (list (xz 419 259) 90 401)
    (list (xz 420 259) 90 26)
    (list (xz 405 260) 90 28)
    (list (xz 406 260) 90 401)
    (list (xz 407 260) 90 28)
    (list (xz 408 260) 90 26)
    (list (xz 409 260) 90 26)
    (list (xz 410 260) 90 28)
    (list (xz 411 260) 90 401)
    (list (xz 412 260) 90 401)
    (list (xz 413 260) 90 401)
    (list (xz 414 260) 90 401)
    (list (xz 415 260) 90 28)
    (list (xz 416 260) 90 26)
    (list (xz 417 260) 90 26)
    (list (xz 418 260) 90 26)
    (list (xz 419 260) 90 401)
    (list (xz 420 260) 90 26)
    (list (xz 405 261) 90 26)
    (list (xz 406 261) 90 401)
    (list (xz 407 261) 90 26)
    (list (xz 408 261) 90 26)
    (list (xz 409 261) 90 401)
    (list (xz 410 261) 90 401)
    (list (xz 411 261) 90 26)
    (list (xz 412 261) 90 26)
    (list (xz 413 261) 90 26)
    (list (xz 414 261) 90 27)
    (list (xz 415 261) 90 401)
    (list (xz 416 261) 90 401)
    (list (xz 417 261) 90 27)
    (list (xz 418 261) 90 26)
    (list (xz 419 261) 90 401)
    (list (xz 420 261) 90 26)
    (list (xz 405 262) 90 26)
    (list (xz 406 262) 90 401)
    (list (xz 407 262) 90 26)
    (list (xz 408 262) 90 28)
    (list (xz 409 262) 90 401)
    (list (xz 410 262) 90 401)
    (list (xz 411 262) 90 26)
    (list (xz 412 262) 90 26)
    (list (xz 413 262) 90 28)
    (list (xz 414 262) 90 26)
    (list (xz 415 262) 90 401)
    (list (xz 416 262) 90 401)
    (list (xz 417 262) 90 26)
    (list (xz 418 262) 90 26)
    (list (xz 419 262) 90 401)
    (list (xz 420 262) 90 28)
    (list (xz 405 263) 90 28)
    (list (xz 406 263) 90 401)
    (list (xz 407 263) 90 26)
    (list (xz 408 263) 90 28)
    (list (xz 409 263) 90 28)
    (list (xz 410 263) 90 27)
    (list (xz 411 263) 90 26)
    (list (xz 412 263) 90 26)
    (list (xz 413 263) 90 28)
    (list (xz 414 263) 90 28)
    (list (xz 415 263) 90 26)
    (list (xz 416 263) 90 26)
    (list (xz 417 263) 90 26)
    (list (xz 418 263) 90 26)
    (list (xz 419 263) 90 401)
    (list (xz 420 263) 90 28)
    (list (xz 405 264) 90 28)
    (list (xz 406 264) 90 401)
    (list (xz 407 264) 90 26)
    (list (xz 408 264) 90 26)
    (list (xz 409 264) 90 28)
    (list (xz 410 264) 90 26)
    (list (xz 411 264) 90 28)
    (list (xz 412 264) 90 26)
    (list (xz 413 264) 90 26)
    (list (xz 414 264) 90 28)
    (list (xz 415 264) 90 26)
    (list (xz 416 264) 90 26)
    (list (xz 417 264) 90 28)
    (list (xz 418 264) 90 28)
    (list (xz 419 264) 90 401)
    (list (xz 420 264) 90 26)
    (list (xz 405 265) 90 26)
    (list (xz 406 265) 90 401)
    (list (xz 407 265) 90 401)
    (list (xz 408 265) 90 401)
    (list (xz 409 265) 90 401)
    (list (xz 410 265) 90 401)
    (list (xz 411 265) 90 401)
    (list (xz 412 265) 90 401)
    (list (xz 413 265) 90 401)
    (list (xz 414 265) 90 401)
    (list (xz 415 265) 90 401)
    (list (xz 416 265) 90 401)
    (list (xz 417 265) 90 401)
    (list (xz 418 265) 90 401)
    (list (xz 419 265) 90 401)
    (list (xz 420 265) 90 26)
    (list (xz 405 266) 90 26)
    (list (xz 406 266) 90 26)
    (list (xz 407 266) 90 26)
    (list (xz 408 266) 90 26)
    (list (xz 409 266) 90 28)
    (list (xz 410 266) 90 28)
    (list (xz 411 266) 90 26)
    (list (xz 412 266) 90 27)
    (list (xz 413 266) 90 26)
    (list (xz 414 266) 90 28)
    (list (xz 415 266) 90 28)
    (list (xz 416 266) 90 26)
    (list (xz 417 266) 90 26)
    (list (xz 418 266) 90 28)
    (list (xz 419 266) 90 28)
    (list (xz 420 266) 90 26))

  ; Indestructible.
  (define-special-locs storage-sack
    (list (xz 447 513) 33 2047)
    (list (xz 447 513) 34 2047))

  ; Indestructible.
  (define-special-locs ship
    (list (xz 446 513) 36 2047)
    (list (xz 446 513) 37 2047)
    (list (xz 446 513) 38 2047)
    (list (xz 446 513) 39 2047)
    (list (xz 446 513) 40 2047)
    (list (xz 446 513) 41 2047)
    (list (xz 446 513) 42 2047)
    (list (xz 446 513) 43 2047)
    (list (xz 446 513) 44 2047)
    (list (xz 441 514) 31 2047)
    (list (xz 441 514) 32 2047)
    (list (xz 441 514) 33 2047)
    (list (xz 442 514) 31 2047)
    (list (xz 442 514) 32 2047)
    (list (xz 442 514) 33 2047)
    (list (xz 443 514) 31 2047)
    (list (xz 443 514) 32 2047)
    (list (xz 443 514) 33 2047)
    (list (xz 444 514) 31 2047)
    (list (xz 444 514) 32 2047)
    (list (xz 444 514) 33 2047)
    (list (xz 445 514) 31 2047)
    (list (xz 445 514) 32 2047)
    (list (xz 445 514) 33 2047)
    (list (xz 446 514) 31 2047)
    (list (xz 446 514) 32 2047)
    (list (xz 446 514) 33 2047)
    (list (xz 446 514) 36 2047)
    (list (xz 446 514) 37 2047)
    (list (xz 446 514) 38 2047)
    (list (xz 446 514) 39 2047)
    (list (xz 446 514) 40 2047)
    (list (xz 446 514) 41 2047)
    (list (xz 446 514) 42 2047)
    (list (xz 446 514) 43 2047)
    (list (xz 446 514) 44 2047)
    (list (xz 447 514) 31 2047)
    (list (xz 447 514) 32 2047)
    (list (xz 447 514) 33 2047)
    (list (xz 448 514) 31 2047)
    (list (xz 448 514) 32 2047)
    (list (xz 448 514) 33 2047)
    (list (xz 449 514) 31 2047)
    (list (xz 449 514) 32 2047)
    (list (xz 449 514) 33 2047)
    (list (xz 450 514) 31 2047)
    (list (xz 450 514) 32 2047)
    (list (xz 451 514) 31 2047)
    (list (xz 451 514) 32 2047)
    (list (xz 451 514) 33 2047)
    (list (xz 452 514) 31 2047)
    (list (xz 452 514) 32 2047)
    (list (xz 452 514) 33 2047)
    (list (xz 453 514) 31 2047)
    (list (xz 453 514) 32 2047)
    (list (xz 453 514) 33 2047)
    (list (xz 441 515) 31 2047)
    (list (xz 441 515) 32 2047)
    (list (xz 441 515) 33 2047)
    (list (xz 442 515) 29 2047)
    (list (xz 442 515) 30 2047)
    (list (xz 442 515) 31 2047)
    (list (xz 442 515) 32 2047)
    (list (xz 442 515) 33 2047)
    (list (xz 443 515) 29 2047)
    (list (xz 443 515) 30 2047)
    (list (xz 443 515) 31 2047)
    (list (xz 443 515) 32 2047)
    (list (xz 443 515) 33 2047)
    (list (xz 444 515) 29 2047)
    (list (xz 444 515) 30 2047)
    (list (xz 444 515) 31 2047)
    (list (xz 444 515) 32 2047)
    (list (xz 445 515) 29 2047)
    (list (xz 445 515) 30 2047)
    (list (xz 445 515) 31 2047)
    (list (xz 445 515) 32 2047)
    (list (xz 446 515) 29 2047)
    (list (xz 446 515) 30 2047)
    (list (xz 446 515) 31 2047)
    (list (xz 446 515) 32 2047)
    (list (xz 446 515) 36 2047)
    (list (xz 446 515) 37 2047)
    (list (xz 446 515) 38 2047)
    (list (xz 446 515) 39 2047)
    (list (xz 446 515) 40 2047)
    (list (xz 446 515) 41 2047)
    (list (xz 446 515) 42 2047)
    (list (xz 446 515) 43 2047)
    (list (xz 446 515) 44 2047)
    (list (xz 447 515) 29 2047)
    (list (xz 447 515) 30 2047)
    (list (xz 447 515) 31 2047)
    (list (xz 447 515) 32 2047)
    (list (xz 448 515) 29 2047)
    (list (xz 448 515) 30 2047)
    (list (xz 448 515) 31 2047)
    (list (xz 448 515) 32 2047)
    (list (xz 449 515) 29 2047)
    (list (xz 449 515) 30 2047)
    (list (xz 449 515) 31 2047)
    (list (xz 449 515) 32 2047)
    (list (xz 450 515) 29 2047)
    (list (xz 450 515) 30 2047)
    (list (xz 450 515) 31 2047)
    (list (xz 450 515) 32 2047)
    (list (xz 451 515) 29 2047)
    (list (xz 451 515) 30 2047)
    (list (xz 451 515) 31 2047)
    (list (xz 451 515) 32 2047)
    (list (xz 452 515) 29 2047)
    (list (xz 452 515) 30 2047)
    (list (xz 452 515) 31 2047)
    (list (xz 452 515) 32 2047)
    (list (xz 453 515) 29 2047)
    (list (xz 453 515) 30 2047)
    (list (xz 453 515) 31 2047)
    (list (xz 453 515) 32 2047)
    (list (xz 453 515) 33 2047)
    (list (xz 454 515) 30 2047)
    (list (xz 454 515) 31 2047)
    (list (xz 454 515) 32 2047)
    (list (xz 454 515) 33 2047)
    (list (xz 454 515) 34 2047)
    (list (xz 441 516) 31 2047)
    (list (xz 441 516) 32 2047)
    (list (xz 441 516) 33 2047)
    (list (xz 442 516) 29 2047)
    (list (xz 442 516) 30 2047)
    (list (xz 442 516) 31 2047)
    (list (xz 442 516) 32 2047)
    (list (xz 443 516) 29 2047)
    (list (xz 443 516) 30 2047)
    (list (xz 443 516) 31 2047)
    (list (xz 443 516) 32 2047)
    (list (xz 444 516) 29 2047)
    (list (xz 444 516) 30 2047)
    (list (xz 444 516) 31 2047)
    (list (xz 444 516) 32 2047)
    (list (xz 445 516) 29 2047)
    (list (xz 445 516) 30 2047)
    (list (xz 445 516) 31 2047)
    (list (xz 445 516) 32 2047)
    (list (xz 446 516) 29 2047)
    (list (xz 446 516) 30 2047)
    (list (xz 446 516) 31 2047)
    (list (xz 446 516) 32 2047)
    (list (xz 446 516) 36 2047)
    (list (xz 446 516) 37 2047)
    (list (xz 446 516) 38 2047)
    (list (xz 446 516) 39 2047)
    (list (xz 446 516) 40 2047)
    (list (xz 446 516) 41 2047)
    (list (xz 446 516) 42 2047)
    (list (xz 446 516) 43 2047)
    (list (xz 446 516) 44 2047)
    (list (xz 447 516) 29 2047)
    (list (xz 447 516) 30 2047)
    (list (xz 447 516) 31 2047)
    (list (xz 447 516) 32 2047)
    (list (xz 448 516) 29 2047)
    (list (xz 448 516) 30 2047)
    (list (xz 448 516) 31 2047)
    (list (xz 448 516) 32 2047)
    (list (xz 449 516) 29 2047)
    (list (xz 449 516) 30 2047)
    (list (xz 449 516) 31 2047)
    (list (xz 449 516) 32 2047)
    (list (xz 450 516) 29 2047)
    (list (xz 450 516) 30 2047)
    (list (xz 450 516) 31 2047)
    (list (xz 450 516) 32 2047)
    (list (xz 451 516) 29 2047)
    (list (xz 451 516) 30 2047)
    (list (xz 451 516) 31 2047)
    (list (xz 451 516) 32 2047)
    (list (xz 452 516) 29 2047)
    (list (xz 452 516) 30 2047)
    (list (xz 452 516) 31 2047)
    (list (xz 452 516) 32 2047)
    (list (xz 453 516) 29 2047)
    (list (xz 453 516) 30 2047)
    (list (xz 453 516) 31 2047)
    (list (xz 453 516) 32 2047)
    (list (xz 454 516) 30 2047)
    (list (xz 454 516) 31 2047)
    (list (xz 454 516) 32 2047)
    (list (xz 454 516) 33 2047)
    (list (xz 454 516) 34 2047)
    (list (xz 440 517) 29 2047)
    (list (xz 440 517) 30 2047)
    (list (xz 440 517) 31 2047)
    (list (xz 441 517) 29 2047)
    (list (xz 441 517) 30 2047)
    (list (xz 441 517) 31 2047)
    (list (xz 441 517) 32 2047)
    (list (xz 441 517) 33 2047)
    (list (xz 441 517) 34 2047)
    (list (xz 442 517) 29 2047)
    (list (xz 442 517) 30 2047)
    (list (xz 442 517) 31 2047)
    (list (xz 442 517) 32 2047)
    (list (xz 443 517) 29 2047)
    (list (xz 443 517) 30 2047)
    (list (xz 443 517) 31 2047)
    (list (xz 443 517) 32 2047)
    (list (xz 444 517) 29 2047)
    (list (xz 444 517) 30 2047)
    (list (xz 444 517) 31 2047)
    (list (xz 444 517) 32 2047)
    (list (xz 445 517) 29 2047)
    (list (xz 445 517) 30 2047)
    (list (xz 445 517) 31 2047)
    (list (xz 445 517) 32 2047)
    (list (xz 445 517) 33 2047)
    (list (xz 445 517) 34 2047)
    (list (xz 445 517) 35 2047)
    (list (xz 445 517) 36 2047)
    (list (xz 445 517) 37 2047)
    (list (xz 445 517) 38 2047)
    (list (xz 445 517) 39 2047)
    (list (xz 445 517) 40 2047)
    (list (xz 445 517) 41 2047)
    (list (xz 445 517) 42 2047)
    (list (xz 445 517) 43 2047)
    (list (xz 445 517) 44 2047)
    (list (xz 445 517) 45 2047)
    (list (xz 446 517) 29 2047)
    (list (xz 446 517) 30 2047)
    (list (xz 446 517) 31 2047)
    (list (xz 446 517) 32 2047)
    (list (xz 446 517) 36 2047)
    (list (xz 446 517) 37 2047)
    (list (xz 446 517) 38 2047)
    (list (xz 446 517) 39 2047)
    (list (xz 446 517) 40 2047)
    (list (xz 446 517) 41 2047)
    (list (xz 446 517) 42 2047)
    (list (xz 446 517) 43 2047)
    (list (xz 446 517) 44 2047)
    (list (xz 447 517) 29 2047)
    (list (xz 447 517) 30 2047)
    (list (xz 447 517) 31 2047)
    (list (xz 447 517) 32 2047)
    (list (xz 448 517) 29 2047)
    (list (xz 448 517) 30 2047)
    (list (xz 448 517) 31 2047)
    (list (xz 448 517) 32 2047)
    (list (xz 449 517) 29 2047)
    (list (xz 449 517) 30 2047)
    (list (xz 449 517) 31 2047)
    (list (xz 449 517) 32 2047)
    (list (xz 450 517) 29 2047)
    (list (xz 450 517) 30 2047)
    (list (xz 450 517) 31 2047)
    (list (xz 450 517) 32 2047)
    (list (xz 451 517) 29 2047)
    (list (xz 451 517) 30 2047)
    (list (xz 451 517) 31 2047)
    (list (xz 451 517) 32 2047)
    (list (xz 452 517) 29 2047)
    (list (xz 452 517) 30 2047)
    (list (xz 452 517) 31 2047)
    (list (xz 452 517) 32 2047)
    (list (xz 453 517) 29 2047)
    (list (xz 453 517) 30 2047)
    (list (xz 453 517) 31 2047)
    (list (xz 453 517) 32 2047)
    (list (xz 454 517) 30 2047)
    (list (xz 454 517) 31 2047)
    (list (xz 454 517) 32 2047)
    (list (xz 454 517) 33 2047)
    (list (xz 454 517) 34 2047)
    (list (xz 455 517) 32 2047)
    (list (xz 455 517) 33 2047)
    (list (xz 455 517) 34 2047)
    (list (xz 441 518) 31 2047)
    (list (xz 441 518) 32 2047)
    (list (xz 441 518) 33 2047)
    (list (xz 442 518) 29 2047)
    (list (xz 442 518) 30 2047)
    (list (xz 442 518) 31 2047)
    (list (xz 442 518) 32 2047)
    (list (xz 442 518) 33 2047)
    (list (xz 443 518) 29 2047)
    (list (xz 443 518) 30 2047)
    (list (xz 443 518) 31 2047)
    (list (xz 443 518) 32 2047)
    (list (xz 444 518) 29 2047)
    (list (xz 444 518) 30 2047)
    (list (xz 444 518) 31 2047)
    (list (xz 444 518) 32 2047)
    (list (xz 445 518) 29 2047)
    (list (xz 445 518) 30 2047)
    (list (xz 445 518) 31 2047)
    (list (xz 445 518) 32 2047)
    (list (xz 446 518) 29 2047)
    (list (xz 446 518) 30 2047)
    (list (xz 446 518) 31 2047)
    (list (xz 446 518) 32 2047)
    (list (xz 446 518) 36 2047)
    (list (xz 446 518) 37 2047)
    (list (xz 446 518) 38 2047)
    (list (xz 446 518) 39 2047)
    (list (xz 446 518) 40 2047)
    (list (xz 446 518) 41 2047)
    (list (xz 446 518) 42 2047)
    (list (xz 446 518) 43 2047)
    (list (xz 446 518) 44 2047)
    (list (xz 447 518) 29 2047)
    (list (xz 447 518) 30 2047)
    (list (xz 447 518) 31 2047)
    (list (xz 447 518) 32 2047)
    (list (xz 448 518) 29 2047)
    (list (xz 448 518) 30 2047)
    (list (xz 448 518) 31 2047)
    (list (xz 448 518) 32 2047)
    (list (xz 449 518) 29 2047)
    (list (xz 449 518) 30 2047)
    (list (xz 449 518) 31 2047)
    (list (xz 449 518) 32 2047)
    (list (xz 450 518) 29 2047)
    (list (xz 450 518) 30 2047)
    (list (xz 450 518) 31 2047)
    (list (xz 450 518) 32 2047)
    (list (xz 451 518) 29 2047)
    (list (xz 451 518) 30 2047)
    (list (xz 451 518) 31 2047)
    (list (xz 451 518) 32 2047)
    (list (xz 452 518) 29 2047)
    (list (xz 452 518) 30 2047)
    (list (xz 452 518) 31 2047)
    (list (xz 452 518) 32 2047)
    (list (xz 453 518) 29 2047)
    (list (xz 453 518) 30 2047)
    (list (xz 453 518) 31 2047)
    (list (xz 453 518) 32 2047)
    (list (xz 454 518) 30 2047)
    (list (xz 454 518) 31 2047)
    (list (xz 454 518) 32 2047)
    (list (xz 454 518) 33 2047)
    (list (xz 454 518) 34 2047)
    (list (xz 441 519) 31 2047)
    (list (xz 441 519) 32 2047)
    (list (xz 441 519) 33 2047)
    (list (xz 442 519) 29 2047)
    (list (xz 442 519) 30 2047)
    (list (xz 442 519) 31 2047)
    (list (xz 442 519) 32 2047)
    (list (xz 442 519) 33 2047)
    (list (xz 442 519) 34 2047)
    (list (xz 443 519) 29 2047)
    (list (xz 443 519) 30 2047)
    (list (xz 443 519) 31 2047)
    (list (xz 443 519) 32 2047)
    (list (xz 443 519) 33 2047)
    (list (xz 444 519) 29 2047)
    (list (xz 444 519) 30 2047)
    (list (xz 444 519) 31 2047)
    (list (xz 444 519) 32 2047)
    (list (xz 444 519) 33 2047)
    (list (xz 445 519) 29 2047)
    (list (xz 445 519) 30 2047)
    (list (xz 445 519) 31 2047)
    (list (xz 445 519) 32 2047)
    (list (xz 446 519) 29 2047)
    (list (xz 446 519) 30 2047)
    (list (xz 446 519) 31 2047)
    (list (xz 446 519) 32 2047)
    (list (xz 446 519) 36 2047)
    (list (xz 446 519) 37 2047)
    (list (xz 446 519) 38 2047)
    (list (xz 446 519) 39 2047)
    (list (xz 446 519) 40 2047)
    (list (xz 446 519) 41 2047)
    (list (xz 446 519) 42 2047)
    (list (xz 446 519) 43 2047)
    (list (xz 446 519) 44 2047)
    (list (xz 447 519) 29 2047)
    (list (xz 447 519) 30 2047)
    (list (xz 447 519) 31 2047)
    (list (xz 447 519) 32 2047)
    (list (xz 448 519) 29 2047)
    (list (xz 448 519) 30 2047)
    (list (xz 448 519) 31 2047)
    (list (xz 448 519) 32 2047)
    (list (xz 449 519) 29 2047)
    (list (xz 449 519) 30 2047)
    (list (xz 449 519) 31 2047)
    (list (xz 449 519) 32 2047)
    (list (xz 450 519) 29 2047)
    (list (xz 450 519) 30 2047)
    (list (xz 450 519) 31 2047)
    (list (xz 450 519) 32 2047)
    (list (xz 451 519) 29 2047)
    (list (xz 451 519) 30 2047)
    (list (xz 451 519) 31 2047)
    (list (xz 451 519) 32 2047)
    (list (xz 452 519) 29 2047)
    (list (xz 452 519) 30 2047)
    (list (xz 452 519) 31 2047)
    (list (xz 452 519) 32 2047)
    (list (xz 452 519) 33 2047)
    (list (xz 453 519) 29 2047)
    (list (xz 453 519) 30 2047)
    (list (xz 453 519) 31 2047)
    (list (xz 453 519) 32 2047)
    (list (xz 453 519) 33 2047)
    (list (xz 454 519) 30 2047)
    (list (xz 454 519) 31 2047)
    (list (xz 454 519) 32 2047)
    (list (xz 454 519) 33 2047)
    (list (xz 454 519) 34 2047)
    (list (xz 441 520) 31 2047)
    (list (xz 441 520) 32 2047)
    (list (xz 441 520) 33 2047)
    (list (xz 442 520) 31 2047)
    (list (xz 442 520) 32 2047)
    (list (xz 442 520) 33 2047)
    (list (xz 443 520) 31 2047)
    (list (xz 443 520) 32 2047)
    (list (xz 443 520) 33 2047)
    (list (xz 444 520) 31 2047)
    (list (xz 444 520) 32 2047)
    (list (xz 444 520) 33 2047)
    (list (xz 445 520) 31 2047)
    (list (xz 445 520) 32 2047)
    (list (xz 445 520) 33 2047)
    (list (xz 446 520) 31 2047)
    (list (xz 446 520) 32 2047)
    (list (xz 446 520) 33 2047)
    (list (xz 446 520) 36 2047)
    (list (xz 446 520) 37 2047)
    (list (xz 446 520) 38 2047)
    (list (xz 446 520) 39 2047)
    (list (xz 446 520) 40 2047)
    (list (xz 446 520) 41 2047)
    (list (xz 446 520) 42 2047)
    (list (xz 446 520) 43 2047)
    (list (xz 446 520) 44 2047)
    (list (xz 447 520) 31 2047)
    (list (xz 447 520) 32 2047)
    (list (xz 447 520) 33 2047)
    (list (xz 448 520) 31 2047)
    (list (xz 448 520) 32 2047)
    (list (xz 448 520) 33 2047)
    (list (xz 449 520) 31 2047)
    (list (xz 449 520) 32 2047)
    (list (xz 449 520) 33 2047)
    (list (xz 450 520) 31 2047)
    (list (xz 450 520) 32 2047)
    (list (xz 451 520) 31 2047)
    (list (xz 451 520) 32 2047)
    (list (xz 451 520) 33 2047)
    (list (xz 452 520) 31 2047)
    (list (xz 452 520) 32 2047)
    (list (xz 452 520) 33 2047)
    (list (xz 453 520) 31 2047)
    (list (xz 453 520) 32 2047)
    (list (xz 453 520) 33 2047)
    (list (xz 446 521) 36 2047)
    (list (xz 446 521) 37 2047)
    (list (xz 446 521) 38 2047)
    (list (xz 446 521) 39 2047)
    (list (xz 446 521) 40 2047)
    (list (xz 446 521) 41 2047)
    (list (xz 446 521) 42 2047)
    (list (xz 446 521) 43 2047)
    (list (xz 446 521) 44 2047))
  }
