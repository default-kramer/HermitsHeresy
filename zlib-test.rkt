#lang racket/base

; Looks like zlib1 gets bundled with Racket.
; (Is the name "zlib1" for Windows only?)
;
; Looks like System.IO.CompressionLevel.Optimal corresponds to zlib's default compression and memory options.
; Here https://github.com/dotnet/runtime/blob/main/src/libraries/System.IO.Compression/src/System/IO/Compression/DeflateZLib/Deflater.cs#L37
;                 case CompressionLevel.Optimal:
;                    zlibCompressionLevel = ZLibNative.CompressionLevel.DefaultCompression;
;                    memLevel = ZLibNative.Deflate_DefaultMemLevel;
;                    break;
;
; So, as long as I can use zlib with the default stuff it should work?
; Found in zlib.h
;   #define Z_NO_COMPRESSION         0
;   #define Z_BEST_SPEED             1
;   #define Z_BEST_COMPRESSION       9
;   #define Z_DEFAULT_COMPRESSION  (-1)
;   /* compression levels */

; Reading more of zlib.h, it looks like the `compress` function does
; exactly what I want (default compression and memory usage).
; So if I use `compress` I can forget about compression levels, I hope...

(require ffi/unsafe
         ffi/unsafe/define)

(define-ffi-definer define-zlib (ffi-lib "zlib1"))

(define-zlib zlibVersion (_fun -> _string))

(println (zlibVersion))


; Okay let's do some guessing:
(define-zlib compress (_fun [dst : (_bytes o 999)] ; Bytef *dest
                            [dst-len : (_ptr o _uint64)] ; uLongf* destLen
                            _bytes ; const Bytef* source
                            _uint64 ; uLong sourceLen
                            -> [retval : _int] ; zlib returns an int
                            ; This is what our compress will return:
                            -> (values retval dst-len dst)))

(let* ([srclen 100]
       [src (make-bytes srclen)])
  (compress src srclen))
