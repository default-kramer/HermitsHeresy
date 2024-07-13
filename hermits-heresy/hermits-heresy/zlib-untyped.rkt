#lang racket/base

(provide compress uncompress)

; If you don't use the right compression settings, DQB2 can misread the save file.
; For example, using System.IO.Compression.CompressionLevel.Fastest from dotnet will sometimes
; cause this error: https://github.com/turtle-insect/DQB2/pull/16
; Fortunately, it seems like the zlib default settings work reliably.
;
;
; = Notes to self =
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

(define-ffi-definer define-zlib
  (case (system-type 'os)
    ; Looks like zlib1 gets bundled with Racket on Windows.
    ; Is the name "zlib1" for Windows only?
    [(windows) (ffi-lib "zlib1")]
    ; I think "zlib" might work on Linux...?
    ; Even if the name is correct, it might simply not be installed by default.
    ; This would mostly just be so the tests can run in a Linux build pipeline.
    ; I don't think anyone is trying to run DQB2 on Linux.
    [else (ffi-lib "zlib")]))

(define-zlib zlibVersion (_fun -> _string))

#;(println (zlibVersion))

(define (zlib-err val)
  (case val
    [(1) 'Z_STREAM_END]
    [(2) 'Z_NEED_DICT]
    [(-1) 'Z_ERRNO]
    [(-2) 'Z_STREAM_ERROR]
    [(-3) 'Z_DATA_ERROR]
    [(-4) 'Z_MEM_ERROR]
    [(-5) 'Z_BUF_ERROR]
    [(-6) 'Z_VERSION_ERROR]
    [else ""]))

(define-syntax-rule (check-retval val name)
  (when (not (= 0 val))
    (error (format "zlib `~a` returned non-zero: ~a ~a" name val (zlib-err val)))))

; zlib.h on compressBound:
;     compressBound() returns an upper bound on the compressed size after
;   compress() or compress2() on sourceLen bytes.  It would be used before a
;   compress() or compress2() call to allocate the destination buffer.
(define-zlib compressBound (_fun _ulong ; uLong sourceLen
                                 -> _ulong))

(define (compress src)
  (define src-len (bytes-length src))
  (define buffer-len (compressBound src-len))
  ; zlib.h on compress:
  ;     Compresses the source buffer into the destination buffer.  sourceLen is
  ;   the byte length of the source buffer.  Upon entry, destLen is the total size
  ;   of the destination buffer, which must be at least the value returned by
  ;   compressBound(sourceLen).  Upon exit, destLen is the actual size of the
  ;   compressed data.  compress() is equivalent to compress2() with a level
  ;   parameter of Z_DEFAULT_COMPRESSION.
  ;
  ;     compress returns Z_OK if success, Z_MEM_ERROR if there was not
  ;   enough memory, Z_BUF_ERROR if there was not enough room in the output
  ;   buffer.
  (define-zlib compress (_fun [dest : (_bytes o buffer-len)] ; Bytef *dest
                              [dest-len : (_ptr io _ulong)] ; uLongf* destLen
                              _bytes ; const Bytef* source
                              _ulong ; uLong sourceLen
                              -> [retval : _int]
                              -> (values retval dest-len dest)))
  (define-values (retval dest-len dst)
    (compress buffer-len src src-len))
  (check-retval retval "compress")
  (subbytes dst 0 dest-len))

(define (uncompress compressed uncompressed-length)
  (define src-length (bytes-length compressed))
  ; zlib.h on uncompress
  ;     Decompresses the source buffer into the destination buffer.  sourceLen is
  ;   the byte length of the source buffer.  Upon entry, destLen is the total size
  ;   of the destination buffer, which must be large enough to hold the entire
  ;   uncompressed data.  (The size of the uncompressed data must have been saved
  ;   previously by the compressor and transmitted to the decompressor by some
  ;   mechanism outside the scope of this compression library.) Upon exit, destLen
  ;   is the actual size of the uncompressed data.

  ;     uncompress returns Z_OK if success, Z_MEM_ERROR if there was not
  ;   enough memory, Z_BUF_ERROR if there was not enough room in the output
  ;   buffer, or Z_DATA_ERROR if the input data was corrupted or incomplete.  In
  ;   the case where there is not enough room, uncompress() will fill the output
  ;   buffer with the uncompressed data up to that point.
  (define-zlib uncompress (_fun [dest : (_bytes o uncompressed-length)] ; Bytef *dest
                                [dest-length : (_ptr io _ulong)] ; uLongf *destLen
                                _bytes ; const Bytef* source
                                _ulong ; uLong sourceLen
                                -> [retval : _int]
                                -> (values retval dest dest-length)))
  (define-values (retval dest actual-length)
    (uncompress uncompressed-length compressed src-length))
  (check-retval retval "uncompress")
  (if (= actual-length (bytes-length dest))
      dest
      (subbytes dest 0 actual-length)))
