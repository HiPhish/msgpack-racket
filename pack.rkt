;;;; Copyright 2017 Alejandro Sanchez
;;;;
;;;; This file is part of MessagePack.rkt
;;;; 
;;;;     MessagePack.rkt is free software: you can redistribute it and/or
;;;;     modify it under the terms of the GNU General Public License as
;;;;     published by the Free Software Foundation, either version 3 of the
;;;;     License, or (at your option) any later version.
;;;; 
;;;;     MessagePack.rkt is distributed in the hope that it will be useful,
;;;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;     GNU General Public License for more details.
;;;; 
;;;;     You should have received a copy of the GNU General Public License
;;;;     along with MessagePack.rkt.  If not, see
;;;;     <http://www.gnu.org/licenses/>.
#lang typed/racket/base

(require "ext.rkt"
         "packable.rkt"
         "private/helpers.rkt")

(provide pack)


;;; A note about magic numbers: Magic numbers, in particular binary and
;;; hexadecimal ones are related to the MessagePack spec. Look up the spec when
;;; in doubt.


;;; ===[ Generic packing ]====================================================
(: pack (-> Any Output-Port Any))
(define (pack datum out)
  (cond
    [(void?           datum) (pack-void                                 out)]
    [(boolean?        datum) (pack-boolean  datum                       out)]
    [(exact-integer?  datum) (pack-integer  datum                       out)]
    [(single-flonum?  datum) (pack-flonum   datum                    #t out)]
    [(flonum?         datum) (pack-flonum   datum                    #f out)]
    [(real?           datum) (pack-flonum   datum                    #t out)]
    [(string?         datum) (pack-string   datum                       out)]
    [(bytes?          datum) (pack-bytes    datum                       out)]
    [(vector?         datum) (pack-sequence datum (vector-length datum) out)]
    [(list?           datum) (pack-sequence datum (length        datum) out)]
    [(hash?           datum) (pack-hash     datum                       out)]
    [(ext?            datum) (pack-ext      datum                       out)]
    [else (error "Type of " datum " not supported by MessagePack")]))


;;; ===[ Nil ]================================================================
(: pack-void (-> Output-Port Any))
(define (pack-void out)
  (write-byte #xC0 out))


;;; ===[ Boolean ]============================================================
(: pack-boolean (-> Boolean Output-Port Any))
(define (pack-boolean b out)
  (write-byte (if b #xC3 #xC2) out))


;;; ===[ Integers ]===========================================================
;;; These predicates make things easier
(: +fixint? (-> Integer Boolean))
(define (+fixint? x) (and (exact-nonnegative-integer? x) (< x 128)))
(: -fixint? (-> Integer Boolean))
(define (-fixint? x) (and (exact-integer? x) (negative? x) (<= -32 x -1)))
(: uint8? (-> Integer Boolean))
(define (uint8? x) (and (exact-nonnegative-integer? x) (< x (expt 2  8))))
(: uint16? (-> Integer Boolean))
(define (uint16? x) (and (exact-nonnegative-integer? x) (< x (expt 2 16))))
(: uint32? (-> Integer Boolean))
(define (uint32? x) (and (exact-nonnegative-integer? x) (< x (expt 2 32))))
(: uint64? (-> Integer Boolean))
(define (uint64? x) (and (exact-nonnegative-integer? x) (< x (expt 2 64))))
(: int8? (-> Integer Boolean))
(define (int8?   x) (and (exact-integer? x) (<= (- (expt 2  7)) x (sub1 (expt 2  7)))))
(: int16? (-> Integer Boolean))
(define (int16?  x) (and (exact-integer? x) (<= (- (expt 2 15)) x (sub1 (expt 2 15)))))
(: int32? (-> Integer Boolean))
(define (int32?  x) (and (exact-integer? x) (<= (- (expt 2 31)) x (sub1 (expt 2 31)))))
(: int64? (-> Integer Boolean))
(define (int64?  x) (and (exact-integer? x) (<= (- (expt 2 63)) x (sub1 (expt 2 63)))))

(: pack-integer (-> Integer Output-Port Any))
(define (pack-integer i out)
  (if (< i 0) (pack-int i out) (pack-uint i out)))

(: pack-uint (-> Integer Output-Port Any))
(define (pack-uint uint out)
  (cond
    ;; In case of a fixint we don't need a tag, just write the byte
    [(+fixint? uint) (void)]
    [(uint8?   uint) (write-byte #xCC out)]
    [(uint16?  uint) (write-byte #xCD out)]
    [(uint32?  uint) (write-byte #xCE out)]
    [(uint64?  uint) (write-byte #xCF out)]
    [else (error "Unsigned integer must not be larger than 2^64 - 1")])
  (write-bytes (integer->bytes uint #f) out))

(: pack-int (-> Integer Output-Port Any))
(define (pack-int int out)
  (cond
    ;; In case of a fixint we don't need a tag, just write the byte
    [(-fixint? int) (void)]
    [(int8?    int) (write-byte #xD0 out)]
    [(int16?   int) (write-byte #xD1 out)]
    [(int32?   int) (write-byte #xD2 out)]
    [(int64?   int) (write-byte #xD3 out)]
    [else (error "Signed integer must not be larger than 64 bits.")])
  (write-bytes (integer->bytes int #t) out))


;;; ===[ Floating point numbers ]=============================================
(: pack-flonum (-> Real Boolean Output-Port Any))
(define (pack-flonum f single? out)
  (write-byte (if single? #xCA #xCB) out)
  (write-bytes (real->floating-point-bytes f (if single? 4 8) #t) out))


;;; ===[ Unicode strings ]====================================================
(: pack-string (-> String Output-Port Any))
(define (pack-string str out)
  (define len (bytes-length (string->bytes/utf-8 str)))
  (: pack-str-n (-> Integer Integer Any))
  (define (pack-str-n len tag)
    (write-byte tag out)
    (write-bytes (integer->bytes len #f) out))
  (cond
    [(<= len #b00011111)
     (write-byte (bitwise-ior len #b10100000) out)]
    [(uint8?  len) (pack-str-n len #xD9)]
    [(uint16? len) (pack-str-n len #xDA)]
    [(uint32? len) (pack-str-n len #xDB)]
    [else (error "String may only be up to 2^32 - 1 bytes long")])
  (write-bytes (string->bytes/utf-8 str) out))


;;; ===[ Binary strings ]=====================================================
(: pack-bytes (-> Bytes Output-Port Any))
(define (pack-bytes bstr out)
  (define len (bytes-length bstr))
  (cond
    [(uint8?  len) (write-byte #xC4 out)]
    [(uint16? len) (write-byte #xC5 out)]
    [(uint32? len) (write-byte #xC6 out)]
    [else (error "Byte string may only be up to 2^32 - 1 bytes long")])
  (write-bytes (integer->bytes len #f) out)
  (write-bytes bstr out))


;;; ===[ Arrays ]=============================================================
(: pack-sequence (-> (U VectorTop (Listof Any))
                     Index
                     Output-Port
                     Any))
(define (pack-sequence seq len out)
  (cond
    [(<= len #b00001111)
     (write-byte (bitwise-ior len #b10010000) out)]
    [(uint16? len)
     (write-byte #xDC out)
     (write-bytes (integer->bytes len #f) out)]
    [(uint32? len)
     (write-byte #xDD out)
     (write-bytes (integer->bytes len #f) out)]
    [else
     (error "A sequence may contain at most 2^32 - 1 items") ])
  (cond
    [(list?   seq) (for ([item (in-list   seq)]) (pack item out))]
    [(vector? seq) (for ([item (in-vector seq)]) (pack item out))]))


;;; ===[ Maps ]===============================================================
(: pack-hash (-> HashTableTop Output-Port Any))
(define (pack-hash hash out)
  (define len (hash-count hash))
  (cond
    [(<= len #b00001111)
     (write-byte (bitwise-ior len #b10000000) out)]
    [else
      (cond
        [(uint16? len) (write-byte #xDE out)]
        [(uint32? len) (write-byte #xDF out)]
        [else (error "An map may contain at most 2^32 - 1 items")])
      (write-bytes (integer->bytes len #f) out)])
  (for ([(key value) (in-hash hash)])
    (pack key   out)
    (pack value out)))


;;; ===[ Extensions ]=========================================================
(: pack-ext (-> Ext Output-Port Any))
(define (pack-ext ext out)
  (define len (bytes-length (ext-data ext)))
  (cond
    [(= len  1) (write-byte #xD4 out)]
    [(= len  2) (write-byte #xD5 out)]
    [(= len  4) (write-byte #xD6 out)]
    [(= len  8) (write-byte #xD7 out)]
    [(= len 16) (write-byte #xD8 out)]
    [(uint8? len)
     (begin
       (write-byte #xC7 out)
       (write-bytes (integer->bytes len #f) out))]
    [(uint16? len)
     (begin
       (write-byte #xC8 out)
       (write-bytes (integer->bytes len #f) out))]
    [(uint32? len)
     (begin
       (write-byte #xC9 out)
       (write-bytes (integer->bytes len #f) out))])
  (write-bytes (integer->integer-bytes* (ext-type ext) 1 #t #t) out)
  (write-bytes (ext-data ext) out))


;;; ===[ Helper functions ]===================================================
;;; Convert an integer to a big-endian byte string. Only certain value ranges
;;; are allowed and we determine the length of the byte string automatically.
;;; Ideally we would use 'integer->integer-bytes', but that function does not
;;; support 8-bit integers, so we need this for the time being.
(: integer->bytes (-> Integer Boolean Bytes))
(define (integer->bytes int signed?)
  (: number-of-bytes (-> Integer))
  (define (number-of-bytes)
    (cond [(uint8?  int) 1]
          [(uint16? int) 2]
          [(uint32? int) 4]
          [(uint64? int) 8]
          [( int8?  int) 1]
          [( int16? int) 2]
          [( int32? int) 4]
          [( int64? int) 8]
          [else (error "Integer " int " is larger than 64-bit.")]))
  (integer->integer-bytes* int (number-of-bytes) signed? #t))
