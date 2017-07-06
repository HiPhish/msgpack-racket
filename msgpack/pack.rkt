;;;; Copyright 1999 Alejandro Sanchez
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
#lang racket

(require "../main.rkt")



;;; === Packing functions ===
(provide 
  (contract-out
    [msg-port? (-> any/c boolean?)]
    [uint8?    (-> any/c boolean?)]
    [uint16?   (-> any/c boolean?)]
    [uint32?   (-> any/c boolean?)]
    [uint64?   (-> any/c boolean?)]
    [int8?     (-> any/c boolean?)]
    [int16?    (-> any/c boolean?)]
    [int32?    (-> any/c boolean?)]
    [int64?    (-> any/c boolean?)]
    [+fixint?  (-> any/c boolean?)]
    [-fixint?  (-> any/c boolean?)]
    [pack          (-> any/c    msg-port? any)]
    [pack-nil      (-> msg-port?          any)]
    [pack-boolean  (-> boolean? msg-port? any)]
    [pack-p-fixint (-> +fixint? msg-port? any)]
    [pack-n-fixint (-> -fixint? msg-port? any)]
    [pack-uint8    (-> uint8?   msg-port? any)]
    [pack-uint16   (-> uint16?  msg-port? any)]
    [pack-uint32   (-> uint32?  msg-port? any)]
    [pack-uint64   (-> uint64?  msg-port? any)]
    [pack-int8     (-> int8?    msg-port? any)]
    [pack-int16    (-> int16?   msg-port? any)]
    [pack-int32    (-> int32?   msg-port? any)]
    [pack-int64    (-> int64?   msg-port? any)]
    [pack-float    (-> real?    msg-port? any)]
    [pack-string   (-> string?  msg-port? any)]
    [pack-bin      (-> bytes?   msg-port? any)]
    [pack-array    (-> vector?  msg-port? any)]
    [pack-map      (-> hash?    msg-port? any)]
    [pack-ext      (-> ext?     msg-port? any)]
    ))

;;; Valid output port
(define msg-port?
  (and/c output-port? (not/c port-closed?)))
;;; Int format family
(define (uint8?   x) (and (exact-nonnegative-integer? x) (< x (expt 2  8))))
(define (uint16?  x) (and (exact-nonnegative-integer? x) (< x (expt 2 16))))
(define (uint32?  x) (and (exact-nonnegative-integer? x) (< x (expt 2 32))))
(define (uint64?  x) (and (exact-nonnegative-integer? x) (< x (expt 2 64))))
(define (int8?    x) (and (exact-integer? x) (<= (- (expt 2  7)) x (- (expt 2  7) 1))))
(define (int16?   x) (and (exact-integer? x) (<= (- (expt 2 15)) x (- (expt 2 15) 1))))
(define (int32?   x) (and (exact-integer? x) (<= (- (expt 2 31)) x (- (expt 2 31) 1))))
(define (int64?   x) (and (exact-integer? x) (<= (- (expt 2 63)) x (- (expt 2 63) 1))))
(define (+fixint? x) (and (exact-nonnegative-integer? x) (<= x #b01111111)))
(define (-fixint? x) (and (exact-integer? x) (negative? x) (>= x #b-00011111)))

(define (pack datum out)
  (cond
    [(null?    datum) (pack-nil out)]
    [(boolean? datum) (pack-boolean datum out)]
    [(integer? datum) ((if (> datum 0) pack-uint pack-int) datum out)]
    [(real?    datum) (pack-float datum)]
    [(flonum?  datum) (pack-float datum)]
    [(string?  datum) (pack-string datum out)]
    [(bytes?   datum) (pack-bin datum out)]
    [(vector?  datum) (pack-array datum out)]
    [(hash?    datum) (pack-map datum out)]
    [(ext?     datum) (pack-ext datum out)]
    [else (error "Type not supported by MessagePack")]))

(define (pack-nil out)
  (write-byte #xC0 out))

(define (pack-boolean b out)
  (write-byte (if b #xC3 #xC2) out))

(define (pack-uint uint out)
  (cond
    [(+fixint? uint) (void)]
    [(uint8?   uint) (write-byte #xCC out)]
    [(uint16?  uint) (write-byte #xCD out)]
    [(uint32?  uint) (write-byte #xCD out)]
    [(uint64?  uint) (write-byte #xCD out)]
    [else
      (error "Unsigned integer must not be larger than 2^64 - 1")])
  (write-bytes (integer->bytes uint 2 #f) out))

(define (pack-int int out)
  (if (-fixint? int)
     ;; int is always negative and > -32, so convert to positive and OR with
     ;; the prefix
     (write-bytes (bitwise-ior #b11100000 (abs int)))
     (begin
       (cond
        [(int8?  int) (write-byte #xD0 out)]
        [(int16? int) (write-byte #xD1 out)]
        [(int32? int) (write-byte #xD2 out)]
        [(int64? int) (write-byte #xD3 out)]
        [else (error "Cannot pack integers larger than 64 bits.")])
       (write-bytes (integer->bytes int) out))))

(define (pack-p-fixint i out)
  (write-byte i))

(define (pack-n-fixint i out)
  (write-byte
    (bitwise-ior #b11100000
                 (* -1 i))))

(define (pack-uint8  i out) (pack-uint i out))
(define (pack-uint16 i out) (pack-uint i out))
(define (pack-uint32 i out) (pack-uint i out))
(define (pack-uint64 i out) (pack-uint i out))
(define (pack-int8   i out) (pack-int  i out))
(define (pack-int16  i out) (pack-int  i out))
(define (pack-int32  i out) (pack-int  i out))
(define (pack-int64  i out) (pack-int  i out))

(define (pack-float datum out)
  (let ([single (single-flonum? datum)])
    (write-byte (if single #xCA #xCB) out)
    (write-bytes (real->floating-point-bytes datum (if single 4 8) #t) out)))

(define (pack-string str out)
  (define (pack-str-n len tag)
    (write-byte tag out)
    (write-bytes (integer->bytes len #f) out))
  (let ([len (bytes-length (string->bytes/utf-8 str))])
    (cond
      [(<= len #b00011111)
       (write-byte (bitwise-ior len #b10100000) out)]
      [(uint8?  len) (pack-str-n len #xD9)]
      [(uint16? len) (pack-str-n len #xDA)]
      [(uint32? len) (pack-str-n len #xDB)]
      [else (error "String may only be up to 2^32 - 1 bytes long")])
    (write-bytes (string->bytes/utf-8 str) out)))

(define (pack-bin bin out)
  (let ([len (bytes-length bin)])
    (cond
      [(uint8?  len) (write-byte #xC4 out)]
      [(uint16? len) (write-byte #xC5 out)]
      [(uint32? len) (write-byte #xC6 out)]
      [else (error "Byte string may only be up to 2^32 - 1 bytes long")])
    (write-bytes (integer->bytes len #f) out)
    (write-bytes bin out)))

(define (pack-array arr out)
  (let ([len (vector-length arr)])
    (if (<= len #b00001111)
      (write-byte (bitwise-ior len #b10010000) out)
      (begin
        (cond
          [(uint8?  len) (write-byte #xDC out)]
          [(uint16? len) (write-byte #xDD out)]
          [else (error "An array may contain at most 2^32 - 1 items")])
        (write-bytes (integer->bytes len) out)))
    (for ([item (in-vector arr)])
      (pack item out))))

(define (pack-map m out)
  (let ([len (hash-count m)])
    (if (<= len #b00001111)
      (write-byte (bitwise-ior len #b10000000) out)
      (begin
        (cond
          [(uint16? len) (write-byte #xDE out)]
          [(uint32? len) (write-byte #xDF out)]
          [else (error "An array may contain at most 2^32 - 1 items")])
        (write-bytes (integer->bytes len) out)))
    (for ([(key value) (in-hash m)])
      (pack key   out)
      (pack value out))))

(define (pack-ext ext out)
  (let ([len (bytes-length (ext-data ext))])
    (cond
      [(= len  1) (write-byte #xD4 out)]
      [(= len  2) (write-byte #xD5 out)]
      [(= len  4) (write-byte #xD6 out)]
      [(= len  8) (write-byte #xD7 out)]
      [(= len 16) (write-byte #xD8 out)]
      [(uint8? len)
       (begin
         (write-byte #xC7 out)
         (write-bytes (integer->bytes len) out))]
      [(uint16? len)
       (begin
         (write-byte #xC8 out)
         (write-bytes (integer->bytes len) out))]
      [(uint32? len)
       (begin
         (write-byte #xC9 out)
         (write-bytes (integer->bytes len) out))])
    (write-byte  (ext-type ext) out)
    (write-bytes (ext-data ext) out)))


;;; === Helper functions ===

;;; Convert an integer to a big-endian byte string. Only certain value ranges
;;; are allowed and we determine the length of the byte string automatically.
(define (integer->bytes int signed?)
  (define (number-of-bytes)
    (if signed?
      (cond
        [(int16? int) 2]
        [(int32? int) 4]
        [(int64? int) 8])
      (cond
        [(uint16? int) 2]
        [(uint32? int) 4]
        [(uint64? int) 8])))
  (if signed?
    (cond
      [(int8?  int) (bytes int)]
      [(int64? int) (integer->integer-bytes int (number-of-bytes) #t #t)]
      [else (error "Signed integers may not be large than 8 bytes")])
    (cond
      [(uint8?  int) (bytes int)]
      [(uint64? int) (integer->integer-bytes int (number-of-bytes) #f #t)]
      [else (error "Unsigned integers may not be large than 8 bytes")])))
