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

(require "msgpack.rkt")
(provide pack)

(define (pack datum out)
  (cond
    [(null? datum) (pack-nil out)]
    [(boolean? datum) (pack-bool datum out)]
    [(integer? datum) ((if (> datum 0) pack-uint pack-int) datum out)]
    [(single-flonum? datum) (pack-float datum)]
    [(flonum? datum) (pack-float datum)]
    [(string? datum) (pack-string datum out)]
    [(bytes? datum) (pack-bin datum out)]
    [(vector? datum) (pack-array datum out)]
    [(hash? datum) (pack-map datum out)]
    [(ext? datum) (pack-ext datum out)]
    [else (error "Type not supported by MessagePack")]))

(define (pack-ext type data out)
  (let ([len (bytes-length data)])
    (cond
      [(= len  1) (write-byte #xD4 out)]
      [(= len  2) (write-byte #xD5 out)]
      [(= len  4) (write-byte #xD6 out)]
      [(= len  8) (write-byte #xD7 out)]
      [(= len 16) (write-byte #xD8 out)]
      [(< len (expt 2  8))
       (begin
         (write-byte #xC7 out)
         (write-bytes (integer->bytes len) out))]
      [(< len (expt 2 16))
       (begin
         (write-byte #xC8 out)
         (write-bytes (integer->bytes len) out))]
      [(< len (expt 2 32))
       (begin
         (write-byte #xC9 out)
         (write-bytes (integer->bytes len) out))])
    (write-byte type out)
    (write-bytes data out)))


;;; Convert an integer to a big-endian byte string. Only certain value ranges
;;; are allowed and we determine the length of the byte string automatically.
(define (integer->bytes int signed?)
  (define (number-of-bytes)
    (if signed?
      (cond
        [(< (- (expt 2 15)) int (- (expt 2 15) 1)) 2]
        [(< (- (expt 2 31)) int (- (expt 2 31) 1)) 4]
        [(< (- (expt 2 63)) int (- (expt 2 63) 1)) 8])
      (cond
        [(< int (expt 2 16)) 2]
        [(< int (expt 2 32)) 4]
        [(< int (expt 2 64)) 8])))
  (if signed?
    (cond
      [(<= -128 int 127)
       (bytes int)]
      [(<= (- (expt 2 63)) int (- (expt 2 63) 1))
       (integer->integer-bytes int (number-of-bytes) #t #t)]
      [else (error "Unsigned integers may not be large than 8 bytes")])
    (cond
      [(< int #x100)
       (bytes int)]
      [(< #xFF int (expt 2 64))
       (integer->integer-bytes int (number-of-bytes) #f #t)]
      [else (error "Unsigned integers may not be large than 8 bytes")])))


(define (pack-nil out)
  (write-byte #xC0 out))

(define (pack-bool b out)
  (write-byte (if b #xC3 #xC2)))

(define (pack-uint uint out)
  (cond
    [(<= uint #b01111111) (void)]
    [(< uint (expt 2  8)) (write-byte #xCC out)]
    [(< uint (expt 2 16)) (write-byte #xCD out)]
    [(< uint (expt 2 32)) (write-byte #xCD out)]
    [(< uint (expt 2 64)) (write-byte #xCD out)]
    [else
      (error "Unsigned integer must not be larger than 2^64 - 1")])
  (write-bytes (integer->bytes uint 2 #f) out))

(define (pack-int int out)
  (if (<= #b-00011111 int -1)  ; negative fixnum
     ;; int is always negative and > -32, so convert to positive and OR with
     ;; the prefix
     (write-bytes (bitwise-ior #b11100000 (abs int)))
     (begin
       (cond
        [(<= (- (expt 2  8)) int (- (expt 2  8) 1)) (write-byte #xD0 out)]
        [(<= (- (expt 2 16)) int (- (expt 2 16) 1)) (write-byte #xD1 out)]
        [(<= (- (expt 2 32)) int (- (expt 2 32) 1)) (write-byte #xD2 out)]
        [(<= (- (expt 2 64)) int (- (expt 2 64) 1)) (write-byte #xD3 out)]
        [else (error "Cannot pack integers larger than 32 bits.")])
       (write-bytes (integer->bytes int) out))))

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
      [(<= len (expt 2 8))
       (pack-str-n len #xD9)]
      [(<= len (expt 2 16))
       (pack-str-n len #xDA)]
      [(<= len (expt 2 32))
       (pack-str-n len #xDB)]
      [else (error "String may only be up to 2^32 - 1 bytes long")])
    (write-bytes (string->bytes/utf-8 str) out)))

(define (pack-bin bin out)
  (let ([len (bytes-length bin)])
    (cond
      [(< len (expt 2  8)) (write-byte #xC4 out)]
      [(< len (expt 2 16)) (write-byte #xC5 out)]
      [(< len (expt 2 32)) (write-byte #xC6 out)]
      [else (error "Byte string may only be up to 2^32 - 1 bytes long")])
    (write-bytes (integer->bytes len #f) out)
    (write-bytes bin out)))

(define (pack-array arr out)
  (let ([len (vector-length arr)])
    (if (<= len #b00001111)
      (write-byte (bitwise-ior len #b10010000) out)
      (begin
        (cond
          [(< len (expt 2 16)) (write-byte #xDC out)]
          [(< len (expt 2 32)) (write-byte #xDD out)]
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
          [(< len (expt 2 16)) (write-byte #xDE out)]
          [(< len (expt 2 32)) (write-byte #xDF out)]
          [else (error "An array may contain at most 2^32 - 1 items")])
        (write-bytes (integer->bytes len) out)))
    (for ([(key value) (in-hash m)])
      (pack key   out)
      (pack value out))))
