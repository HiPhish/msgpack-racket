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
#lang racket/base

(require racket/contract/base)

(provide
  (contract-out
    [integer-bytes->integer*
      (->* (bytes?
            any/c)
           (any/c
            exact-nonnegative-integer?
            exact-nonnegative-integer?)
          exact-integer?)]
    [integer->integer-bytes*
      (->* (exact-integer?
            (or/c 1 2 4 8)
            any/c)
           (any/c
            (and/c bytes? (not/c immutable?))
            exact-nonnegative-integer?)
           bytes?)]
    [int8->byte (-> (integer-in -128 127) byte?)]
    [byte->int8 (-> byte? (integer-in -128 127))]))

;;; I need this because 'integer->integer-bytes' does not support 8-bit
;;; integers.
(define (int8->byte i)
  (if (< i 0) (+ #x100 i) i))

(define (byte->int8 b)
  (if (not (zero? (bitwise-and b #b10000000))) (- b #x100) b))


;;; An extension of 'integer->integer-bytes' which adds support for 8-bit
;;; numbers.

(define (integer-bytes->integer* bstr
                                 signed?
                                 [big-endian? (system-big-endian?)]
                                 [start 0]
                                 [end (bytes-length bstr)])
  (cond
    [(= (- end start) 1)
     (let ([b (bytes-ref bstr start)])
       (if signed? (byte->int8 b) b))]
    [else
      (integer-bytes->integer bstr signed? big-endian? start end)]))

(define (integer->integer-bytes* n
                                 size-n
                                 signed?
                                 [big-endian? (system-big-endian?)]
                                 [dest-bstr (make-bytes size-n)]
                                 [start 0])
  (cond
    [(= size-n 1)
     (bytes-set! dest-bstr 0 (if signed? (int8->byte n) n))
     dest-bstr]
    [else
     (integer->integer-bytes n size-n signed? big-endian? dest-bstr start)]))
