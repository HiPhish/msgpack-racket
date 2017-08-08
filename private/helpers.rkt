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

(provide
  integer-bytes->integer*
  integer->integer-bytes*)


;;; I need this because 'integer->integer-bytes' does not support 8-bit
;;; integers.

(: int8->byte (-> Integer Integer))
(define (int8->byte i)
  (if (< i 0) (+ #x100 i) i))

(: byte->int8 (-> Integer Integer))
(define (byte->int8 b)
  (if (not (zero? (bitwise-and b #b10000000))) (- b #x100) b))


;;; An extension of 'integer->integer-bytes' which adds support for 8-bit
;;; numbers.

(: integer-bytes->integer* (->* (Bytes Boolean)
                                (Boolean Integer Integer)
                                Integer))
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

(: integer->integer-bytes* (->* (Integer Integer Boolean)
                                (Boolean Bytes Integer)
                                Bytes))
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
