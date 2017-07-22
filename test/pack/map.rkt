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
#lang racket

(require
  quickcheck
  rackunit/quickcheck
  (file "../../msgpack/pack.rkt")
  (file "../../msgpack/unpack.rkt"))


;;; Racket hash tables are packed to MessagePack maps. For simplicity we will
;;; fill the hash table with key-value pairs where every entry is the same
;;; number.
;;;
;;; To verify the packed data we iterate over the number of items, grabbing the
;;; key and value from the packed byte string. The index of the key is 2i+1 and
;;; the index of the value is 2i+2.

;;; Fixed map
(check-property
  (property ([len (choose-integer 0 #b00001111)])
    (let ([out  (open-output-bytes)]
          (hash (for/hash ([i (in-range 0 len)]) (values i i))))
      (pack-map hash out)
      (let ([bstr (get-output-bytes out)])
        (and (= (bytes-ref bstr 0) (bitwise-ior #b10000000 len))
             (for/and ([i (in-range 0 len)])
               (let ([key (bytes-ref bstr (+ (* 2 i) 1))]
                     [val (bytes-ref bstr (+ (* 2 i) 2))])
                 (= val (hash-ref hash key)))))))))

;;; Map 16
;;; I think this one needs some explanation. The first packed byte is the tag,
;;; the next two are the size, nothing surprising here. As for the rest, we
;;; have to treat everything from the fourth byte onwards as an input port and
;;; unpack the values in order. We then compare the hash-reference of the
;;; unpacked key with the unpacked value.
(check-property
  (property ([len (choose-integer #b00010000 (sub1 (expt 2 16)))])
    (let ([out (open-output-bytes)]
          [hash (for/hash ([i (in-range 0 len)]) (values i i))])
      (pack-map hash out)
      (let ([bstr (get-output-bytes out)])
        (and (= #xDE (bytes-ref bstr 0))
             (= len (integer-bytes->integer bstr #f #t 1 3))
             (for/and
               ([key-val (in-port
                           (λ (in) (if (eof-object? (peek-byte in))
                                     eof
                                     (cons (unpack in) (unpack in))))
                           (open-input-bytes (subbytes bstr 3)))])
               (= (hash-ref hash (car key-val)) (cdr key-val))))))))

;;; I cannot test larger maps because my machine runs out of memory. If one
;;; one key or value is one byte large, 2^32 key-value pairs would take up
;;; 8GiB.
