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

(module+ test
  (require racket/port
           quickcheck
           rackunit/quickcheck
           msgpack
           (file "../../private/helpers.rkt"))


  ;;; Fixed string
  (check-property
    (property ([n (choose-integer 0 (sub1 (expt 2 5)))])
      (let* ([str      (make-string n)]
             [packed   (bytes-append (bytes (bitwise-ior #b10100000 n))
                                     (string->bytes/utf-8 str))]
             [unpacked (call-with-input-bytes packed (λ (in) (unpack in)))])
        (string=? str unpacked))))

  ;;; String 8, 16
  (for ([size (in-vector #(8 16))]
        [tag  (in-naturals #xD9)])
    (check-property
      (property ([n (choose-integer 0 (sub1 (expt 2 size)))])
        (let* ([str      (make-string n)]
               [packed   (bytes-append (bytes tag)
                                       (integer->integer-bytes* n
                                                                (/ size 8)
                                                                #f
                                                                #t)
                                       (string->bytes/utf-8 str))]
               [unpacked (call-with-input-bytes packed (λ (in) (unpack in)))])
          (string=? str unpacked))))))

;;; I cannot test larger strings because my machine runs out of memory.
;;; 2^16B is 64MiB, and 2^32 is 4GiB.
