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
  (file "../../msgpack/unpack.rkt"))


;;; Fixed string
(check-property
  (property ([n (choose-integer 0 (sub1 (expt 2 5)))])
    (let* ([str      (make-string n)]
           [packed   (bytes-append (bytes (bitwise-ior #b10100000 n))
                                   (string->bytes/utf-8 str))]
           [unpacked (call-with-input-bytes packed (λ (in) (unpack in)))])
      (string=? str unpacked))))

;;; String 8
(check-property
  (property ([n (choose-integer 0 (sub1 (expt 2 8)))])
    (let* ([str      (make-string n)]
           [packed   (bytes-append (bytes #xD9 n) (string->bytes/utf-8 str))]
           [unpacked (call-with-input-bytes packed (λ (in) (unpack in)))])
      (string=? str unpacked))))

;;; String 16
(check-property
  (property ([n (choose-integer 0 (sub1 (expt 2 16)))])
    (let* ([str      (make-string n)]
           [packed   (bytes-append (bytes #xDA)
                                   (integer->integer-bytes n 2 #f #t)
                                   (string->bytes/utf-8 str))]
           [unpacked (call-with-input-bytes packed (λ (in) (unpack in)))])
      (string=? str unpacked))))

;;; I cannot test larger strings because my machine runs out of memory.
;;; 2^16B is 64MiB, and 2^32 is 4GiB.
