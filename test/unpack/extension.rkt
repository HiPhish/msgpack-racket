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
  (file "../../msgpack/main.rkt")
  (file "../../msgpack/unpack.rkt")
  (file "../../msgpack/private/helpers.rkt"))


;;; The type part of an ext is a signed 8-bit integer, i.e. a number between
;;; -2^7 and (2^7)-1.

;;; Fixed ext
(for ([n   (in-vector #(1 2 4 8 16))]
      [tag (in-naturals #xD4)])
  (check-property
    (property ([type (choose-integer (- (expt 2 7)) (sub1 (expt 2 7)))])
      (let* ([data (make-bytes n)]
             [packed (bytes-append (bytes tag (int8->byte type)) data)]
             [unpacked (call-with-input-bytes packed (λ (in) (unpack in)))])
        (and (ext? unpacked)
             (= type (ext-type unpacked))
             (bytes=? data (ext-data unpacked)))))))

;;; Ext 8
(check-property
  (property ([type (choose-integer (- (expt 2 7)) (sub1 (expt 2 7)))]
             [n    (choose-integer              0 (sub1 (expt 2 8)))])
    (let* ([data (make-bytes n)]
           [packed (bytes-append (bytes #xC7 n (int8->byte type)) data)]
           [unpacked (call-with-input-bytes packed (λ (in) (unpack in)))])
      (and (ext? unpacked)
           (= type (ext-type unpacked))
           (bytes=? data (ext-data unpacked))))))

;;; Ext 16
(check-property
  (property ([type (choose-integer (- (expt 2 7)) (sub1 (expt 2  7)))]
             [n    (choose-integer    (expt 2 8)  (sub1 (expt 2 16)))])
    (let* ([data (make-bytes n)]
           [packed (bytes-append (bytes #xC8)
                                 (integer->integer-bytes n 2 #f #t)
                                 (bytes (int8->byte type))
                                 data)]
           [unpacked (call-with-input-bytes packed (λ (in) (unpack in)))])
      (and (ext? unpacked)
           (= type (ext-type unpacked))
           (bytes=? data (ext-data unpacked))))))

;;; I cannot test larger extensions because my machine runs out of memory. If
;;; one datum is one byte large, 2^32 data would take up 4GiB.