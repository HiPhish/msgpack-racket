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
           racket/vector
           quickcheck
           rackunit/quickcheck
           (file "../../main.rkt")
           (file "../../private/helpers.rkt"))


  ;;; The type part of an ext is a signed 8-bit integer, i.e. a number between
  ;;; -2^7 and (2^7)-1.

  ;;; Fixed ext
  (for ([n   (in-vector #(1 2 4 8 16))]
        [tag (in-naturals #xD4)])
    (check-property
      (property ([type (choose-integer (- (expt 2 7)) (sub1 (expt 2 7)))])
        (let ([ext (ext type (make-bytes n))])
          (bytes=? (call-with-output-bytes (λ (out) (pack ext out)))
                   (bytes-append (bytes tag (int8->byte type))
                                 (ext-data ext)))))))

  ;;; Ext 8, 16
  (for ([size (in-vector #(8 16))]
        [tag  (in-naturals #xC7)])
    (check-property
      (property ([type (choose-integer (- (expt 2 7))
                                       (sub1 (expt 2  7)))]
                 [n    (choose-integer (expt 2 (- size 8))
                                       (sub1 (expt 2 size)))])
        ;; Skip over the fixed sizes
        (if (vector-member n #(1 2 4 8 16))
          #t
          (let ([ext (ext type (make-bytes n))])
            (bytes=? (call-with-output-bytes (λ (out) (pack ext out)))
                     (bytes-append (bytes tag)
                                   (integer->integer-bytes* n (/ size 8) #f #t)
                                   (integer->integer-bytes* type 1 #t #t)
                                   (ext-data ext)))))))))

;;; I cannot test larger extensions because my machine runs out of memory. If
;;; one datum is one byte large, 2^32 data would take up 4GiB.
