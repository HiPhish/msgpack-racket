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
           (file "../../main.rkt"))

  ;;; Fixed array
  (check-property
    (property ([n (choose-integer 0 15)])
      (let* ([vec (make-vector n '())]
             [packed (bytes-append (bytes (bitwise-ior #b10010000 n))
                                   (make-bytes n #xC0))]
             [unpacked (call-with-input-bytes packed (λ (in) (unpack in)))])
        (and (vector? unpacked)
             (for/and ([v1 (in-vector      vec)]
                       [v2 (in-vector unpacked)])
               (eq? v1 v2))))))

  ;;; Array16
  (check-property
    (property ([n (choose-integer 0 (sub1 (expt 2 16)))])
      (let* ([vec (make-vector n '())]
             [packed (bytes-append (bytes #xDC)
                                   (integer->integer-bytes n 2 #f #t)
                                   (make-bytes n #xC0))]
             [unpacked (call-with-input-bytes packed (λ (in) (unpack in)))])
        (and (vector? unpacked)
             (for/and ([v1 (in-vector      vec)]
                       [v2 (in-vector unpacked)])
               (eq? v1 v2)))))))

;;; I cannot test larger array because my machine runs out of memory. If one
;;; one element is one byte large, 2^32 element would take up 4GiB.
