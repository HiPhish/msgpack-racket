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
           "../../main.rkt")

  ;;; There is a little trick to convert a negative number to its 2's
  ;;; complement byte: add the negative number to 256 or #x100

  ;;; Positive fixint
  (check-property
    (property ([i (choose-integer 0 #b01111111)])
      (= i (call-with-input-bytes (bytes i) (λ (in) (unpack in))))))

  ;;; Negative fixint
  (check-property
    (property ([i (choose-integer -32 -1)])
      (= i (call-with-input-bytes (integer->integer-bytes i 1 #t)
                                  (λ (in) (unpack in))))))

  ;;; Uint and Int 16, 32, 64
  (for ([n   (in-vector #(8 16 32 64))]
        [tag (in-naturals #xCC)])
    (for ([signed? (in-list '(#f #t))])
      (check-property
        (property ([i (choose-integer (if signed? (- (expt 2 (sub1 n))) 0)
                                      (sub1 (expt 2 (if signed? (sub1 n) n))))])
          (= i
             (call-with-input-bytes
               ;; For signed types the tag is offset by 4
               (bytes-append (bytes (if signed? (+ 4 tag) tag))
                             (integer->integer-bytes i (/ n 8) signed? #t))
               (λ (in) (unpack in)))))))))
