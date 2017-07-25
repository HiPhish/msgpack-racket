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


  ;;; Fixed map, Map 16
  (for ([size (in-vector (vector #b00001111 (sub1 (expt 2 16))))])
    (check-property
      (property ([n (choose-integer 0 size)])
        (let* ([hash     (for/hash ([i (in-range 0 n)]) (values i i))]
               [packed   (call-with-output-bytes (λ (out) (pack hash out)))]
               [unpacked (call-with-input-bytes packed (λ (in)  (unpack in)))])
          (and (equal? hash unpacked)
               ))))))

;;; I cannot test larger maps because my machine runs out of memory. If one
;;; one key or value is one byte large, 2^32 key-value pairs would take up
;;; 8GiB.
