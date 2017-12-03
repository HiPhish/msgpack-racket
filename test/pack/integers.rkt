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


  ;;; Integers can be packed in many ways, we need to use these predicates to
  ;;; narrow down the type
  (define (uint8?  i) (<= 0 i (sub1 (expt 2  8))))
  (define (uint16? i) (<= 0 i (sub1 (expt 2 16))))
  (define (uint32? i) (<= 0 i (sub1 (expt 2 32))))
  (define (uint64? i) (<= 0 i (sub1 (expt 2 64))))

  (define (int8?  i) (<= (- (expt 2  7)) i))
  (define (int16? i) (<= (- (expt 2 15)) i))
  (define (int32? i) (<= (- (expt 2 31)) i))
  (define (int64? i) (<= (- (expt 2 63)) i))


  (with-medium-test-count
    (check-property
      ;; The total possible range for an integer is from the minimum value of
      ;; an int64 to the maximum value of a uint64.
      (property ([i (choose-integer (- (expt 2 63)) (sub1 (expt 2 64)))])
        (bytes=?
          (call-with-output-bytes (λ (out) (pack i out)))
          (cond [(<= 0 i  127) (integer->integer-bytes i 1 #f #t)]
                [(<= -32 i -1) (integer->integer-bytes i 1 #t #t)]
                [(uint8?  i)
                 (bytes-append (bytes #xCC)
                               (integer->integer-bytes i 1 #f #t))]
                [(uint16? i)
                 (bytes-append (bytes #xCD)
                               (integer->integer-bytes i 2 #f #t))]
                [(uint32? i)
                 (bytes-append (bytes #xCE)
                               (integer->integer-bytes i 4 #f #t))]
                [(uint64? i)
                 (bytes-append (bytes #xCF)
                               (integer->integer-bytes i 8 #f #t))]
                [(int8?  i)
                 (bytes-append (bytes #xD0)
                               (integer->integer-bytes i 1 #t #t)) ]
                [(int16? i)
                 (bytes-append (bytes #xD1)
                               (integer->integer-bytes i 2 #t #t))]
                [(int32? i)
                 (bytes-append (bytes #xD2)
                               (integer->integer-bytes i 4 #t #t))]
                [(int64? i)
                 (bytes-append (bytes #xD3)
                               (integer->integer-bytes i 8 #t #t))]
                [else (error "Number " i " outside range in test.")]))))))
