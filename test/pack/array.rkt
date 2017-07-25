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
           racket/list
           quickcheck
           rackunit/quickcheck
           (file "../../main.rkt"))


  ;;; For every vector we need to pack the vector as well as its contents. We
  ;;; will pack only a '() for simplicity, but we need to check that the '() has
  ;;; been packed properly.

  ;;; A note about packing lists: if the length of a list is zero it will be
  ;;; packed as a nil object rather than an array.

  ;;; Fixed array
  (check-property
    (property ([len (choose-integer 0 #b00001111)])
      (let* ([vec    (make-vector len '())]
             [lst    (make-list   len #t)]
             [packed-v (call-with-output-bytes (λ (out) (pack vec out)))]
             [packed-l (call-with-output-bytes (λ (out) (pack lst out)))])
        (and (= (bytes-length packed-v) (+ 1 (vector-length vec)))
             (= (bytes-ref packed-v 0) (bitwise-ior #b10010000 len))
             (unless (= len 0)
               (= (bytes-length packed-l) (+ 1 (length        lst)))
               (= (bytes-ref packed-l 0) (bitwise-ior #b10010000 len)))
             (for/and ([i (in-range 1 len)])
               (and (= (bytes-ref packed-v i) #xC0)
                    (if (= len 0) #t (= (bytes-ref packed-l i) #xC3))))))))

  ;;; Array16
  (check-property
    (property ([len (choose-integer 16 (sub1 (expt 2 16)))])
      (let* ([vec    (make-vector len '())]
             [lst    (make-list   len '())]
             [packed-v (call-with-output-bytes (λ (out) (pack vec out)))]
             [packed-l (call-with-output-bytes (λ (out) (pack lst out)))])
        (and (= (bytes-length packed-v) (+ 3 (vector-length vec)))
             (= (bytes-length packed-l) (+ 3 (length        lst)))
             (= (bytes-ref packed-v 0) #xDC)
             (= (bytes-ref packed-l 0) #xDC)
             (for/and ([i (in-range 0 len)])
               (and (= (bytes-ref packed-v (+ i 3)) #xC0)
                    (= (bytes-ref packed-l (+ i 3)) #xC0))))))))

;;; I cannot test larger array because my machine runs out of memory. If one
;;; one element is one byte large, 2^32 element would take up 4GiB.
