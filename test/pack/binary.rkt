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
  (file "../../msgpack/pack.rkt"))


(check-property
  (property ([n (choose-integer 0 (sub1 (expt 2 8)))])
    (let ([out (open-output-bytes)]
          [bs  (make-bytes n)])
      (pack-bin bs out)
      (bytes=?
        (get-output-bytes out)
        (bytes-append (bytes #xC4 n) bs)))))

;;; I cannot test strings this large, the Racket virtual machine runs out of
;;; memory
#;(with-test-count 1
  (check-property
    (property ([n (choose-integer (expt 2 16) (sub1 (expt 2 32)))])
      (let* ([out (open-output-bytes)]
             [bs  (make-bytes n)])
        (pack-bin bs out)
        (bytes=?
          (get-output-bytes out)
          (bytes-append (bytes #xC4)
                        (integer->integer-bytes n 2 #f #t)
                        bs))))))
