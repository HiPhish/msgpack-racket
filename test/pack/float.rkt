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


;;; All floating point real numbers have double-precision by default in Racket.
;;; This means we have to test once using the regular arbitrary real number,
;;; and once using the number converted to single precision.

;;; Predicate which adjusts to precision automatically.
(define (packed-properly? f packed)
  (define tag  (if (single-flonum? f) #xCA #xCB))
  (define size (if (single-flonum? f)    4    8))
  (bytes=? packed
           (bytes-append (bytes tag)
                         (real->floating-point-bytes f size #t))))

(check-property
  (property ([f arbitrary-real])
    (and
      ;; Double precision
      (let ([packed (call-with-output-bytes (λ (out) (pack f out)))])
        (packed-properly? f packed)))
      ;; Single precision
      (let* ([f (real->single-flonum f)]
             [packed (call-with-output-bytes (λ (out) (pack f out)))])
        (packed-properly? f packed))))
