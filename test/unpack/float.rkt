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


;;; Float 64 (double precision is the default in Racket)
(check-property
  (property ([f arbitrary-real])
    (let ([packed (bytes-append (bytes #xCB)
                                (real->floating-point-bytes f 8 #t))])
      (= f (call-with-input-bytes packed (λ (in) (unpack in)))))))

;;; Float 32 (similar to the above, except convert to single precision first)
(check-property
  (property ([f arbitrary-real])
    (let* ([f      (real->single-flonum f)]
           [packed (bytes-append (bytes #xCB)
                                 (real->floating-point-bytes f 8 #t))])
      (= f (call-with-input-bytes packed (λ (in) (unpack in)))))))
