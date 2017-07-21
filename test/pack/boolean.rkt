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
  rackunit
  "../../msgpack/pack.rkt")

;;; There are two values and two functions: the generic 'pack' and the more
;;; specific 'pack-boolean'. We need to test that both functions perform the
;;; same for each value.
(for ([val (in-list '(#f   #t))]
      [tag (in-list '(#xC2 #xC3))])
  (for ([pack-func (in-list (list pack pack-boolean))])
    (let ([out (open-output-bytes)])
      (pack-func val out)
      (check
        bytes=?
        (get-output-bytes out)
        (bytes tag)))))
