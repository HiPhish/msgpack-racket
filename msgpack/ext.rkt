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
#lang typed/racket/base


;;; The MessagePack ext type does not have a direct correspondence to a Racket
;;; type.
(struct ext
  ([type : Integer] [data : Bytes])
  #:transparent
  #:type-name Ext
  #:guard (λ (type data name)
            (unless (<= -128 type 127)  ; Must be signed 8-bit integer
              (raise-arguments-error 'ext "type must be within [-128, 127]"
                                     "type" type
                                     "data" data))
            (values type data)))

(provide (struct-out ext) Ext)
