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

(require "ext.rkt")

(provide Packable packable?)

(define-type Packable
  (U Void
     Boolean
     Integer
     Real
     String
     Bytes
     (Vectorof Packable)
     (Listof Packable)
     (HashTable Packable Packable)
     Ext))

(: packable? (-> Any Boolean))
(define (packable? x)
  (cond
    [(void?    x) #t]
    [(boolean? x) #t]
    [(integer? x) #t]
    ((real?    x) #t)
    [(string?  x) #t]
    [(bytes?   x) #t]
    [(ext?     x) #t]
    [(vector?  x) (for/and ([v (in-vector x)]) (packable? v))]
    [(list?    x) (for/and ([v (in-list   x)]) (packable? v))]
    [(hash?    x)
     (for/and ([(k v) (in-hash x)]) (and (packable? k) (packable? v)))]
    [else #f]))
