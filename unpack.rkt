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

(require racket/contract/base
         (file "ext.rkt")
         (file "private/helpers.rkt"))

(provide
  (contract-out
    [unpack (-> (and/c input-port? (not/c port-closed?)) any/c)]))


;;; ===[ Generic unpacking ]==================================================
(define (unpack in)
  (let ([tag (read-byte in)])
    (cond
      [(<= #x00 tag #x7F) tag]
      [(<= #x80 tag #x8F) (unpack-map    (bitwise-and #b00001111 tag) in)]
      [(<= #x90 tag #x9F) (unpack-array  (bitwise-and #b00001111 tag) in)]
      [(<= #xA0 tag #xBF) (unpack-string (bitwise-and #b00011111 tag) in)]
      [(= #xC0 tag) '()]  ; nil
      [(= #xC1 tag) (error "MessagePack tag 0xC1 is never used")]
      [(= #xC2 tag) #f]  ; false
      [(= #xC3 tag) #t]  ; true
      [(= #xC4 tag) (read-bytes (unpack-integer  8 #f in) in)]  ; bin8
      [(= #xC5 tag) (read-bytes (unpack-integer 16 #f in) in)]  ; bin16
      [(= #xC6 tag) (read-bytes (unpack-integer 32 #f in) in)]  ; bin32
      [(= #xC7 tag) (unpack-ext (unpack-integer  8 #f in) in)]  ; bin64
      [(= #xC8 tag) (unpack-ext (unpack-integer 16 #f in) in)]
      [(= #xC9 tag) (unpack-ext (unpack-integer 32 #f in) in)]
      [(= #xCA tag) (unpack-float 32 in)]
      [(= #xCB tag) (unpack-float 64 in)]
      [(= #xCC tag) (unpack-integer  8 #f in)]
      [(= #xCD tag) (unpack-integer 16 #f in)]
      [(= #xCE tag) (unpack-integer 32 #f in)]
      [(= #xCF tag) (unpack-integer 64 #f in)]
      [(= #xD0 tag) (unpack-integer  8 #t in)]
      [(= #xD1 tag) (unpack-integer 16 #t in)]
      [(= #xD2 tag) (unpack-integer 32 #t in)]
      [(= #xD3 tag) (unpack-integer 64 #t in)]
      [(= #xD4 tag) (unpack-ext    1 in)]
      [(= #xD5 tag) (unpack-ext    2 in)]
      [(= #xD6 tag) (unpack-ext    4 in)]
      [(= #xD7 tag) (unpack-ext    8 in)]
      [(= #xD8 tag) (unpack-ext   16 in)]
      [(= #xD9 tag) (unpack-string (unpack-integer  8 #f in) in)]
      [(= #xDA tag) (unpack-string (unpack-integer 16 #f in) in)]
      [(= #xDB tag) (unpack-string (unpack-integer 32 #f in) in)]
      [(= #xDC tag) (unpack-array  (unpack-integer 16 #f in) in)]
      [(= #xDD tag) (unpack-array  (unpack-integer 32 #f in) in)]
      [(= #xDE tag) (unpack-map    (unpack-integer 16 #f in) in)]
      [(= #xDF tag) (unpack-map    (unpack-integer 32 #f in) in)]
      [(<= #xE0 tag #xFF) (integer-bytes->integer* (bytes tag) #t #t)]
      [else (error "Unknown tag")])))


;;; ===[ Integers ]===========================================================
(define (unpack-integer size signed? in)
  (integer-bytes->integer* (read-bytes (/ size 8) in) signed? #t))


;;; ===[ Floating point numbers ]=============================================
(define (unpack-float size in)
  (floating-point-bytes->real (read-bytes (/ size 8) in) #t))


;;; ===[ Unicode strings ]====================================================
(define (unpack-string size in)
  (let ((bstr (read-bytes size in)))
    (bytes->string/utf-8 bstr)))


;;; ===[ Arrays ]=============================================================
(define (unpack-array size in)
  (for/vector #:length size ([_ (in-range size)])
    (unpack in)))


;;; ===[ Maps ]===============================================================
(define (unpack-map size in)
  (for/hash ([_ (in-range size)])
    (values (unpack in)
            (unpack in))))


;;; ===[ Extensions ]=========================================================
(define (unpack-ext size in)
  (ext (unpack-integer 8 #t in)
       (read-bytes size in)))
