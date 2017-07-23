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

(module+ test
  (require rackunit))

(require "../main.rkt")
(provide
  (contract-out
    [unpack (-> (and/c input-port? (not/c port-closed?)) any/c)]))

;;; Use type bytes to decide how to unpack the data; return extracted datum.
(define (unpack in)
  (let ([tag (read-byte in)])
    (cond
      [(<= #x00 tag #x7F) tag]
      [(<= #x80 tag #x8F) (unpack-map    (bitwise-and #b11110000 tag) in)]
      [(<= #x90 tag #x9F) (unpack-array  (bitwise-and #b11110000 tag) in)]
      [(<= #xA0 tag #xBF) (unpack-string (bitwise-and #b00011111 tag) in)]
      [(= #xC0 tag) '()]
      [(= #xC1 tag) (error "0xC1 is never used")]
      [(= #xC2 tag) #false]
      [(= #xC3 tag) #true]
      [(= #xC4 tag) (read-bytes (unpack-uint  8 in) in)]
      [(= #xC5 tag) (read-bytes (unpack-uint 16 in) in)]
      [(= #xC6 tag) (read-bytes (unpack-uint 32 in) in)]
      [(= #xC7 tag) (unpack-ext (unpack-uint  8 in) in)]
      [(= #xC8 tag) (unpack-ext (unpack-uint 16 in) in)]
      [(= #xC9 tag) (unpack-ext (unpack-uint 32 in) in)]
      [(= #xCA tag) (unpack-float 32 in)]
      [(= #xCB tag) (unpack-float 64 in)]
      [(= #xCC tag) (unpack-uint   8 in)]
      [(= #xCD tag) (unpack-uint  16 in)]
      [(= #xCE tag) (unpack-uint  32 in)]
      [(= #xCF tag) (unpack-uint  64 in)]
      [(= #xD0 tag) (unpack-int    8 in)]
      [(= #xD1 tag) (unpack-int   16 in)]
      [(= #xD2 tag) (unpack-int   32 in)]
      [(= #xD3 tag) (unpack-int   64 in)]
      [(= #xD4 tag) (unpack-ext    1 in)]
      [(= #xD5 tag) (unpack-ext    2 in)]
      [(= #xD6 tag) (unpack-ext    4 in)]
      [(= #xD7 tag) (unpack-ext    8 in)]
      [(= #xD8 tag) (unpack-ext   16 in)]
      [(= #xD9 tag) (unpack-string (unpack-uint  8 in) in)]
      [(= #xDA tag) (unpack-string (unpack-uint 16 in) in)]
      [(= #xDB tag) (unpack-string (unpack-uint 32 in) in)]
      [(= #xDC tag) (unpack-array  (unpack-uint 16 in) in)]
      [(= #xDD tag) (unpack-array  (unpack-uint 32 in) in)]
      [(= #xDE tag) (unpack-map    (unpack-uint 16 in) in)]
      [(= #xDF tag) (unpack-map    (unpack-uint 32 in) in)]
      [(<= #xE0 tag #xFF) (- (bitwise-and tag #b00011111) (expt 2 5))]
      [else (error "Unknown tag")])))

;;; --- Unpack individual types
(define (unpack-uint size in)
  (let ([bstr (read-bytes (/ size 8) in)])
    (if (= 1 (bytes-length bstr))
      (bytes-ref bstr 0)
      (integer-bytes->integer bstr #f #t))))

(module+ test
  (for ([bstr (in-list (list
                         (bytes #x23)
                         (bytes #x23 #x45)
                         (bytes #x23 #x45 #x56 #x78)
                         (bytes #x23 #x45 #x56 #x78 #x9A)))]
        [uint (in-list (list
                         #x23
                         #x2345
                         #x23455678
                         #x234556789A))])
    (check-= uint (unpack-uint (* (bytes-length bstr) 8) (open-input-bytes bstr)) 0)))


(define (unpack-int size in)
  (let ([bstr (read-bytes (/ size 8) in)])
    (if (= 1 (bytes-length bstr))
      (let ([i (bytes-ref bstr 0)])
        (if (> i #b01111111)
          (- i #x100)
          i))
      (integer-bytes->integer bstr #t #t))))


(define (unpack-float size in)
  (floating-point-bytes->real (read-bytes (/ size 8) in) #t))


(define (unpack-string size in)
  (let ((bstr (read-bytes size in)))
    (bytes->string/utf-8 bstr)))


(define (unpack-array size in)
  (for/vector #:length size ([_ (in-range size)])
    (unpack in)))


(define (unpack-map size in)
  (for/hash ([_ (in-range size)])
    (values (unpack in)
            (unpack in))))


(define (unpack-ext size in)
  (ext (unpack-int 8 in)
       (read-bytes size in)))
