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


;;; Generate a property which specifies that for a given size and sign the
;;; integers are packed properly. The name of the packing function is generated
;;; from a string and the 'signed?' template variable.
(define-syntax (packs-integer stx)
  (syntax-case stx ()
    [(_ size signed? tag)
     #`(property ([n (choose-integer
                       (if signed? (- (expt 2 (sub1 size))) 0)
                       (sub1 (expt 2 (if signed? (sub1 size) size))))])
         (let ([out (open-output-bytes)])
           (#,(datum->syntax stx
                             (string->symbol
                               (format
                                 (if (syntax->datum #'signed?)
                                   "pack-int~a"
                                   "pack-uint~a")
                                 (syntax->datum #'size))))
            n out)
           (bytes=?
             (get-output-bytes out)
             (bytes-append
               (bytes tag)
               (integer->integer-bytes n (/ size 8) signed? #t)))))]))

;;; 8-bit integers are a more complicated case because we cannot use
;;; integer->integer-bytes and we have to avoid stepping into the range of the
;;; "fixnum" types.
(define-syntax (packs-integer8 stx)
  (syntax-case stx ()
    [(_ signed? tag)
     #`(property ([n (choose-integer
                       (if signed?
                         (- (expt 2 7))
                         #b10000000)
                       (if signed?
                         #b-00011111
                         (sub1 (expt 2 8))))])
         (let ([out (open-output-bytes)])
           (#,(datum->syntax stx
                             (string->symbol
                               (if (syntax->datum #'signed?)
                                 "pack-int8"
                                 "pack-uint8")))
            n out)
           (bytes=?
             (get-output-bytes out)
             (bytes-append
               (bytes tag)
               (bytes (if signed? (bitwise-ior (if (< n 0) #x80 #x00) (abs n)) n))))))]))

(check-property (packs-integer 16 #t #xD1))
(check-property (packs-integer 32 #t #xD2))
(check-property (packs-integer 64 #t #xD3))

(check-property (packs-integer 16 #f #xCD))
(check-property (packs-integer 32 #f #xCE))
(check-property (packs-integer 64 #f #xCF))

(check-property (packs-integer8 #t #xD0))
(check-property (packs-integer8 #f #xCC))

;;; Check positive "fixnum" types separately
(check-property
  (property ([n (choose-integer 0 #b01111111)])
    (let ([out (open-output-bytes)])
      (pack-p-fixint n out)
      (bytes=? (get-output-bytes out) (bytes n)))))

;;; Check negative "fixnum" types separately
(check-property
  (property ([n (choose-integer #b-00011111 -1)])
    (let ([out (open-output-bytes)])
      (pack-n-fixint n out)
      (bytes=? (get-output-bytes out)
               (bytes (bitwise-ior #b11100000 (abs n)))))))
