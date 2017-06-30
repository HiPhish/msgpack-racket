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

(require rackunit "unpack.rkt")

(define-syntax test
  (syntax-rules ()
    [(_ pred expect (byte ...))
     (check pred expect (unpack (open-input-bytes (bytes byte ...))))]))

;;; nil format
(test eq? '() (#xC0))

;;; bool format family
(test eq? #f (#xC2))
(test eq? #t (#xC3))

;;; int format family
;;; --- uint7
(test = #x00 (#x00))
(test = #x10 (#x10))
(test = #x7f (#x7F))
;;; --- int5
(test =  -1 (#xFF))
(test = -16 (#xF0))
(test = -32 (#xE0))
;;; --- uint8
(test = #x80 (#xCC #x80))
(test = #xf0 (#xCC #xF0))
(test = #xff (#xCC #xFF))
;;; --- uint16
(test = #x100  (#xCD #x01 #x00))
(test = #x2000 (#xCD #x20 #x00))
(test = #xFFFF (#xCD #xFF #xFF))
;;; --- uint32
(test = #x10000 (#xce #x00 #x01 #x00 #x00))
(test = #x200000 (#xce #x00 #x20 #x00 #x00))
(test = #xffffffff (#xce #xff #xff #xff #xff))
;;; --- uint64
(test = #x100000000 (#xcf #x00 #x00 #x00 #x01 #x00 #x00 #x00 #x00))
(test = #x200000000000 (#xcf #x00 #x00 #x20 #x00 #x00 #x00 #x00 #x00))
(test = #xffffffffffffffff (#xcf #xff #xff #xff #xff #xff #xff #xff #xff))
;;; --- int8
(test = -33 (#xd0 #xdf))
(test = -100 (#xd0 #x9c))
(test = -128 (#xd0 #x80))
;;; --- int16
(test = -129 (#xd1 #xff #x7f))
(test = -2000 (#xd1 #xf8 #x30))
(test = -32768 (#xd1 #x80 #x00))
;;; --- int32
(test = -32769 (#xd2 #xff #xff #x7f #xff))
(test = -1000000000 (#xd2 #xc4 #x65 #x36 #x00))
(test = -2147483648 (#xd2 #x80 #x00 #x00 #x00))
;;; --- int64
(test = -2147483649 (#xd3 #xff #xff #xff #xff #x7f #xff #xff #xff))
(test = -1000000000000000002 (#xd3 #xf2 #x1f #x49 #x4c #x58 #x9b #xff #xfe))
(test = -9223372036854775808 (#xd3 #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00))

;;; float family
;;; --- float 32
(test = 0.0 (#xCA #x00 #x00 #x00 #x00))
(test = 2.0625 (#xCA #x40 #x04 #x00 #x00))
(let ([f (floating-point-bytes->real (bytes #x79 #x9A #x13 #x0C) #t)])
  (test = f (#xCA #x79 #x9A #x13 #x0C)))
;;; --- float 64
(test = 0.0 (#xCB #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00))
(test = 2.5 (#xCB #x40 #x04 #x00 #x00 #x00 #x00 #x00 #x00))
(test = 1e+35 (#xCB #x47 #x33 #x42 #x61 #x72 #xC7 #x4D #x82))

;;; str format family
;;; --- fixstr
(test string=? ""    (#xA0))
(test string=? "a"   (#xA1 #x61))
(test string=? "abc" (#xA3 #x61 #x62 #x63))
(check string=?
       (make-string 31 #\a)
       (unpack (open-input-bytes (bytes-append (bytes #xbf)
                                               (make-bytes 31 #x61)))))
;;; --- string 8
(check string=?
       (make-string  32 #\b)
       (unpack (open-input-bytes (bytes-append (bytes #xD9 #x20)
                                               (make-bytes  32 #x62)))))
(check string=?
       (make-string 100 #\c)
       (unpack (open-input-bytes (bytes-append (bytes #xD9 #x64)
                                               (make-bytes 100 #x63)))))
(check string=?
       (make-string 255 #\d)
       (unpack (open-input-bytes (bytes-append (bytes #xD9 #xFF)
                                               (make-bytes 255 #x64)))))
;;; --- string 16
(check string=?
       (make-string   256 #\b)
       (unpack (open-input-bytes (bytes-append (bytes #xDA #x01 #x00)
                                               (make-bytes   256 #x62)))))
(check string=?
       (make-string 65535 #\c)
       (unpack (open-input-bytes (bytes-append (bytes #xDA #xFF #xFF)
                                               (make-bytes 65535 #x63)))))
;;; --- string 32
(check string=?
       (make-string 65536 #\b)
       (unpack (open-input-bytes (bytes-append (bytes #xDB #x00 #x01 #x00 #x00)
                                               (make-bytes 65536 #x62)))))
;;; --- UTF-8 string
(test string=? "мсгпак" (#xAC #xD0 #xBC #xD1 #x81 #xD0 #xB3 #xD0 #xBF #xD0 #xB0 #xD0 #xBA))

;;; bin format family
;;; --- bin 8
(check bytes=?
       (make-bytes #x01 #x80)
       (unpack (open-input-bytes (bytes-append (bytes #xC4 #x01) (make-bytes #x01 #x80)))))
(check bytes=?
       (make-bytes #x20 #x80)
       (unpack (open-input-bytes (bytes-append (bytes #xC4 #x20) (make-bytes #x20 #x80)))))
(check bytes=?
       (make-bytes #xFF #x80)
       (unpack (open-input-bytes (bytes-append (bytes #xC4 #xFF) (make-bytes #xFF #x80)))))
;;; --- bin 16
(check bytes=?
       (make-bytes #x100 #x80)
       (unpack (open-input-bytes (bytes-append (bytes #xC5 #x01 #x00) (make-bytes #x100 #x80)))))
;;; --- bin 32
(check bytes=?
       (make-bytes #x10000 #x80)
       (unpack (open-input-bytes (bytes-append (bytes #xC6 #x00 #x01 #x00 #x00) (make-bytes #x10000 #x80)))))

;;; ext format family 
;;; --- fixext 1
(define (test-fixext tag size)
  ;; Hardcode #x05 as the "type" and #x80 as the "data"
  (let ([bstr (bytes-append (bytes tag #x05) (make-bytes size #x80))])
    (let-values ([(type data) (unpack (open-input-bytes bstr))])
      (check-= #x05 type 0)
      (check bytes=? data (make-bytes size #x80)))))

(test-fixext #xD4 1)
(test-fixext #xD5 2)
(test-fixext #xD6 4)
(test-fixext #xD7 8)
(test-fixext #xD8 16)
;;; --- ext 8
;;; --- ext 16
;;; --- ext 32
