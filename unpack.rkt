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

(require (file "ext.rkt")
         (file "private/helpers.rkt")
         (for-syntax racket/base))

(provide unpack)


;;; ===[ Generic unpacking ]==================================================
;;; We will use 'case' to dispatch the particular unpacking function, but that
;;; would require spelling out every byte for the cases where an entire range
;;; is expected, so we will instead use a macro which generates the 'case'
;;; expression, splicing in a list of values where it makes sense.
;;;
;;; We cannot use macros to generate the case-clauses because the clauses are
;;; all quoted, so we have to generate the entire case expression.
(define-syntax (dispatch-on-case stx)
  (syntax-case stx ()
    [(_ tag-var in-expr)
     #`(case tag-var
         [(#,@(for/list ([i (in-range #x00 #x80)]) (datum->syntax stx i)))
          tag-var]
         [(#,@(for/list ([i (in-range #x80 #x90)]) (datum->syntax stx i)))
          (unpack-map (bitwise-and #b00001111 tag-var) in-expr)]
         [(#,@(for/list ([i (in-range #x90 #xA0)]) (datum->syntax stx i)))
          (unpack-array  (bitwise-and #b00001111 tag-var) in-expr)]
         [(#,@(for/list ([i (in-range #xA0 #xC0)]) (datum->syntax stx i)))
          (unpack-string (bitwise-and #b00011111 tag-var) in-expr)]
         [(#xC0) (void)]  ; nil
         [(#xC1) (error "MessagePack tag 0xC1 is never used")]
         [(#xC2) #f]  ; false
         [(#xC3) #t]  ; true
         [(#xC4) (read-bytes (unpack-integer 1 #f in-expr) in-expr)]  ; bin8
         [(#xC5) (read-bytes (unpack-integer 2 #f in-expr) in-expr)]  ; bin16
         [(#xC6) (read-bytes (unpack-integer 4 #f in-expr) in-expr)]  ; bin32
         [(#xC7) (unpack-ext (unpack-integer 1 #f in-expr) in-expr)]  ; ext8
         [(#xC8) (unpack-ext (unpack-integer 2 #f in-expr) in-expr)]  ; ext16
         [(#xC9) (unpack-ext (unpack-integer 4 #f in-expr) in-expr)]  ; ext32
         [(#xCA) (unpack-float 4 in-expr)]
         [(#xCB) (unpack-float 8 in-expr)]
         [(#xCC) (unpack-integer 1 #f in-expr)]
         [(#xCD) (unpack-integer 2 #f in-expr)]
         [(#xCE) (unpack-integer 4 #f in-expr)]
         [(#xCF) (unpack-integer 8 #f in-expr)]
         [(#xD0) (unpack-integer 1 #t in-expr)]
         [(#xD1) (unpack-integer 2 #t in-expr)]
         [(#xD2) (unpack-integer 4 #t in-expr)]
         [(#xD3) (unpack-integer 8 #t in-expr)]
         [(#xD4) (unpack-ext    1 in-expr)]
         [(#xD5) (unpack-ext    2 in-expr)]
         [(#xD6) (unpack-ext    4 in-expr)]
         [(#xD7) (unpack-ext    8 in-expr)]
         [(#xD8) (unpack-ext   16 in-expr)]
         [(#xD9) (unpack-string (unpack-integer 1 #f in-expr) in-expr)]
         [(#xDA) (unpack-string (unpack-integer 2 #f in-expr) in-expr)]
         [(#xDB) (unpack-string (unpack-integer 4 #f in-expr) in-expr)]
         [(#xDC) (unpack-array  (unpack-integer 2 #f in-expr) in-expr)]
         [(#xDD) (unpack-array  (unpack-integer 4 #f in-expr) in-expr)]
         [(#xDE) (unpack-map    (unpack-integer 2 #f in-expr) in-expr)]
         [(#xDF) (unpack-map    (unpack-integer 4 #f in-expr) in-expr)]
         [(#,@(for/list ([i (in-range #xE0 #x100)]) (datum->syntax stx i)))
          (integer-bytes->integer* (bytes tag-var) #t #t)]
         [else (error "Unknown tag " tag-var)])]))

(: unpack (-> Input-Port Any))
(define (unpack in)
  (define tag (read-byte in))
  (cond
    [(byte? tag) (dispatch-on-case tag in)]
    [else (raise-eof-exception)]))


(define (raise-eof-exception)
  (raise (exn:fail:read "Unexpected EOF" (current-continuation-marks) '())))


;;; ===[ Integers ]===========================================================
(: unpack-integer (-> Integer Boolean Input-Port Integer))
(define (unpack-integer size signed? in)
  (define in-bytes (read-bytes size in))
  (cond
    [(bytes? in-bytes) (integer-bytes->integer* in-bytes signed? #t)]
    [else (raise-eof-exception)]))


;;; ===[ Floating point numbers ]=============================================
(: unpack-float (-> Integer Input-Port Real))
(define (unpack-float size in)
  (define in-bytes (read-bytes size in))
  (cond
    [(bytes? in-bytes) (floating-point-bytes->real in-bytes #t)]
    [else (raise-eof-exception)]))


;;; ===[ Unicode strings ]====================================================
(: unpack-string (-> Integer Input-Port String))
(define (unpack-string size in)
  (define in-bytes (read-bytes size in))
  (cond
    [(bytes? in-bytes) (bytes->string/utf-8 in-bytes)]
    [else (raise-eof-exception)]))


;;; ===[ Arrays ]=============================================================
(: unpack-array (-> Integer Input-Port VectorTop))
(define (unpack-array size in)
  (for/vector #:length size ([_ (in-range size)])
    (unpack in)))


;;; ===[ Maps ]===============================================================
(: unpack-map (-> Integer Input-Port HashTableTop))
(define (unpack-map size in)
  (for/hash ([_ (in-range size)])
    (values (unpack in)
            (unpack in))))


;;; ===[ Extensions ]=========================================================
(: unpack-ext (-> Integer Input-Port Ext))
(define (unpack-ext size in)
  (define type (read-bytes    1 in))
  (define data (read-bytes size in))
  (cond
    [(and (bytes? type) (bytes? data))
     (ext (integer-bytes->integer* type #t #t) data)]
    [else (raise-eof-exception)]))
