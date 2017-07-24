#lang racket

;;; I need this because 'integer->integer-bytes' does not support 8-bit
;;; integers.
(define (int8->byte i) (if (< i 0) (+ #x100 i) i))

(provide
  (contract-out
    [int8->byte (-> (λ (i) (<= -128 i 127)) byte?)]))
