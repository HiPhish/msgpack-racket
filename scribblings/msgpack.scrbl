#lang scribble/manual
@(require
  scribble/example
  (for-label
    racket
    msgpack))

@title{MessagePack}
@author[@author+email["Alejandro Sanchez" "hiphish@openmailbox.org"]]
@defmodule[msgpack #:no-declare]
@(declare-exporting msgpack msgpack/ext msgpack/pack msgpack/unpack)


@hyperlink["http://msgpack.org/"]{MessagePack} is a binary serialisation
format focused on speed and size. This library allows you to serialise
(@racket[pack]) and de-serialise (@racket[unpack]) Racket objects.

Source code: @url["https://gitlab.com/HiPhish/MsgPack.rkt"]

@table-of-contents[]


@section{Introduction}

When two processes want to exchgange data they need to agree on a protocol for
serialising and de-serialising said data. MessagePack is a protocol designed
with speed and size in mind; this means that serialised data should be as small
as possible and the recipient should be able to de-serialise the data as
quickly as possible. The flipside of this is that the protocol is not easily
readable to humans, but this is not a concern if the data is only meant to be
exchanged beetween processes anyway.

The API of this implementation follows the naming conventions of MessagePack:
we call the serialising process @emph{packing} and the de-serialising process
@emph{unpacking}. Integers have different length and can be either signed
(@code{intN}) or unsigned (@code{uintN}), where @code{N} is the length of the
integer in bytes.

MessagePack objects are converted to Racket objects and vice-versa according
to the following rules:

@tabular[
  #:style 'boxed
  #:column-properties '(left left)
  #:row-properties '(bottom-border ())
  (list (list @bold{MessagePack type}  @bold{Racket type})
        (list "nil"               @racket[(void)])
        (list "true"              @racket[#t])
        (list "false"             @racket[#f])
        (list "positive-fixint,
               negative-fixint,
               uint8,
               uint16,
               uint32,
               uint64,
               int8,
               int16,
               int32,
               int64"
              @racket[integer])
        (list "fixstr,
               str8,
               str16,
               str32"
              @racket[string])
        (list "bin8,
               bin16,
               bin32"
              @racket[bytes])
        (list "float16,
               float32" 
              @racket[real])
        (list "fixarray,
               array16,
               array32"
              @racket[vector list])
        (list "fixmap,
               map16,
               map32"
              @racket[hash])
        (list "fixext1,
               fixext2,
               fixext4,
               fixext8,
               fixext16,
               ext8,
               ext16,
               ext32" 
              @racket[ext]))]

When there is more than one Racket type listed @racket[unpack] will return a
value of the first type listed.


@section{An example session}

Here we have an object in Racket which we wish to pack. The object is a vector
of various other packable Racket objects. Objects are packed to ports, usually
these ports point to files or network connections, but here we will use byte
strings as ports for the sake of simplicity.

@(examples
  #:label '()
  (code:comment "Import the library first")
  (eval:alts (require msgpack) (require racket/port msgpack))

  (code:comment "Here is some data we want to pack: a vector of numbers,")
  (code:comment "nothing, another vector and a string.")
  (define hodgepodge (vector 1 2 (void) '#(3 #t) "foo"))

  (code:comment "Use a byte string as the output port")
  (define packed (call-with-output-bytes (λ (out) (pack hodgepodge out))))
  (code:comment "The entire hodgepodge has now been packed to binary data")
  packed

  (code:comment "If we want our original hodgepodge back we need to unpack it")
  (define unpacked (call-with-input-bytes packed (λ (in) (unpack in))))
  unpacked)

Packing and unpacking are the primitive operations associated with the
MessagePack format, more complex tasks like sending and receiving RPC messages
can then be implemented on top of this library.


@section{MessagePack API}

@subsection{Data types}
@subsubsection{Packable types}
@defmodule[msgpack/packable #:no-declare]

@defidform[Packable]{
  Union of all packable types for use with Typed Racket. Use this as the most
  general type for objects you can send to @racket[pack] or receive from
  @racket[unpack].
}
@defproc[(packable? [x Any]) boolean?]{
  True if @racket[x] can be packed.
}

@subsubsection{MessagePack extension type}
@defmodule[msgpack/ext #:no-declare]
@(declare-exporting msgpack msgpack/ext)

@defidform[Ext]{
  The type of an @racket[ext] structure for use with Typed Racket.
}
@defstruct*[ext ([type integer?] [data bytes?])]{
  Represents a MessagePack extension type, a pair of a signed 8-bit
  @racket[type] integer and a @racket[data] byte string. The type name for
  Typed Racket is @racket[Ext].
}


@subsection{Packing}
@defmodule[msgpack/pack #:no-declare]
@(declare-exporting msgpack msgpack/pack)

@defproc[(pack [datum any/c] [out (and/c output-port? (not/c port-closed?))]) any]{
  Pack @racket[datum] into the @racket[out] port. The type to pack
  @racket[datum] to will be determined automatically to use the least amount
  of space.
}


@subsection{Unpacking}
@defmodule[msgpack/unpack #:no-declare]
@(declare-exporting msgpack msgpack/unpack)

@defproc[(unpack [in (and/c input-port? (not/c port-closed?))]) any]{
  Unpack a datum from @racket[in]. At least one byte is consumed in the process
  to read the tag, more bytes are consumed as needed by the type of data.
}
