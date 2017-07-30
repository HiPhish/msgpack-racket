#lang scribble/manual
@(require
  (for-label
    racket
    msgpack))

@title{MessagePack}
@defmodule[msgpack #:no-declare]
@(declare-exporting msgpack msgpack/ext msgpack/pack msgpack/unpack)


@hyperlink["http://msgpack.org/"]{MessagePack} is a binary serialisation
format focused on speed and size. This library allows you to serialise
(@racket[pack]) and de-serialise (@racket[unpack]) Racket objects.

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
        (list "positive fixint"   @racket[integer])
        (list "negative fixint"   'cont)
        (list "uint 8"            'cont)
        (list "uint 16"           'cont)
        (list "uint 32"           'cont)
        (list "uint 64"           'cont)
        (list "int 8"             'cont)
        (list "int 16"            'cont)
        (list "int 32"            'cont)
        (list "int 64"            'cont)
        (list "fixstr"            @racket[string])
        (list "str 8"             'cont)
        (list "str 16"            'cont)
        (list "str 32"            'cont)
        (list "bin 8"             @racket[bytes])
        (list "bin 16"            'cont)
        (list "bin 32"            'cont)
        (list "float 16"          @racket[real])
        (list "float 32"          'cont)
        (list "fixarray"          @racket[vector list])
        (list "array 16"          'cont)
        (list "array 32"          'cont)
        (list "fixmap"            @racket[hash])
        (list "map 16"            'cont)
        (list "map 32"            'cont)
        (list "fixext 1"          @racket[ext])
        (list "fixext 2"          'cont)
        (list "fixext 4"          'cont)
        (list "fixext 8"          'cont)
        (list "fixext 16"         'cont)
        (list "ext 8"             'cont)
        (list "ext 16"            'cont)
        (list "ext 32"            'cont))]


@section{Data types}
@defmodule[msgpack/ext #:no-declare]
@(declare-exporting msgpack msgpack/ext)

@defstruct*[ext ([type integer?] [data bytes?])]{
  Represents a MessagePack @racket[ext] type, a pair of a @racket[type] integer
  and a @racket[data] byte string.
}


@section{Packing}
@defmodule[msgpack/pack #:no-declare]
@(declare-exporting msgpack msgpack/pack)

@defproc[(pack [datum any/c] [out (and/c output-port? (not/c port-closed?))]) any]{
  Pack @racket[datum] into the @racket[out] port. The type to pack
  @racket[datum] to will be determined automatically to use the least amount
  of space.
}


@section{Unpacking}
@defmodule[msgpack/unpack #:no-declare]
@(declare-exporting msgpack msgpack/unpack)

@defproc[(unpack [in (and/c input-port? (not/c port-closed?))]) any]{
  Unpack a datum from @racket[in]. At least one byte is consumed in the process
  to read the tag, more bytes are consumed as needed by the type of data.
}
