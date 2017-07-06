#lang scribble/manual
@(require
  (for-label
    racket
    msgpack
    msgpack/pack
    msgpack/unpack))

@title{MessagePack}

@hyperlink["http://msgpack.org/"]{MessagePack} is a binary serialisation
format focused on speed and size. This library allows you to serialise
(@racket[pack]) and de-serialise (@racket[unpack]) Racket objects.

@table-of-contents[]

@section{Introduction}

When two processes want to excgange data they need to agree on a protocol for
serialising and de-serialising said data. MessagePack is a protocol designed
with speed and size in mind; this means that serialised data should be as small
as possible and the recipient should be able to de-serialise the data as
quickly as possible. The downside of this is that the protocol is not readable
for humans, but this is not a concern if the data is only meant to be exchanged
beetween processes.

The API of this implementation follows the naming conventions of MessagePack:
we call the serialising process @emph{packing} and the de-serialising process
@emph{unpacking}. Integers have different length and can be either signed
(@code{intN}) or unsigned (@code{uintN}), where @code{N} is the length of the
integer in bytes.

MessagePack objects are converted to Racket objects and vice-versa according
to these rules:

@tabular[
  #:style 'boxed
  #:column-properties '(left left)
  #:row-properties '(bottom-border ())
  (list (list @bold{MessagePack type}  @bold{Racket type})
        (list "nil"               @racket['()])
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
        (list "fixarray"          @racket[vector])
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
        (list "ext 32"            'cont)
        )
]


@section{MessagePack}
@defmodule[msgpack]

@defstruct*[ext ([type integer?] [data bytes?])]{
  Represents a MessagePack @racket[ext] type, a pair of a @racket[type] integer
  and a @racket[data] byte string.
}



@section{Unpacking}
@defmodule[msgpack/unpack]

@defproc[(unpack [in (and/c input-port? (not/c port-closed?))]) any]{
  Unpack a datum from @racket[in]. At least one byte is consumed in the process
  to read the tag, more bytes are consumed as needed by the type of data.
}


@section{Packing}
@defmodule[msgpack/pack]

@subsection{Type predicates}
The following predicates are supplied by the module:

@defproc[(msg-port? (port any/c)) boolean?]{
  An open binary output port.
}

@deftogether[(
  @defproc[(uint8?  [x any/c]) boolean?]
  @defproc[(uint16? [x any/c]) boolean?]
  @defproc[(uint32? [x any/c]) boolean?]
  @defproc[(uint64? [x any/c]) boolean?])
]{
  Unsigned exact integers up to a fixed size. @racket[x] can be considered
  unsigned of size @math{n} if its value is @math{0 ≤ @racket[x] < 2^n}. If an
  integer satisfies a predicate of one size it will also satisfy any predicate
  of larger size.
}

@deftogether[(
  @defproc[(int8?  [x any/c]) boolean?]
  @defproc[(int16? [x any/c]) boolean?]
  @defproc[(int32? [x any/c]) boolean?]
  @defproc[(int64? [x any/c]) boolean?])
]{
  Signed exact integers within a fixed size. For a size @code{n} the range of
  @racket[x] is @math{-2@superscript{n-1} ≤ @racket[x] < 2@superscript{n-1}}.
  If an integer satisfies a predicate of one size it will also satisfy any
  predicate of larger size.
}

@deftogether[(
  @defproc[(+fixint? (x any/c)) boolean?]
  @defproc[(-fixint? (x any/c)) boolean?])
]{
  A positive of negative @code{fixnum}. Postive ones are exact integers between
  @math{0} and @math{128}, negative ones are integers between @math{-1} and
  @math{31} (all inclusive).
}


@subsection{Packing procedures}

@defproc[(pack [datum any/c] [out msg-port?]) any]{
  Pack @racket[datum] into the @racket[out] port. The type to pack
  @racket[datum] to will be determined automatically to use the least amount
  of space; use one of the more specialised procedures if you wish for greater
  control.
}

@defproc[(pack-nil [out msg-port?]) any]{
  Pack a @racket['()] as a @code{nil} object.
}

@defproc[(pack-boolean [b boolean?] [out msg-port?]) any]{
  Pack a @racket[#t] or @racket[#f] as a boolean object.
}

@deftogether[(
  @defproc[(pack-uint8  [x uint8? ] [out msg-port?]) any]
  @defproc[(pack-uint16 [x uint16?] [out msg-port?]) any]
  @defproc[(pack-uint32 [x uint32?] [out msg-port?]) any]
  @defproc[(pack-uint64 [x uint64?] [out msg-port?]) any])
]{
  Pack an unsigned integer to a @code{uintN} of size @code{N}.
}

@deftogether[(
  @defproc[(pack-int8  [x int8? ] [out msg-port?]) any]
  @defproc[(pack-int16 [x int16?] [out msg-port?]) any]
  @defproc[(pack-int32 [x int32?] [out msg-port?]) any]
  @defproc[(pack-int64 [x int64?] [out msg-port?]) any])
]{
  Pack a signed integer to an @code{intN} of size @code{N}.
}

@deftogether[(
  @defproc[(pack-p-fixint [x +fixnum?] [out msg-port?]) any]
  @defproc[(pack-n-fixint [x -fixnum?] [out msg-port?]) any])
]{
  Pack an integer to a positive or negative @code{fixnum}.
}

@defproc[(pack-float [x +fixnum?] [out msg-port?]) any]{
  Pack a real number as a @code{float}. Whether the size is single or double
  will be determined automatically.
}

@deftogether[(
  @defproc[(pack-string [str string?] [out msg-port?]) any]
  @defproc[(pack-bin    [bstr bytes?] [out msg-port?]) any])
]{
  Pack a string or byte string as a @code{string} or a @code{bin}
  respectively.  Strings are UTF-8 encoded.
}

@defproc[(pack-array [v vector?] [out msg-port?]) any]{
  Pack a vector as an @code{array}.
}

@defproc[(pack-map [h hash?] [out msg-port?]) any]{
  Pack a hash table as a @code{map}.
}

@defproc[(pack-ext [e ext?] [out msg-port?]) any]{
  Pack an @racket[ext] as an @code{ext}.
}
