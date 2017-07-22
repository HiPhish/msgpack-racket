#######################
 MessagePack in Racket
#######################

.. default-role:: code


This is an implementation of MessagePack_ written in Racket_.  Currently only
de-serialisation is implemented, I am still in the process of poking my way
through Racket, see below for caveats.

.. _MessagePack: http://msgpack.org/
.. _Racket: http://racket-lang.org/


Using
#####

.. code:: racket

   ;;; Packing data
   (require msgpack/pack)
   (define out (open-output-bytes))
   (pack #x1234 out)  ;; (get-output-bytes out) returns #xCD #x12 #x34

   ;;; Unpacking data
   (require msgpack/unpack)
   (define in (open-input-bytes (bytes #xCD #x12 #x34)))
   (unpack in)  ;; returns #x1234, or 4660 in decimal

The `pack` function takes a Racket object and a binary output port as arguments
and writes the serialised data to the port.  The `unpack` function takes a
binary input port and returns one de-serialised object, consuming the necessary
amount of bytes from the port in the process.

In the above example code we created a port from a byte string, but the port
may be any kind of Racket port.


Caveats
#######

The library is still early in development, technically the packing and
unpacking routines have been written, but they are largely untested yet.

The following cases cannot be tested for the time being:

- The `bin32` type, storing a byte string that is :math:`2^32` bytes long
  requires 4GiB, my machine simply runs out of memory.
- The same goes for the `str32` type
- Strings are only tested using ASCII characters, if anyone can generate
  UTF-8 strings with a given length in *bytes* please help out.


License
#######

Released under the GPL v3 license, see the COPYING_ file for details.

.. _COPYING: COPYING.txt
