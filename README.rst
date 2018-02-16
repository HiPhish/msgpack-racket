#######################
 MessagePack in Racket
#######################

.. default-role:: code


This is a Racket_ implementation of MessagePack_ , a binary data serialisation
format. It allows you to serialise (pack) and de-serialise (unpack) Racket
object to and from binary data.

.. _MessagePack: http://msgpack.org/
.. _Racket: http://racket-lang.org/


Installation
############

The easiest way to install this library is from the `Racket Package Catalog`_.
Run the following code from your shell:

.. code:: sh

   raco pkg install msgpack

If you wish to install the package from this repository use the included
makefile:

.. code:: sh

   make install   # Install the package
   make remove    # Uninstall the package

.. _Racket Package Catalog: https://pkgs.racket-lang.org/


Using MessagePack
#################

.. code:: racket

   ;;; Import the library first
   (require msgpack)

   ;;; Some object to pack
   (define hodgepodge (vector 1 2 (void) '#(3 #t) "foo"))

   ;;; Packing data
   (define packed (call-with-output-bytes (λ (out) (pack hodgepodge out))))
   ;;; > #"\225\1\2\300\222\3\303\243foo"

   ;;; Unpacking data
   (define unpacked (call-with-input-bytes packed (λ (in) (unpack in))))
   ;;; > '#(1 2 #<void> #(3 #t) "foo")

The `pack` function takes a Racket object and a binary output port as arguments
and writes the serialised data to the port.  The `unpack` function takes a
binary input port and returns one de-serialised object, consuming the necessary
amount of bytes from the port in the process. For more details please refer to
the documentation_.

In the above example code we set the output and input ports to be byte strings
so we could work with the packed and unpacked data directly inside the Racket
instance.

.. _documentation: https://docs.racket-lang.org/msgpack/index.html


Status
######

The library is fully functional, covered by test cases, and the API should be
reasonably mature, but I am not yet willing to completely rule out changes. See
also below for parts of the library that could not be tested at the moment due
to technical reasons.


Caveats
#######

The following cases cannot be tested for the time being:

- The `bin32` type, storing a byte string that is :math:`2^32` bytes long
  requires 4GiB, my machine simply runs out of memory.
- The same goes for the `str32` type
- The same goes for the `array32` type
- The same goes for the `map32` type
- The same goes for the `ext32` type
- Strings are only tested using ASCII characters, if anyone can generate
  UTF-8 strings with a given length in *bytes* please help out.


License
#######

Released under the GPLv3+ license, see the COPYING_ file for details.

.. _COPYING: COPYING.txt
