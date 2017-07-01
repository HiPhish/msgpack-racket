#######################
 MessagePack in Racket
#######################

.. default-role:: code


This is an implementation of MessagePack_ written in Racket_. Currently only
de-serialisation is implemented, I am still in the process of poking my way
through Racket. I still need to write serialisation, provide full testing, and
package is as a proper Racket library.

.. _MessagePack: http://msgpack.org/
.. _Racket: http://racket-lang.org/


Using
#####

.. code-block: racket

   (require msgpack/unpack)
   (define in (open-input-bytes (bytes #xCD #x23 #x45)))
   (define value (unpack in))
   ;; value is now #x2345, or 9029 in decimal

The `unpack` function takes a binary port and returns one de-serialised object,
consuming the necessary amount of bytes from the port in the process. In the
above example we created a port from a byte string, but the port may be any
kind of Racket port.


License
#######

Released under the GPL v3 license, see the COPYING_ file for details.

.. _COPYING: COPYING.txt
