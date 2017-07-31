.. default-role:: code

######################################
 TODO List for MessagePack for Racket
######################################


- Provide examples in the documentation
- Provide benchmark measurements comparing to `serialize`/`deserialize` or
  `jsexpr->bytes`


Porting to Typed Racket
#######################

- Investigate why when testing I need to require `msgpac` instead of `(file
  "../../main.rkt")` and similar.
- Investigate why the `ext` structure needs to be `#:transparent`.
- See if the exception raising in `unpack` is good practice
- Update the manual appropriately.

