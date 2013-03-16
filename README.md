## Ixl Language Prototype [![Build Status](https://travis-ci.org/jayferd/ixl-prototype.png)](https://travis-ci.org/jayferd/ixl-prototype)

Ixl is a stream-oriented language that works especially well in a REPL.
It will be the scripting language of a new editor called IX.

### Example Programs

    # Indent the paragraph under the cursor by two spaces
    select -paragraph | indent 2

### Why a new language?

Lisp programs are `(built (on (the left)))` which can be annoying to
manipulate in a REPL. Ixl works like the shell does, by chaining modular
pieces through pipes. It will also work well in source files, and serve
as a configuration language for an upcoming editor called IX. Ixl is
VimL done right.
