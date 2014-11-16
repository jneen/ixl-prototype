## Ixl Language Prototype [![Build Status](https://travis-ci.org/jneen/ixl-prototype.png)](https://travis-ci.org/jneen/ixl-prototype)

Ixl is a general-purpose scripting language that works especially well
in a REPL.  It will be the scripting language of a new editor called IX.

### Example Programs

    # Indent the paragraph under the cursor by two spaces
    select -paragraph | indent 2

### Installation

1. Install haskell and cabal
  Mac: `$ brew install haskell-platform`
  Ubuntu: `$ sudo apt-get install haskell-platform`
1. `$ make && make test && make install`
1. `$ ixl` to run the repl

Ixl installs to `$HOME/.local` by default.  To install to a different location,
run `PREFIX=/path/to/install/location make install`.

Ixl currently only parses programs. When you type a program it will output a
representation of the parse tree.  The interpreter is yet to be built.

### Why a new language?

Lisp programs are `(built (on (the left)))` which can be annoying to manipulate
in a REPL. Ixl works like the shell does, by chaining modular pieces through
pipes. It will also work well in source files, and serve as a configuration
language for an upcoming editor called IX. Ixl is VimL done right.
