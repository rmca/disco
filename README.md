[![Build Status](https://travis-ci.org/disco-lang/disco.svg?branch=master)](https://travis-ci.org/disco-lang/disco)

Prototype implementation of a small functional teaching language
for use in a discrete mathematics course.

Design principles:

* Includes those features, and *only* those features, useful in the
  context of a discrete math course. This is *not* intended to be a
  general-purpose language.
* Syntax is as close to standard *mathematical* practice as possible,
  to make it easier for mathematicians to pick up, and to reduce as
  much as possible the incongruity between the language and the
  mathematics being explored and modeled.
* Tooling, error messages, etc. are very important---the language
  needs to be accessible to undergrads with no prior programming
  experience. (However, this principle is, as of yet, only
  that---there is no tooling or nice error messages to speak of.)

Feel free to look around, ask questions, etc.  You can even contribute
some code if you like---collaborators are most welcome.  However, note
that no guarantees are made about anything in particular at the
moment.

Documentation
-------------

Documentation (such as it is) is [hosted on
readthedocs.io](http://disco-lang.readthedocs.io/en/latest/).

Building
--------

First, make sure you have
[the `stack` tool](https://docs.haskellstack.org/en/stable/README/).
Then at a command prompt, execute

```
stack build
```

After this completes, you should be able to

```
stack exec disco
```

to run the Disco command-line REPL.

While developing, you may want to use a command like

```
stack build --fast --file-watch --ghc-options='-Wall'
```

which will turn on warnings, turn off optimizations for a faster
edit-compile-test cycle, and automatically recompile every time a
source file changes.
