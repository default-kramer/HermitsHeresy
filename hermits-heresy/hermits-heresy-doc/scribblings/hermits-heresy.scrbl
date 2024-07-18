#lang scribble/manual

@(require (for-label hermits-heresy
                     racket))

@title{Hermit's Heresy}
Unauthorized utilities for scheming up scenes. Move mountains, carve canyons, or summon superstructures!

(Power Tools for Dragon Quest Builders 2)

WORK IN PROGRESS.
For now this is placeholder documentation.
I want to verify that I can create tutorials and examples on an external site
but have them link to this reference documentation using Scribble.

@section{Disclaimers}
TODO explain:
Use at your own risk.
Not authorized by Square Enix or Valve (Steam).
How to avoid catastrophic data loss.

@section{WIP}
@(racketblock
  (define test (put-hill! a b c)))

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

Blah.

@(defmodule hermits-heresy)

@subsection{Loading and Saving}

@defparam[save-dir dir (or/c #f path-string?)]{
 A parameter that tells Hermit's Heresy where your "SD" directory is.
 Typically used at the start of a script like this:
 @(racketblock
   (save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/"))
}

@defproc[(load-stage [kind (or/c 'IoA)]
                     [slot (or/c 'B00 'B01 'B02 path?)])
         stage?]{
 Loads a STGDAT file.

 For now, the only supported @(racket kind) is @(racket 'IoA) indicating "Isle of Awakening"
 / "Home Island" / STGDAT01.BIN.
 Support for other kinds of islands will be added in the future.

 The meaning of @(racket slot) is as follows:
 @(itemlist
   @item{@(racket 'B00) -- Save Slot 1, relative to @(racket save-dir)}
   @item{@(racket 'B01) -- Save Slot 2, relative to @(racket save-dir)}
   @item{@(racket 'B02) -- Save Slot 3, relative to @(racket save-dir)}
   @item{A @(racket path?) -- The full path to any STGDAT file, ignores @(racket save-dir)}
   )
}

@defproc[(save-stage! [stage stage?])
         any/c]{
 Writes the given @(racket stage) to the STGDAT file it was originally loaded from.
 TODO link to explanation of how to mark a directory writable.
}

@defproc[(copy-all-save-files! [#:from from-slot (or/c 'B00 'B01 'B02)]
                               [#:to to-slot (or/c 'B00 'B01 'B02)])
         any/c]{
 Copies all DQB2 save files, leaving other files (such config files) untouched.
 The meaning of B00/B01/B02 is explained at @(racket load-stage).
}

@subsection{Block Manipulation}
@defproc[(put-hill! [stage stage?]
                    [hill hill?]
                    [block block?])
         any/c]{
 TODO testing if I can link here from an external site
}
