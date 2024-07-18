#lang scribble/manual

@(require (for-label hermits-heresy
                     racket)
          scribble/example)

@title{Hermit's Heresy}
Unauthorized utilities for scheming up scenes.
Move mountains, carve canyons, or summon superstructures!

(Power Tools for Dragon Quest Builders 2)

@section{Disclaimers}
TODO explain:
Use at your own risk.
Not authorized by Square Enix or Valve (Steam).
How to avoid catastrophic data loss.

@section{Avoiding Catastrophic Data Loss}
The Steam version of Dragon Quest Builders 2 saves data in a "SD" directory.
By default, this directory will be something like this:
@para{@tt{C:\Users\kramer\Documents\My Games\DRAGON QUEST BUILDERS II\Steam\76561198073553084\SD}}

If you are using all 3 save slots, you will see 3 subdirectories:

TODO
Recommend backing up the entire dir.
At the very least, CMNDAT and STGDAT belong together.
Recommend multiple immutable archive locations.


@section{Getting Started}
@subsection{Installation}
In its current form, Hermit's Heresy requires you to understand a little bit
about programming using Racket.
If you've never used Racket before, you should work through the
@hyperlink["https://docs.racket-lang.org/quick/index.html"]{Quick Introduction to Racket}
up to at least section 4 (Definitions).

Once you've installed Racket, you still need to install Hermit's Heresy.
If you are comfortable using command prompt, just type
@(racketblock raco pkg install hermits-heresy)
Or you can install Hermit's Heresy from DrRacket.
Navigate to @tt{File -> Package Manager -> Available from Catalog}.
Wait for it to refresh, then use the Filter to find the package named @bold{hermits-heresy}.
Select it and click Install.

Once you have everything installed, the following program
(which does basically nothing) should run without errors:
@(examples
  #:lang racket
  (require hermits-heresy)
  (block 'Chert))

@subsection{Configuring Writable Slots}
Hermit's Heresy protects you from accidentally overwriting your hard work.
It will never write to a save slot unless you have configured that directory as writable.
Personally, I use Save Slot 1 (B00) as my ephemeral save slot, and I always
assume that I might intentionally or accidentally delete or overwrite it at any time.
Any real, long-term work I am doing belongs in slots 2 or 3.

To configure a save slot to be writable, first ensure that it does not contain anything
you don't mind losing. Then create a file named
@tt{hermits-heresy.config.json} in that directory with the following content:
@verbatim{
 {"writable": true}
}

TODO Wait! The filename should be something like hermits-heresy.config.B00.json
to avoid the problem where you accidentally copy the B01 config file into B00.

@section{Reference}
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
