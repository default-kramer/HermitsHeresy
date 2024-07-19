#lang scribble/manual

@(require (for-label hermits-heresy
                     racket)
          scribble/example)

@title{Hermit's Heresy}
Unauthorized utilities for scheming up scenes.
Move mountains, carve canyons, or summon superstructures!

(Power Tools for Dragon Quest Builders 2)

@section{Disclaimers}
Not authorized by Square Enix or Valve (Steam).
Use at your own risk.

This tool and this documentation assumes that you are using Steam.
It might work with Switch also, but I have never tested this.

@section{Avoiding Catastrophic Data Loss}
The Steam version of Dragon Quest Builders 2 saves data in a "SD" directory.
By default, this directory will be something like this:
@para{@tt{C:\Users\kramer\Documents\My Games\DRAGON QUEST BUILDERS II\Steam\76561198073553084\SD}}

If you are using all 3 save slots, you will see 3 subdirectories:
@(itemlist
  @item{@(racket 'B00) -- Save Slot 1}
  @item{@(racket 'B01) -- Save Slot 2}
  @item{@(racket 'B02) -- Save Slot 3}
  )

@bold{It is your responsibility to understand how to back up and restore this data.
 It is your responsibility to make frequent backups of any important work.}

Copying the entire directory is the safest way to create a backup.
You can prove this to yourself by doing something like this:
@(itemlist
  @item{Using Windows Explorer, copy one of the directories (e.g. B00) to create a backup.}
  @item{Start DQB2, load that slot, place a single test block, and save your game.}
  @item{Exit DQB2.}
  @item{Confirm that the Modified Date has changed on one of your STGDAT files.}
  @item{Using Windows Explorer, restore the files from the backup you just created.}
  @item{Start DQB2, load that slot, and confirm that the test block is gone.})

Either Google Drive or OneDrive (or maybe both?) has reported issues
where an extremely small percentage of files were corrupted or lost.
For this reason, I copy my backups to 2 different cloud storage providers.

@subsection{Beware Steam Autocloud}
One time I accidentally copied a STGDAT file into the same directory,
resulting in a file named @tt{STGDAT01 - Copy.BIN}.
Because of the way Steam Autocloud is configured, I am now unable to remove this file.
I can delete it, but Steam will restore it whenever I run DQB2.
For now, this is a minor annoyance.
But you can imagine if you did this with a large amount of data, it might
raise some red flags on your Steam account.

@section{Getting Started}
@subsection{Installation}
In its current form, Hermit's Heresy requires you to understand a little bit
about programming using Racket.
If you've never used Racket before, you should work through the
@hyperlink["https://docs.racket-lang.org/quick/index.html"]{Quick Introduction to Racket}
up to at least section 4 (Definitions).
Don't worry if you don't understand everything; if you are following
a tutorial or an example you won't need deep programming knowledge.
You just need to be familiar with how DrRacket works.

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
you don't mind losing.
Let's assume you follow my convention and use B00 as your ephemeral slot.
Create a file named @tt{hermits-heresy.config.B00.json} in your B00 directory with the following content:
@verbatim{
 {"writable": true}
}

In general, the file must be named @tt{hermits-heresy.config.<<DIR>>.json}.
Including the directory name in this filename protects against mistakes.
For example, if you accidentally copy your B00 config file into the B01 directory,
you will not accidentally make B01 writable because the B00 config file will be ignored.

@subsection{Tutorials and Examples}
TODO add links here.

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
 TODO
}
