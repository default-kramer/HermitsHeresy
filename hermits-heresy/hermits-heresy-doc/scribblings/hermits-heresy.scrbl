#lang scribble/manual

@(require (for-label hermits-heresy
                     racket)
          scribble/example)

@(begin
   (define (make-fresh-eval)
     (let ([e (make-base-eval)])
       (e '(require hermits-heresy))
       e))
   (define default-eval (make-fresh-eval))
   )

@title{Hermit's Heresy: DQB2 Power Tools}
Unauthorized utilities for scheming up scenes.
Move mountains, carve canyons, or summon superstructures!

(Power Tools for Dragon Quest Builders 2)

@section{Disclaimers}
Not authorized by Square Enix or Valve (Steam).
Use at your own risk.

Be sure to inspect your results after modifying a stage.
In particular, make sure you can sail away from and return to
the island you just modified.

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
  @item{Start and then exit DQB2. Leave Steam running.}
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
@subsubsub*section{It might revert your changes}
If you modify a save file while Steam is not running, it is likely that when you
do start Steam it will overwrite your change, reverting to your previous cloud save.
For this reason, you should start Steam and DQB2 before using Hermit's Heresy.
I recommend exiting DQB2 whenever you run a Hermit's Heresy script,
but you must leave Steam running.
This seems to work; Steam will accept the save file modifications
as if they had come from DQB2.

@subsubsub*section{It might make files undeletable}
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

@(define make-slot-writable-tag "make-slot-writable")
@subsection[#:tag make-slot-writable-tag]{Configuring Writable Slots}
An @italic{ongoing project} is one in which you maintain and evolve your Hermit's Heresy
project (scripts and images) alongside the save file that you play conventionally,
possibly for months or even years.
You can read more about this workflow @hyperlink["https://dqb-forever.com/stb/perimeter.html"]{here}.

A simpler workflow is a @italic{one-off script}, in which you just run the script once
and then play conventionally going forward, allowing you to discard the script as soon as you
are satisfied with the results.

@subsubsection[#:tag "ongoing-project"]{For Ongoing Projects}
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

@subsubsection{For One-Off Scripts}
In this workflow, you create an "artificial save slot" and mark it writable.
You will have to
@(itemlist
  #:style 'ordered
  @item{manually copy your STGDAT file into this directory,}
  @item{pass its full path into @(racket load-stage), and}
  @item{copy the updated STGDAT file back to a real slot when you are done.})

For example, you could create a directory named @tt{oneoff} with a corresponding
config file @tt{hermits-heresy.config.oneoff.json}.
The config file behaves the same way as in @secref["ongoing-project"].

@subsection{Tutorials and Examples}
Congratulations on making it this far!
Now go to @hyperlink["https://dqb-forever.com/hermits-heresy/"]{DQB Forever}
for tutorials/examples that you can copy and modify to suit your needs.

@section{Acknowledgments}
Thanks to the team who made DQB2 so excellent.

Thanks to all contributors who made Racket so excellent.

Thanks to turtle-insect, who paved the way in save-file-editing.

Thanks to Aura and Sapphire645 for contributions to Hermit's Heresy.

@section{Reference}
@(defmodule hermits-heresy)
@subsection{Loading and Saving}

@defparam[save-dir dir (or/c #f path-string?)]{
 A parameter that tells Hermit's Heresy where your "SD" directory is.
 Typically used at the start of a script like this:
 @(racketblock
   (save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/"))
}

@defproc[(load-stage [kind (or/c 'IoA
                                 'Furrowfield
                                 'Khrumbul-Dun
                                 'Moonbrooke
                                 'Malhalla
                                 'Anglers-Isle
                                 'Skelkatraz
                                 'BT1
                                 'BT2
                                 'BT3)]
                     [slot (or/c 'B00 'B01 'B02 path-string?)])
         stage?]{
 Loads a STGDAT file.

 The kinds @(racket 'BT1 'BT2 'BT3) refer to Buildertopias 1, 2, and 3.

 The meaning of @(racket slot) is as follows:
 @(itemlist
   @item{@(racket 'B00) -- Save Slot 1, relative to @(racket save-dir)}
   @item{@(racket 'B01) -- Save Slot 2, relative to @(racket save-dir)}
   @item{@(racket 'B02) -- Save Slot 3, relative to @(racket save-dir)}
   @item{A @(racket path-string?) -- The full path to any STGDAT file, ignores @(racket save-dir)}
   )
}

@defproc[(save-stage! [stage stage?])
         any/c]{
 Writes the given @(racket stage) to the STGDAT file it was originally loaded from.

 Will produce an error if the destination directory has not been @seclink[make-slot-writable-tag]{made writable}.
}

@defproc[(copy-all-save-files! [#:from from-slot (or/c 'B00 'B01 'B02)]
                               [#:to to-slot (or/c 'B00 'B01 'B02)])
         any/c]{
 Copies all DQB2 save files, leaving other files (such config files) untouched.
 The meaning of B00/B01/B02 is explained at @(racket load-stage).

 Will produce an error if the destination directory has not been @seclink[make-slot-writable-tag]{made writable}.
}

@subsection{Samplers}
An @deftech{XZ} coordinate consists of two values.
When referring to a DQB2 stage, larger values of X indicate "more east"
and larger values of Z indicate "more south".
When working with images, larger values of X indicate "more right"
and larger values of Z indicate "more down".
This correspondence allows you to use images to define edits to a DQB2 stage.

To fully specify a point in 3D space, you need an @tech{XZ} plus a Y coordinate.
The Y coordinate specifies elevation; larger values are higher up.
All stages in DQB2 have a max elevation of 96 blocks.

@defproc[(sampler? [x any/c])
         any/c]{
 A sampler is a basic building block for many terraforming techniques.

 A sampler is basically a function that accepts an @tech{XZ} coordinate and
 returns either an integer or @(racket #f).
 The meaning of the integer depends on how it is being used; for example,
 @(racket make-hill) uses the result of the sampler to define the hill's
 elevation at that point.
 When a sampler returns @(racket #f), it always means "the given XZ was
 not contained inside this sampler."

 The set of all @tech{XZ}s for which a sampler returns non-false is
 the @deftech{domain} of the sampler.
}

@defproc[(combine-samplers [sampler sampler?]
                           [combiner sampler-combiner?] ...)
         sampler?]{
 Starts with the given @(racket sampler) and applies each @(racket combiner)
 in order to produce a new sampler.

 A combiner is a @(racket sampler?) with some extra information
 on how it should be combined with the incoming sampler.
 The three kinds of combiners are @(racket function), @(racket intersection), and @(racket union).
}

@defproc[(function [operation (or/c '+ '-)]
                   [sampler sampler?]
                   [#:fallback fallback (or/c #f fixnum?) #f])
         sampler-combiner?]{
 To be used with @(racket combine-samplers).

 The combined sampler will add or subtract the given @(racket sampler)'s
 value from the incoming value.
 The @tech{domain} of the combined sampler will exactly equal the domain
 of the incoming sampler.
 For any @tech{XZ}s at which the given @(racket sampler) is undefined,
 the @(racket fallback) will be used.
 A @(racket fallback) of @(racket #f) does not mean "produce false";
 it means "produce the incoming value unchanged."
}

@defproc[(intersection [operation (or/c '+ '-)]
                       [sampler sampler?])
         sampler-combiner?]{
 To be used with @(racket combine-samplers).

 The combined sampler will add or subtract the given @(racket sampler)'s
 value from the incoming value.
 The @tech{domain} of the combined sampler will be the intersection of the
 incoming domain and the given @(racket sampler)'s domain.
 That is, for any @tech{XZ}s at which the given @(racket sampler) is undefined,
 the combined sampler is also undefined.
}

@defproc[(union [operation (or/c '+ '-)]
                [sampler sampler?])
         sampler-combiner?]{
 To be used with @(racket combine-samplers).

 The combined sampler will add or subtract the given @(racket sampler)'s
 value from the incoming value.
 The @tech{domain} of the combined sampler will be the union of the
 incoming domain and the given @(racket sampler)'s domain.
 Whenever one of the samplers (the incoming sampler or the given @(racket sampler))
 is undefined, the value from the other sampler will be produced.
}

@defproc[(bitmap-sampler [filename path-string?]
                         [#:rgb grayscale grayscale-spec?]
                         [#:invert? invert? any/c #f]
                         [#:normalize normalize normalize-spec? #f]
                         [#:project project project-spec?])
         sampler?]{
 Creates a @(racket sampler?) from the bitmap specified by @(racket filename).
 For every pixel that is not fully transparent, these 4 transformations will
 be applied @bold{in the following order} to define the value
 that will be returned by this sampler.
 @(itemlist
   @item{@(racket grayscale) -- Applies the given @(racket grayscale-spec?).}
   @item{@(racket invert?) -- When true, the grayscale value will be inverted.
  The resulting value will remain in the inclusive range @(racket [0 .. 255]).}
   @item{@(racket normalize) -- Applies the given @(racket normalize-spec?).}
   @item{@(racket project) -- Applies the given @(racket project-spec?).})
}

@defproc[(grayscale-spec? [spec any/c]) any/c]{
 Step 1 of the @(racket bitmap-sampler) pipeline.

 A grayscale spec describes how to produce a single grayscale value from an RGB pixel.
 The resulting value will always be in the inclusive range @(racket [0 .. 255]).
 @(itemlist
   @item{@(racket 'r) -- Selects the pixel's red component.}
   @item{@(racket 'g) -- Selects the pixel's green component.}
   @item{@(racket 'b) -- Selects the pixel's blue component.}
   @item{@(racket 'max) -- Selects whichever of R,G,B has the maximum (lightest) value.}
   @item{@(racket 'min) -- Selects whichever of R,G,B has the minimum (darkest) value.})
}

@defproc[(normalize-spec? [spec any/c]) any/c]{
 Step 3 of the @(racket bitmap-sampler) pipeline.

 A normalize spec describes how to compress or remap a range of grayscale values.
 The input range is assumed to be the inclusive @(racket [0 .. 255]).
 @(itemlist
   @item{@(racket #f) -- No normalization. The output range remains defined
  as the inclusive @(racket [0 .. 255]) regardless of what values actually exist.}
   @item{@(racket '[0 .. N-1]) -- Scans the entire input (such as a bitmap)
  to find the @(racket N) distinct grayscale values that actually occur.
  Remaps those values such that the output range is the inclusive @(racket '[0 .. N-1]).
  For example, if the input has 3 distinct grayscale values @(racket '(22 100 144))
  the output mapping will be equivalent to
  @(racketblock
    (case input
      [(22) 0]
      [(100) 1]
      [(144) 2]
      [else (error "impossible")]))
  This is useful so that you can create bitmaps with human-friendly contrast
  and remap them to a more useful and predictable range.})
}

@defproc[(project-spec? [spec any/c]) any/c]{
 Step 4 of the @(racket bitmap-sampler) pipeline.

 A project spec defines an arbitrary projection.
 @margin-note{Here I am using examples rather than precise Racket contracts.}
 @(itemlist
   @item{@(racket #f) -- The identity function; output matches input.}
   @item{@(racket '([darkest 55] [step 2])) -- Declares an anchor:
  the darkest (minimum) value of the input range will be projected to 55.
  For each step taken towards the lightest (maximum) value of the input range,
  the projected value will increase by 2 (the "step").
  This example might be used to build a hill starting at elevation 55 which
  increases by 2 steps at a time.}
   @item{@(racket '([lightest 31] [step -1])) -- Declares an anchor:
  the lightest (maximum) value of the input range will be projected to 31.
  For each step taken towards the darkest (minimum) value of the input range,
  the projected value will decrease by 1 (the "step").
  This example might be used to build a seafloor at elevation 31 which decreases
  by 1 step at a time.}
   @item{@(racket '[43 44 45 46]) -- Asserts that the input range will have exactly 4
  values and projects them to the values of the given list of integers.
  The darkest (minimum) value projects to the first element of the list
  and the lightest (maximum) value projects to the last element of the list.})

 To be technically precise, @(racket 'lightest) and @(racket 'darkest) must
 specify a @(racket fixnum?) as the anchor; while the @(racket 'step) must
 be an @(racket exact?) and @(racket rational?) number.
 For example @(racket '([lightest 40] [step -2/3])) would mean "The lightest value
 of the input range is projected to 40. For every 2 steps taken towards the
 darkest value of the input range, the projected value decreases by 3."

 It is important to remember that @(racket 'lightest) and @(racket 'darkest) refer
 to the endpoints of the inclusive input range, which can be much wider than the
 range of values that actually occur when (for example)
 a @(racket normalize-spec?) of @(racket #f) is used.
}

@subsection{Image Utilities}

@defproc[(get-template-image [id symbol?])
         pict?]{
 Returns a prebuilt template image.
 Probably only useful from the @italic{interactions area} of DrRacket.
 The current list of template images is as follows:
 @(racketblock
   'IoA-background
   'IoA-bedrock-mask
   'IoA-chunk-mask
   'Furrowfield-background
   'Furrowfield-bedrock-mask
   'Furrowfield-chunk-mask
   'Khrumbul-Dun-background
   'Khrumbul-Dun-bedrock-mask
   'Khrumbul-Dun-chunk-mask
   'Moonbrooke-background
   'Moonbrooke-bedrock-mask
   'Moonbrooke-chunk-mask
   'Malhalla-background
   'Malhalla-chunk-mask
   'Anglers-Isle-background
   'Anglers-Isle-bedrock-mask
   'Anglers-Isle-chunk-mask
   'Skelkatraz-background
   'Skelkatraz-bedrock-mask
   'Skelkatraz-chunk-mask)
 @(examples
   (require pict hermits-heresy)
   (scale (get-template-image 'IoA-background) 0.5))

 The masks can be used to guide you while you work in your image editor.
 The @(racket '*-bedrock-mask) images black out all pixels which lack "bedrock",
 the indestructible block that forms the floor of the island.
 While playing the game normally, it is impossible to place, destroy, or even trowel blocks
 that are outside of the bedrock area.
 But using Hermit's Heresy, it is possible to place blocks outside of the bedrock area.
 If you do this, you won't be able to manipulate those blocks while playing normally.

 If you want to avoid accidentally placing any blocks outside of the bedrock area,
 consider passing the bedrock-mask image into @(racket bitmap->area) and then passing
 the resulting area into @(racket protect-area!).

 The @(racket '*-chunk-mask) images black out all pixels which cannot hold any blocks
 under any circumstances, because there is no place in the save file that maps
 to those coordinates.
 Using these images with @(racket protect-area!) would be redundant.

 @(examples
   (require pict hermits-heresy)
   (scale (cc-superimpose (get-template-image 'IoA-background)
                          (get-template-image 'IoA-bedrock-mask))
          0.5))

 You might also consider using
 @hyperlink["https://github.com/Sapphire645/DQB2MinimapExporter"]{this Minimap Exporter
  by Sapphire645} instead of these prebuilt @(racket '*-background) images.
}

@defproc[(save-template-image [id symbol?])
         any/c]{
 Like @(racket get-template-image) except it saves the image to your
 filesystem in the same directory as the script you are running.
 This allows you to import the images into paint.net or a similar program
 and use them as a guide for drawing your own custom geographical features.
}

@defproc[(bitmap->hill [path path-string?]
                       [#:adjust-y adjust-y fixnum? 0]
                       [#:semitransparent-handling semitransparent-handling
                        (or/c 'adjust 'ignore) 'adjust])
         hill?]{
 Converts the given bitmap to a hill.
 Each pixel corresponds to a 1x1 column of blockspace.
 Transparent pixels (alpha = 0) are ignored.
 For all other pixels, the height of that column will be the max height (95) minus
 half of whichever of R,G,B has the largest value at that pixel.
 I recommend defining hills using grayscale (R=G=B) to avoid confusion.

 This means that darker colors are taller. For example
 @(itemlist
   @item{A totally black pixel (R=G=B=0) indicates max height.}
   @item{A pixel having R=G=B=16 indicates 16/2=8 blocks short of max height.}
   @item{A pixel having R=G=B=40 indicates 40/2=20 blocks short of max height.})
}

This version discards the remainder of the division by 2.
A future version may decide to respect it via the flat chisel.

The @(racket adjust-y) can be used to raise or lower the entire hill.
Positive values raise; negative values lower.

For the most accurate results, you should avoid semitransparent pixels in your hill.
(In other words, alpha should always be either 0 or 255, no in-between values.)
But in practice, it's very easy to accidentally introduce semitransparency.
For this reason, @(racket semitransparent-handling) defaults to @(racket 'adjust)
which reduces the elevation based on how transparent the pixel is.
If you really know what you are doing, you might want to disable this behavior by using @(racket 'ignore).

@defproc[(bitmap->area [path path-string?])
         area?]{
 Converts the given bitmap to an area.
 Each pixel corresponds to a 1x1 column of blockspace.
 Fully transparent pixels (alpha = 0) are considered outside of the area;
 all other pixels are considered inside the area.
}

@subsection{Block Manipulation}
An @deftech{item} is a special kind of block, such as a tree or a door or a fountain.
A good rule of thumb is that anything capable of facing north/east/south/west is an item.
By contrast, @deftech{simple} blocks like Sand or Chert cannot face a cardinal direction;
rotating them with your gloves has no effect.

Unfortunately, this version of Hermit's Heresy is not capable of manipulating items.
For example, @(racket put-hill!) cannot overwrite items, meaning you may end up with items
hidden inside your hill unless you manually destroy them first.
I hope to solve this problem soon.

Note to Self:
Assuming that someday I will learn how to safely overwrite items, what is the best design?
Probably a parameter that would apply to all block manipulation procs.
Values could be @(racket 'no 'yes 'yes-even-indestructible).

@defproc[(put-hill! [stage stage?]
                    [hill hill?]
                    [block block?])
         any/c]{
 Fills the space defined by the given @(racket hill) with the given @(racket block).
 @tech{Simple} blocks are overwritten, but @tech[#:key "item"]{items} are left intact.
}

@defproc[(block [id symbol?])
         block?]{
 @tech{Simple} blocks in DQB2 are represented as integers.
 Any procedure that wants a block probably also accepts an integer.
 But using this procedure will make your code more readable:
 @(examples #:eval default-eval (block 'Chunky-Chert))

 Sometimes there is more than one integer that represents the same block.
 This probably doesn't matter, but it will write an informational message anyway.
 If the block doesn't appear in-game the way you expected, perhaps one of these alternate
 integers will work. (If any alternate does behave differently, let me know.)
 @(examples #:eval default-eval (block 'Chert))
}

@defproc[(find-block-name [name string?])
         any/c]{
 For use from the @italic{interactions area} of DrRacket to help you find the symbolic name
 to be used with the @(racket block) procedure:
 @(examples #:eval default-eval (find-block-name "snow"))

 Might find what you want even if you have a typo:
 @(examples #:eval default-eval (find-block-name "posion"))

 If you still can't find what you want, you can check the complete list in the
 @hyperlink["https://github.com/default-kramer/HermitsHeresy/blob/main/hermits-heresy/hermits-heresy/blockdata/blockdef-raw.rkt"]{source code here}.
}

@defproc[(protect-area! [stage stage?] [area area?])
         area?]{
 Marks all coordinates inside the given @(racket area) as protected.
 The protected area will silently ignore all attempted future block manipulations (for example, by @(racket put-hill!)).

 This function is supposed to perform a union with any previously protected area,
 but right now it will throw a "not implemented" error if the previously protected area is not empty.
 Sorry about that, let me know if you need it.
 (Consider performing the union using your image editing software if possible.)

 This function returns the previously protected area intended for use with a future hypothetical
 function like @(racket revert-protected-area!).
}

@defproc[(build-mottler [arg (or/c symbol?
                                   fixnum?
                                   (list/c (or/c symbol? fixnum?)
                                           exact-positive-integer?))] ...)
         (-> fixnum?)]{
 Creates a procedure that will return a random block from the given
 weighted block list each time it is called.
 @(examples
   #:eval default-eval
   (define my-grassy-mottler
     (build-mottler '[Mossy-Earth 3] (code:comment "weight 3")
                    'Grassy-Earth (code:comment "weight 1 by default")
                    '[Earth 1] (code:comment "weight 1")
                    '[Stony-Soil 2]))
   (code:comment "c'mon RNG, please don't embarrass me here:")
   (for/list ([i (in-range 25)])
     (my-grassy-mottler)))
}

@defproc[(selection [stage stage?]
                    [area area?]
                    [transforms (listof (or/c (list/c 'translate fixnum? fixnum?)
                                              (list/c 'translate-to fixnum? fixnum?)
                                              'mirror-x
                                              'mirror-z
                                              (list/c 'rotate integer?)
                                              (list/c 'adjust-y fixnum?)))])
         selection?]{
 Creates a selection.
 The combination of the given @(racket stage) and @(racket area) defines the
 blockspace to select from.
 The @(racket transforms) do not cause different blocks to be selected; instead, they
 shift the original blockspace to appear as if it is in a different location.
 See also @(racket with-selection).

 The meaning of the transforms is as follows.
 Some of the transformations depend on the "bounding rectangle", which is the
 smallest possible rectangle which includes all XZ coordinates inside the selection.
 @(itemlist
   @item{@(racket (list 'translate dx dz)) -- Shifts the X and Z
  coordinates by the given dx and dz values.}
   @item{@(racket (list 'translate-to X Z)) -- Sets the X and Z coordinates
  of the northwest/top-left corner of the bounding rectangle.}
   @item{@(racket 'mirror-x) -- Reflects such that east and west are swapped.
  The bounding rectangle remains in the same place.}
   @item{@(racket 'mirror-z) -- Reflects such that north and south are swapped.
  The bounding rectangle remains in the same place.}
   @item{@(racket (list 'rotate N)) -- Rotates the selection by N degrees.
  N must be a multiple of 90.
  The post-rotation bounding rectangle will be located such that the XZ coordinate
  of the northwest/top-left corner matches the pre-rotation bounding rectangle.
  For example, if the rectangle goes from (10,10) to (100,20) a 90-degree
  rotation would produce a rectangle that goes from (10,10) to (20,100).}
   @item{@(racket (list 'adjust-y dy)) -- Shifts the Y coordinate up or down by the
  given dy value. Positive values raise the selection; negative values lower it.})
}

@defproc[(make-hill [sampler sampler?]
                    [combiner sampler-combiner?] ...)
         hill?]{
 Creates a hill from the given @(racket sampler) and @(racket combiner)s,
 which are combined the same way as in @(racket combine-samplers).

 The output of the combined sampler defines the elevation of the hill.

 The hill can be used with @(racket in-hill?) inside a traversal.

 DQB2 supports a max elevation of 96.
 Any samples greater than 96 will be equivalent to 96.
 Any samples of 0 or less are eagerly excluded from the hill.
}

@defproc[(generate-platform-layout [width positive-fixnum?]
                                   [depth positive-fixnum?])
         platform-layout?]{
 Randomly generates a rectangular platform layout.
 To be used with @(racket make-platform-hills).
}

@defproc[(make-platform-hills [layout (or/c platform-layout?
                                            area?
                                            path-string?)]
                              [#:x dx fixnum? 0]
                              [#:z dz fixnum? 0]
                              [#:wall-block wall-block (or/c symbol? fixnum?) 'Umber]
                              [#:wall-chisel wall-chisel (or/c 'none 'flat-lo 'flat-hi) 'flat-lo]
                              [#:platform-block platform-block (or/c symbol? fixnum?) 'Seeded-Mossy-Spoiled-Soil]
                              [#:peak-y peak-y fixnum? 40]
                              [#:tall-y tall-y fixnum? -4]
                              [#:short-y short-y fixnum? -2])
         platform-hills?]{
 Creates platform hills.
 To be used with @(racket in-platform-hills?!).

 The @(racket layout) must be one of:
 @(itemlist
   @item{The result of @(racket generate-platform-layout)}
   @item{An area, whose bounding rect will be used for the width and depth
  of @(racket generate-platform-layout). Its offset will also control @(racket dx)
  and @(racket dz); so you should not provide these arguments.}
   @item{A @(racket path-string?), which will be used with @(racket bitmap->area)
  and the resulting area will be used as described above.})

 For the other arguments:
 @(itemlist
   @item{@(racket dx) translates the hill east or west.}
   @item{@(racket dz) translates the hill south or north.}
   @item{@(racket wall-block) specifies which block to use for the walls.}
   @item{@(racket wall-chisel) specifies the chisel to use for tops of walls that border platforms.}
   @item{@(racket platform-block) specifies which block to use for the platforms.}
   @item{@(racket peak-y) specifies the Y coordinate that the top of the hill will occupy.}
   @item{@(racket tall-y) specifies the Y coordinate of the tall platforms.
  If negative, indicates the amount to subtract from @(racket peak-y).}
   @item{@(racket short-y) specifies the Y coordinate of the short platforms.
  If negative, indicates the amount to subtract from @(racket tall-y).}
   )
}

@subsection{Traversal}
A traversal is basically a callback that is invoked for each coordinate in the blockspace.
It is a generic building block from which more complex functionality can be built.
For example, here is how a traversal could be used to replace certain blocks with others:
@(racketblock
  (traverse stage
            (traversal
             (cond
               [(block-matches? 'Snow)
                (set-block! 'Mossy-Earth)]
               [(block-matches? 'Chalk 'Chunky-Chalk)
                (set-block! 'Stony-Soil)]
               [(block-matches? 'Marble)
                (set-block! 'Copper-Vein)]))))

@defproc[(traverse [stage stage?] [traversal traversal?])
         any/c]{
 Executes the given @(racket traversal) over the given @(racket stage).
}

@defform[(traversal expr ...)]{
 Produces a traversal to be executed by @(racket traverse).
 Inside a traversal, certain macros become available such as @(racket block-matches?) and @(racket set-block!).

 Conceptually, the code inside the traversal will be executed for every coordinate in the blockspace.
 But optimizations might make this not strictly true.
 For example, it is possible to prove that the following traversal only needs to
 be run on coordinates which contain snow, and internal indexing of the stage
 may allow the traversal to run faster by skipping portions of the stage known to lack snow.
 @(racketblock
   (traversal (when (block-matches? 'Snow)
                (set-block! 'Ice))))
}

@defform[(block-matches? block-expr ...)]{
 Can only be used inside a @(racket traversal).

 Returns true if the current block (ignoring its chisel status)
 matches any of the given @(racket block-expr)s.
 Each @(racket block-expr) should be either a literal symbol (such as @(racket 'Chert))
 or a literal number (such as @(racket 414)).
 But if you know what you are doing, it can also be any expression that produces
 a @(racket fixnum?).

 Using symbols is recommended to match all possible values that symbol could mean.
 For example, @(racket (block-matches? 'Earth)) will be true when the current block
 is 2 or 414 because both of those values are mapped to Earth, as the following
 log message proves:
 @(examples
   ; Need a fresh eval to ensure the disambiguation appears
   #:eval (make-fresh-eval) #:label #f
   (block 'Earth))
}

@defform[(set-block! block-expr)]{
 Can only be used inside a @(racket traversal).

 Sets the current block to the given value (chisel status is unchanged).
 When @(racket block-expr) is a literal symbol, it is equivalent to @(racket (block block-expr)).
 Otherwise @(racket block-expr) must produce a @(racket fixnum?).

 @tech{Simple} blocks are overwritten, but @tech[#:key "item"]{items} are left intact.
}

@defform[(with-selection [block-id selection-expr] body ...)]{
 Can only be used inside a @(racket traversal).

 Executes the @(racket body) when the given @(racket selection) produces
 a @tech{simple} block (including vacancy) at the current coordinate.
 Within the @(racket body), the @(racket block-id) will contain that block's value.

 In other words, the @(racket body) will not be executed if the current coordinate
 lies outside of the selection or if the selection produces an @tech{item}.
}

@defform[(in-hill? hill)]{
 Can only be used inside a @(racket traversal).

 Returns true if the current xzy coordinate is inside the given @(racket hill),
 false otherwise.
 Hills can be created using @(racket make-hill) or @(racket bitmap->hill).
}

@defform[(in-platform-hills?! platform-hills)]{
 Can only be used inside a @(racket traversal).

 If the current xzy coordinate is inside the given platform hills,
 performs the block manipulations specified by that hill and returns true.
 Otherwise, does nothing and returns false.

 Platform hills can be created using @(racket make-platform-hills).
}

@subsection{Miscellaneous}
@defform[(with-absolute-seed [seed] body ...)]{
 Uses the given @(racket seed) to seed a random generator deterministically.
 Evaluates the given @(racket body) using that random generator,
 and then restores the generator to its previous state.

 The given @(racket seed) must be an integer that @(racket random-seed) will accept.
}