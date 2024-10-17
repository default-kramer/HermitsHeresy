#lang racket/gui

(require pict)

; == Thoughts ==
; By having a data model made up of primitive racket values,
; we should get undo/redo and save/load pretty much for free.
;
;
; Instead of having an "add new" button, have a function that will
; insert placeholders as necessary. So for example
#;(cond
    (cond-item (in-hill? "hill.bmp")
               (do something)))
; Would get massaged into this:
#;(cond
    (cond-item (in-hill? "hill.bmp")
               (do something))
    (cond-item #f
               (do nothing)))
; Which would allow the user to then select the <false> node and
; change it to something else.
;
;
; The treeview still needs to expose a "selected node changed" event
; which will probably something like (list 2 1 3) indicating
; second root item -> first child -> third child.
; Then we just need to use that to figure out stuff like:
; * can move up
; * can move down
; * can delete
; * current node type
; * available node types
; * selected node detail editor state


(define frame (new frame%
                   [alignment '(left top)]
                   [label "Hermit's Heresy Prototype GUI"]
                   [width 550]
                   [height 650]))

(define treenode%
  (class radio-box%
    (init-field text)
    (super-new [label ""]
               [stretchable-width #f]
               ;[alignment '(left top)]
               [choices (list text)])
    (send this set-selection #f)
    ))

(define treeview%
  (class vertical-panel%
    (super-new)
    (init-field root-nodes
                item->text
                item->children)
    (define (checkbox-clicked child event)
      (when (send child get-selection)
        (for ([treenode (send this get-children)])
          (when (not (eq? treenode child))
            (send treenode set-selection #f)))))
    (define (handle item [indent 0])
      (new treenode%
           [text (item->text item)]
           [callback checkbox-clicked]
           [parent this]
           [horiz-margin (* 24 indent)])
      (for ([child (item->children item)])
        (handle child (+ 1 indent))))
    (map handle root-nodes)))

(define tree-content
  '(cond
     (cond-item (in-hill? "hill.bmp")
                (set-block! 'Chert))
     (cond-item (block-matches? 'Snow)
                (set-block! 'Ice))))

(define (get-text node)
  (match node
    [(list 'cond stuff ...)
     "Decision:"]
    [(list 'cond-item test-expr body ...)
     (format "Else If: ~v" test-expr)]
    [else
     (format "~v" node)]))

(define (get-children node)
  (match node
    [(list 'cond items ...)
     items]
    [(list 'cond-item test-expr body ...)
     body]
    [else (list)]))

(define splitter (new horizontal-panel%
                      [parent frame]))

(define left-side (new vertical-panel%
                       [parent splitter]))

(define top-editor (new horizontal-panel%
                        [alignment '(left top)]
                        ;[max-height 40]
                        [stretchable-height #f]
                        [parent left-side #;node-editor]))

(define tv (new treeview%
                [stretchable-width #t]
                [alignment '(left top)]
                [root-nodes (list tree-content)]
                [item->text get-text]
                [item->children get-children]
                [parent left-side]))

(define node-editor (new vertical-panel%
                         [alignment '(left top)]
                         ;[stretchable-height #f]
                         [parent splitter]))

(define move-up-button
  (new button%
       [label (pict->bitmap (arrow 20 (/ pi 2)))]
       [parent top-editor]))

(define move-down-button
  (new button%
       [label (pict->bitmap (arrow 20 (/ pi -2)))]
       [parent top-editor]))
(send move-down-button enable #f)

(define delete-button
  (new button%
       [label "Delete"]
       [parent top-editor]))

(define node-kind-chooser
  (new list-box%
       [label ""]
       [choices (list "<false>" "block-matches?" "in-area?" "in-hill?")]
       [parent node-editor]))

(define node-detail-editor
  (new group-box-panel%
       [label "Node Options"]
       [parent node-editor]))

(define hill-chooser
  (new choice%
       [label "Bitmap:  "]
       [choices (list "hill.bmp")]
       [parent node-detail-editor]))

#;(define hill-adjust-y
    (new slider%
         [label "Adjust Y:  "]
         [min-value -96]
         [max-value 96]
         [init-value 0]
         [parent node-detail-editor]))

(define hill-adjust-y
  (new text-field%
       [label "Adjust Y:  "]
       [init-value "0"]
       [parent node-detail-editor]))

{module+ main ; Don't cause `raco test` to hang
  (send frame show #t)
  }
