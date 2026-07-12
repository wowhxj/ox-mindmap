;;; org-mindmap.el --- Editable mindmap visualization in org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2026 krvkir

;; Author: krvkir <krvkir@gmail.com>
;; Version: 0.3
;; Keywords: org, tools, outlines
;; Package-Requires: ((emacs "29.1") (org "9.1"))
;; URL: https://github.com/krvkir/org-mindmap

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;; Provides an editable mindmap visualization system within org-mode buffers.
;; Implements core data structures, region detection, parsing,
;; rendering (top, centered, with optional compaction), alignment, structural editing,
;; layout switching, and configuration via custom variables and text properties.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-mindmap-parser)
(require 'org-mindmap-svg)

(defgroup org-mindmap nil
  "Editable mindmap visualization within `org-mode'."
  :group 'org)

(defcustom org-mindmap-default-spacing 1
  "Characters between nodes."
  :type 'integer
  :group 'org-mindmap)

(defcustom org-mindmap-default-layout 'centered
  "Default layout mode."
  :type '(choice (const top) (const centered))
  :group 'org-mindmap)

(defcustom org-mindmap-default-compacted nil
  "Default compaction mode for new mindmap blocks.
When non-nil, child nodes fill vacant vertical spaces to produce
a denser layout.  When nil, children are stacked sequentially."
  :type 'boolean
  :group 'org-mindmap)

(defcustom org-mindmap-default-max-width 'auto
  "Default maximal width for node text soft wrapping.
If nil, no wrapping is applied.
If a non-negative integer, constant max width is used (0 means
a newline after each word).
If \\='auto, calculate max-width based on tree depth and the
current window width."
  :type '(choice integer (const auto))
  :group 'org-mindmap)

(defcustom org-mindmap-min-width 2
  "Minimal width for joining short lines.
Lines of this length or shorter are joined with the previous
line.  For example, with a value of 2 or more, \"an apple\"
is kept as one line rather than being split into \"an\" and
\"apple\"."
  :type 'integer
  :group 'org-mindmap)

(defcustom org-mindmap-default-wrap-leaves 3
  "Default wrapping behavior for leaf nodes.
If nil, leaf nodes are never wrapped.
If t, leaf nodes are wrapped like any other node.
If a positive number (int or float), leaf node max width is
multiplied by this factor: values above 1.0 allow wider leaves,
values below 1.0 make them narrower.
For example, with :max-width 10 and :wrap-leaves 1.5, leaf nodes
have a soft max width of 15."
  :type '(choice boolean float)
  :group 'org-mindmap)

(defcustom org-mindmap-protect-connectors nil
  "Make connectors read-only."
  :type 'boolean
  :group 'org-mindmap)

(defcustom org-mindmap-confirm-delete t
  "Require confirmation for deletions if node has children."
  :type 'boolean
  :group 'org-mindmap)

(defface org-mindmap-face-connectors
  '((t :inherit fixed-pitch))
  "Face for connector characters."
  :group 'org-mindmap)

(defface org-mindmap-face-text
  '((t :inherit fixed-pitch))
  "Face for node text."
  :group 'org-mindmap)

;; ...subtree painting

(defcustom org-mindmap-default-paint-depth 0
  "Default depth from which to paint subtrees."
  :type 'integer
  :group 'org-mindmap)

(defun org-mindmap-color-palette-rgb ()
  "A simple hardcoded palette: red, green and blue."
  '("red" "green" "blue"))

(defun org-mindmap-color-palette-from-font-lock ()
  "Gather foreground colors from the usual entities painted by font lock.
In every decent theme they probably are discernable, colorful and in accordance
with the other theme colors."
  (mapcar
   #'face-foreground
   '(font-lock-keyword-face
     font-lock-function-name-face
     font-lock-string-face
     font-lock-type-face
     font-lock-constant-face
     font-lock-builtin-face
     font-lock-warning-face
     font-lock-variable-name-face
     font-lock-number-face)))

(defun org-mindmap-color-palette-from-rainbow-delimiters ()
  "Gather foreground colors from `rainbow-delimiters' package, if instelled."
  (if (featurep 'rainbow-delimiters)
      (mapcar
       #'face-foreground
       '(rainbow-delimiters-depth-1-face
         rainbow-delimiters-depth-2-face
         rainbow-delimiters-depth-3-face
         rainbow-delimiters-depth-4-face
         rainbow-delimiters-depth-5-face
         rainbow-delimiters-depth-6-face
         rainbow-delimiters-depth-7-face
         rainbow-delimiters-depth-8-face
         rainbow-delimiters-depth-9-face))
    (error "`rainbow-delimiters' not found!")))

(defcustom org-mindmap-color-palette-fn 'org-mindmap-color-palette-from-font-lock
  "Function returning a list of color strings for painting subtrees.
Predefined options:
- \\='org-mindmap-color-palette-from-font-lock: sample colors
  from font-lock faces in the current theme (functions, variables,
  errors, etc.), giving a good-looking result in most themes.
- \\='org-mindmap-color-palette-from-rainbow-delimiters: sample
  colors from rainbow-delimiters, if installed.
- \\='org-mindmap-color-palette-rgb: just red, green, and blue
  (may look ugly in some themes).
Any function returning a list of strings is accepted: either color
names like \"black\" or \"grey12\", or RGB codes like \"#ff0000\"."
  :type 'function
  :group 'org-mindmap)

;; Set a dummy value so that benchmarks that test individual functions
;; won't crash.
(defvar org-mindmap-color-palette (org-mindmap-color-palette-rgb)
  "List of colors to paint subtrees in.")

(defcustom org-mindmap-paint-tinge-fg 0.8
  "Ratio of colorization for face foreground color."
  :type 'float
  :group 'org-mindmap)

(defcustom org-mindmap-paint-tinge-bg 0.2
  "Ratio of colorization for face background color."
  :type 'float
  :group 'org-mindmap)

(defun org-mindmap--color-blend (a b &optional alpha)
  "Blend the two colors A and B in linear space with ALPHA.
A and B should be lists (RED GREEN BLUE), where each element is
between 0.0 and 1.0, inclusive.  ALPHA controls the influence A
has on the result and should be between 0.0 and 1.0, inclusive.

For instance:

   (org-mindmap--color-blend \\='(1 0.5 1) \\='(0 0 0) 0.75)
      => (0.75 0.375 0.75)

(A backport of the function from emacs v31.)"
  (setq alpha (or alpha 0.5))
  (if (and a b)
      (let (blend)
        (dotimes (i 3)
          (push (+ (* (nth i a) alpha) (* (nth i b) (- 1 alpha))) blend))
        (nreverse blend))
    (or a b)))

(defun org-mindmap--color-rgb-to-hex  (red green blue &optional digits-per-component)
  "Return hexadecimal #RGB notation for the color specified by RED GREEN BLUE.
RED, GREEN, and BLUE should be numbers between 0.0 and 1.0, inclusive.
Optional argument DIGITS-PER-COMPONENT can be either 4 (the default)
or 2; use the latter if you need a 24-bit specification of a color.

(A backport of the function from emacs v31.)"
  (or digits-per-component (setq digits-per-component 4))
  (let* ((maxval (if (= digits-per-component 2) 255 65535))
         (fmt (if (= digits-per-component 2) "#%02x%02x%02x" "#%04x%04x%04x")))
    (format fmt (* red maxval) (* green maxval) (* blue maxval))))

(defun org-mindmap--tinge-fg (face color)
  "Tinge FACE's background color with COLOR."
  (apply #'org-mindmap--color-rgb-to-hex
         (org-mindmap--color-blend
          (color-name-to-rgb (or color (face-foreground 'default)))
          (color-name-to-rgb (face-foreground face nil 'default))
          org-mindmap-paint-tinge-fg)))

(defun org-mindmap--tinge-bg (face color)
  "Tinge FACE's background color with COLOR."
  (apply #'org-mindmap--color-rgb-to-hex
         (org-mindmap--color-blend
          (color-name-to-rgb (or color (face-background 'default) ))
          (color-name-to-rgb (face-background face nil 'default))
          org-mindmap-paint-tinge-bg)))

(defun org-mindmap-assign-color-by-num (_node num)
  "Assign a color to a NODE in a tree by its NUM in the tree."
  (nth (mod num (length org-mindmap-color-palette))
       org-mindmap-color-palette))

(defun org-mindmap-assign-color-by-text (node _num)
  "Assign a color to a NODE by its text, regardless its NUM in the tree."
  (let ((i (string-to-number (substring (secure-hash 'md5 (org-mindmap-parser-node-text node)) 0 8) 16)))
    (nth (mod i (length org-mindmap-color-palette))
         org-mindmap-color-palette)))

(defcustom org-mindmap-color-assign-fn 'org-mindmap-assign-color-by-text
  "Function returning a color string for a node by properties and index.
Predefined options:
- \\='org-mindmap-assign-color-by-num: assigns color based on the
  node\\='s position in the tree; moving the node changes its color.
- \\='org-mindmap-assign-color-by-text: assigns color based on the
  node\\='s text content; the color stays constant when the node
  is moved.
Note that neither option can make colors truly stick to nodes,
since no persistent metadata is stored."
  :type 'function
  :group 'org-mindmap)

;; helper fns

(defun org-mindmap--propertize-connector (str &optional color)
  "Apply face and optional COLOR to connector STR.
Ensures properties are not sticky to allow editing text at the boundary."
  (let* ((face (if color
                   (list (list :foreground (org-mindmap--tinge-fg 'org-mindmap-face-connectors color))
                         'org-mindmap-face-connectors)
                 'org-mindmap-face-connectors))
         (props (list 'face face
                      'font-lock-face face
                      'rear-nonsticky '(read-only face font-lock-face)
                      'front-sticky '(read-only face font-lock-face)
                      'line-height t)))
    (when org-mindmap-protect-connectors
      (setq props (plist-put props 'read-only t)))
    (apply #'propertize str props)))

(defun org-mindmap--propertize-text (str &optional color)
  "Apply text face and optional COLOR to STR."
  (let ((face (if color
                  (list (list :background (org-mindmap--tinge-bg 'org-mindmap-face-text color))
                        'org-mindmap-face-text)
                'org-mindmap-face-text)))
    (propertize str
                'face face
                'font-lock-face face
                'line-height t)))

(defun org-mindmap--string-pad-width (string length &optional padding start)
  "Pad STRING to visual column LENGTH using PADDING (character).
If START is non-nil, pad at the beginning; otherwise at the end."
  (let* ((w (string-width string))
         (diff (- length w))
         (pad-char (if padding (char-to-string padding) " ")))
    (if (<= diff 0)
        string
      (let ((pad-str (cl-loop repeat diff concat pad-char)))
        (if start
            (concat pad-str string)
          (concat string pad-str))))))

;;
;; Rendering and Layout Engine
;;

(defun org-mindmap--move-to (row col)
  "Navigate to ROW and COL within current buffer, padding spaces if needed."
  (goto-char (point-max))
  (let ((max-line (1- (line-number-at-pos))))
    (when (< max-line row)
      (insert (make-string (- row max-line) ?\n))))
  (goto-char (point-min))
  (forward-line row)
  (move-to-column col)
  (when (< (current-column) col)
    ;; TODO Here we have a surprising side effect, it should probably
    ;; be placed somewhere closer to rendering.
    (insert (org-mindmap--propertize-connector (make-string (- col (current-column)) ?\s)))
    (move-to-column col)))

;; Text rendering
(defun org-mindmap--node-display-text (node)
  "Return the actual string to be displayed for NODE, including delimiters if root."
  (let ((raw-text (org-mindmap-parser-node-text node)))
    (if (null (org-mindmap-parser-node-parent node))
        (let ((pair (car org-mindmap-parser-root-delimiters)))
          (if (string= raw-text "")
              (concat (car pair) (cdr pair))
            (concat (car pair) " " raw-text " " (cdr pair))))
      raw-text)))

(defun org-mindmap--add-root-delimiters (text &optional pair)
  "Add root delimiters from PAIR to the line of TEXT."
  (let ((left (if pair (car pair) " "))
        (right (if pair (cdr pair) " ")))
    (if (string= text "")
        (concat (car pair) (cdr pair))
      (concat left " " text " " right))))

(defun org-mindmap--join-short-lines (lines)
  "Join short LINES with previous lines to avoid orphaned words."
  (let ((acc (list (car lines)))
        (width (string-width (car lines))))
    (dolist (line (cdr lines))
      (cond
       ;; If a new line is very short, join it to the current one.
       ((<= (string-width (car acc)) org-mindmap-min-width)
        (setf (car acc) (string-join (list (car acc) line) " "))
        (setf width (max width (string-width (car acc)))))
       ;; If we can fit the next line into the box already reserved, do that.
       ((< (+ (string-width (car acc)) (string-width line)) width)
        (setf (car acc) (string-join (list (car acc) line) " ")))
       ;; Otherwise, keep the line.
       (t (push line acc))))
    (nreverse acc)))

(defconst org-mindmap-line-break "\\\\"
  "Marker for an in-node hard line break (two backslashes, as in Org).
It is stored literally in the node text and on the canvas, so it survives
parsing; display and SVG split on it and strip it.")

(defun org-mindmap--node-display-lines (node props)
  "Return display lines for NODE respecting :max-width and :wrap-leaves PROPS.
In-node hard breaks (the `org-mindmap-line-break' marker) split the text
into stacked lines; the marker is re-appended so it survives the canvas
round-trip (it is a literal cell that reparsing recovers)."
  (let* ((text (org-mindmap-parser-node-text node))
         (side (org-mindmap-parser-node-side node))
         (is-leaf (not (org-mindmap-parser-node-children node)))
         (wrap-leaves (plist-get props :wrap-leaves))
         (leaves-mult (if (numberp wrap-leaves) wrap-leaves 1))
         (max-width (plist-get props :max-width))
         (target-width (and max-width (floor (* (if is-leaf leaves-mult 1) max-width))))
         (segments (split-string text (regexp-quote org-mindmap-line-break)))
         (nseg (length segments))
         (lines
          (cl-loop for seg in segments
                   for si from 1
                   for seg* = (string-trim seg)
                   for sub = (if (string-empty-p seg*)
                                 ;; keep blank lines; a marker-only row below
                                 ;; makes it a real (non-empty) canvas cell
                                 (list "")
                               (org-mindmap--join-short-lines
                                (if (and max-width
                                         (or wrap-leaves (not is-leaf))
                                         (> (string-width seg*) target-width))
                                    (string-split (string-fill seg* target-width) "\n")
                                  (list seg*))))
                   append (if (< si nseg)
                              ;; re-append the marker to this segment's last line
                              (append (butlast sub)
                                      (list (concat (car (last sub))
                                                    org-mindmap-line-break)))
                            sub)))
         (node-box-width (apply #'max (mapcar #'string-width lines)))
         (padded-lines (mapcar #'(lambda (l) (org-mindmap--string-pad-width l node-box-width nil (eq side 'left))) lines)))
    ;; IDEA Put lines both below and above the connector row.
    (if (null (org-mindmap-parser-node-parent node))
        ;; ... append delimiters to the first line of the root node
        (append (list (org-mindmap--add-root-delimiters (car padded-lines) (car org-mindmap-parser-root-delimiters)))
                (mapcar #'org-mindmap--add-root-delimiters (cdr padded-lines)))
      ;; ... or just return the lines
      padded-lines)))

(defun org-mindmap--node-box (node props)
  "Calculate NODE width, respecting node text wrapping specified by PROPS.
Return (width height . lines) cons cell."
  (if-let* ((node-cache (plist-get props :node-cache))
            (box (gethash node node-cache)))
      box
    (let* ((lines (org-mindmap--node-display-lines node props))
           (width (string-width (car lines)))
           (height (length lines))
           (box (cons width (cons height lines))))
      (puthash node box node-cache)
      box)))

(defvar org-mindmap--side-children-cache nil
  "Dynamic cache for side children of nodes during layout.")

(defvar org-mindmap--side-descendants-cache nil
  "Dynamic cache for side descendants during layout.")

(defun org-mindmap--side-is (node side)
  "Check if NODE is on the given SIDE of the tree."
  (eq (org-mindmap-parser-node-side node) side))

(defun org-mindmap--side-children (node side)
  "Return NODE children from the tree SIDE."
  (if-let* ((cache org-mindmap--side-children-cache)
            (node-cache (gethash node cache))
            (cached (gethash side node-cache)))
      cached
    (let ((children (cl-remove-if-not (lambda (c) (org-mindmap--side-is c side))
                                      (org-mindmap-parser-node-children node))))
      (when org-mindmap--side-children-cache
        (let ((node-cache (or (gethash node org-mindmap--side-children-cache)
                              (puthash node (make-hash-table :test 'eq) org-mindmap--side-children-cache))))
          (puthash side children node-cache)))
      children)))

(defun org-mindmap--descendants (node)
  "Return all descendants of NODE from both sides of the tree."
  (cl-loop for child in (org-mindmap-parser-node-children node)
           append (cons child (org-mindmap--descendants child))))

(defun org-mindmap--subtree (node)
  "Return NODE and all its descendants from both sides of the tree."
  (cons node (org-mindmap--descendants node)))

(defun org-mindmap--side-descendants (node side)
  "Return NODE descendants (children, grandchildren etc) from the given tree SIDE."
  (if-let* ((cache org-mindmap--side-descendants-cache)
            (node-cache (gethash node cache))
            (cached (gethash side node-cache)))
      cached
    (let ((descendants (cl-loop for child in (org-mindmap--side-children node side)
                                append (cons child (org-mindmap--side-descendants child side)))))
      (when org-mindmap--side-descendants-cache
        (let ((node-cache (or (gethash node org-mindmap--side-descendants-cache)
                              (puthash node (make-hash-table :test 'eq) org-mindmap--side-descendants-cache))))
          (puthash side descendants node-cache)))
      descendants)))

;; Occupancy helpers
(defun org-mindmap--node-occupancy (node props)
  "Return occupancy as (start-col end-col) for NODE respecting PROPS."
  (let* ((side (org-mindmap-parser-node-side node))
         (row (org-mindmap-parser-node-row node))
         (col (org-mindmap-parser-node-col node))
         (spacing (plist-get props :spacing))
         (box (org-mindmap--node-box node props))
         (len (car box))
         (num-lines (cadr box))
         (parent (org-mindmap-parser-node-parent node))
         (parent-col (when parent (org-mindmap-parser-node-col parent)))
         (parent-len (when parent (car (org-mindmap--node-box parent props))))
         (start-col (if (eq side 'left) (- col spacing) (if parent (+ parent-col parent-len 1) col)))
         (end-col (if (eq side 'left) (if parent parent-col (+ col len)) (+ col len spacing))))
    (cl-loop for i below num-lines collect (list (+ row i) start-col end-col))))

;; Tree compaction helpers
(defun org-mindmap--check-overlap-subtree (rel-occ base-row delta occupied-map)
  "Check if shifting REL-OCC by BASE-ROW + DELTA overlaps OCCUPIED-MAP."
  (cl-loop for (rel-row start-col end-col) in rel-occ
           thereis
           (let ((r (+ rel-row base-row delta)))
             (cl-loop for (occ-start . occ-end) in (gethash r occupied-map)
                      thereis (not (or (<= end-col occ-start) (>= start-col occ-end)))))))


(defvar org-mindmap--subtree-occ-cache nil
  "Dynamic cache for subtree occupancy lists.")

(defun org-mindmap--get-subtree-occupancy (node props)
  "Return relative occupancy list for NODE subtree based on PROPS."
  (if-let* ((cache org-mindmap--subtree-occ-cache)
            (cached (gethash node cache)))
      cached
    (let* ((own-row (org-mindmap-parser-node-row node))
           (len (car (org-mindmap--node-box node props)))
           (col (org-mindmap-parser-node-col node))
           ;; Own node occupancy (relative)
           (node-occ (cl-loop for item in (org-mindmap--node-occupancy node props)
                              collect (cons (- (car item) own-row) (cdr item))))
           (sub-occs node-occ))
      ;; Add vertical connectors and children subtrees
      (dolist (side (list 'left 'right))
        (when-let* ((children (org-mindmap--side-children node side))
                    (conn-c (if (= len 0) col (if (eq side 'left) (- col 2) (+ col len 1))))
                    (first-r (org-mindmap-parser-node-row (car children)))
                    (last-r (org-mindmap-parser-node-row (car (last children)))))
          ;; Add vertical connector
          (setq sub-occs
                (append
                 (cl-loop for r from first-r to last-r
                          collect (list (- r own-row) conn-c (1+ conn-c)))
                 sub-occs))
          ;; Add children relative occupancies
          (dolist (child children)
            (let* ((child-occ (org-mindmap--get-subtree-occupancy child props))
                   (child-row (org-mindmap-parser-node-row child))
                   (row-offset (- child-row own-row)))
              (setq sub-occs
                    (append
                     (cl-loop for item in child-occ
                              collect (cons (+ (car item) row-offset) (cdr item)))
                     sub-occs))))))
      (when org-mindmap--subtree-occ-cache
        (puthash node sub-occs org-mindmap--subtree-occ-cache))
      sub-occs)))

(defun org-mindmap--shift-subtree-rows (node delta)
  "Shift rows of NODE and all its descendants by DELTA."
  (cl-incf (org-mindmap-parser-node-row node) delta)
  (dolist (child (org-mindmap-parser-node-children node))
    (org-mindmap--shift-subtree-rows child delta)))

(defun org-mindmap--max-row-subtree (node props)
  "Find the maximum row of NODE and its descendants, using PROPS."
  (let ((max-r (+ (org-mindmap-parser-node-row node) (1- (cadr (org-mindmap--node-box node props))))))
    (dolist (child (org-mindmap-parser-node-children node))
      (setq max-r (max max-r (org-mindmap--max-row-subtree child props))))
    max-r))

(defun org-mindmap--update-occupied-map (occupied-map node props)
  "Update OCCUPIED-MAP with NODE subtree locations using PROPS."
  (let* ((row (org-mindmap-parser-node-row node))
         (subtree-rel-occ (org-mindmap--get-subtree-occupancy node props)))
    (dolist (occ subtree-rel-occ)
      (let ((r (+ (car occ) row)))
        (push (cons (cadr occ) (caddr occ)) (gethash r occupied-map))))))

(defun org-mindmap--shift-subtree (node prev-node occupied-map props)
  "Shift NODE subtree downwards and update OCCUPIED-MAP.
Requires PREV-NODE (may be nil) and map PROPS."
  (let* ((compacted (plist-get props :compacted))
         (delta
          ;; Compute vertical shift:
          (if compacted
              ;; ... if compacting, shift the tree upwards if there's vacant space
              (let* ((row (org-mindmap-parser-node-row node))
                     ;; this prevents nodes from reordering
                     (delta (if prev-node
                                (+ (org-mindmap-parser-node-row prev-node)
                                   (cadr (org-mindmap--node-box prev-node props))
                                   (- row))
                              0))
                     ;; Get the relative occupancy, then shift it by delta
                     (subtree-rel-occ (org-mindmap--get-subtree-occupancy node props)))
                (while (org-mindmap--check-overlap-subtree subtree-rel-occ row delta occupied-map)
                  (cl-incf delta))
                delta)
            ;; ... otherwise just take the next unoccupied row
            (let ((d 0))
              (when prev-node
                (setq d (1+ (org-mindmap--max-row-subtree prev-node props))))
              d))))
    ;; Shift each child node downwards in the subtree.
    (when (> delta 0)
      (org-mindmap--shift-subtree-rows node delta))
    ;; Mark the tree location in the occupied map.
    (org-mindmap--update-occupied-map occupied-map node props)))

;; Subtree builder
(defun org-mindmap--min-row (nodes)
  "Find minimal row number among NODES if any, otherwise return 0."
  (if nodes
      (cl-loop for n in nodes minimize (org-mindmap-parser-node-row n))
    0))

(defun org-mindmap--max-row (nodes props)
  "Find maximal row number among NODES using PROPS, or 0 if none."
  ;; (if nodes (apply #'max (mapcar #'org-mindmap-parser-node-row nodes)) 0)
  (if nodes
      (cl-loop for node in nodes maximize
               (+ (org-mindmap-parser-node-row node) (1- (cadr (org-mindmap--node-box node props)))))
    0))

(defun org-mindmap--center-subtree (node props)
  "Vertically center NODE's tree, using PROPS for box sizes."
  (let* ((left-descendants (org-mindmap--side-descendants node 'left))
         (l-min (org-mindmap--min-row left-descendants))
         (l-max (org-mindmap--max-row left-descendants props))
         (l-middle (/ (+ l-min l-max) 2))
         (right-descendants (org-mindmap--side-descendants node 'right))
         (r-min (org-mindmap--min-row right-descendants))
         (r-max (org-mindmap--max-row right-descendants props))
         (r-middle (/ (+ r-min r-max) 2))
         (root-row (max l-middle r-middle))
         (l-shift (- root-row l-middle))
         (r-shift (- root-row r-middle)))
    (dolist (n left-descendants) (cl-incf (org-mindmap-parser-node-row n) l-shift))
    (dolist (n right-descendants) (cl-incf (org-mindmap-parser-node-row n) r-shift))
    root-row))

(defun org-mindmap-build-subtree (node col props)
  "Recursively calculate rows and cols for NODE and its children.
Requires starting COL and map PROPS."
  ;; Set the node column.
  (setf (org-mindmap-parser-node-col node) col)
  (let* ((text-len (car (org-mindmap--node-box node props)))
         (occupied-map (make-hash-table :test 'eq))
         (layout (plist-get props :layout)))
    ;; For each side of the tree:
    ;; (only root node may have two sides, so most of the time there will be only one side)
    (dolist (side (list 'left 'right))
      (let ((children (org-mindmap--side-children node side))
            (prev-child nil))
        ;; For each child node:
        (dolist (child children)
          (let* ((child-len (car (org-mindmap--node-box child props)))
                 (child-col (if (eq side 'left) (- col 4 child-len) (+ col text-len 4))))
            ;; ... position child subtree nodes starting from row 0
            (org-mindmap-build-subtree child child-col props))
          ;; ... shift the subtree below the previous children subtrees.
          (org-mindmap--shift-subtree child prev-child occupied-map props)
          (setq prev-child child))))
    ;; Set the node row:
    (setf (org-mindmap-parser-node-row node)
          (cond ((not (org-mindmap-parser-node-children node)) 0)
                ;; ...for centered layout, recenter the whole tree (!!! It has side effects)
                ((eq layout 'centered) (org-mindmap--center-subtree node props))
                ;; ...for top layout, take the top children rows
                (t (org-mindmap--min-row (org-mindmap--descendants node)))))))

(defun org-mindmap--min-column (nodes)
  "Find minimal column number among NODES if any, otherwise return 0."
  (if nodes
      (cl-loop for n in nodes minimize
               (let* ((col (org-mindmap-parser-node-col n))
                      (has-left-children (cl-some (lambda (c) (eq (org-mindmap-parser-node-side c) 'left))
                                                  (org-mindmap-parser-node-children n))))
                 (if has-left-children (- col 2) col)))
    0))

(defun org-mindmap-build-tree-layout (roots props)
  "Assign row and col to all nodes in ROOTS using map PROPS."
  (let ((occupied-map (make-hash-table :test 'eq))
        (org-mindmap--subtree-occ-cache (make-hash-table :test 'eq))
        (org-mindmap--side-children-cache (make-hash-table :test 'eq))
        (org-mindmap--side-descendants-cache (make-hash-table :test 'eq))
        (prev-root nil))
    ;; TODO Multiple roots are rudimentary, we should remove them and simplify the logic.
    (dolist (root roots)
      (org-mindmap-build-subtree root 3 props)
      (org-mindmap--shift-subtree root prev-root occupied-map props)
      (setq prev-root root))
    ;; Put the map to the upper-left corner if it somehow drifted away.
    (let* ((all-nodes (cl-loop for root in roots append (org-mindmap--subtree root)))
           (min-r (org-mindmap--min-row all-nodes))
           (min-c (org-mindmap--min-column all-nodes)))
      (dolist (n all-nodes)
        (cl-decf (org-mindmap-parser-node-row n) min-r)
        (cl-decf (org-mindmap-parser-node-col n) min-c))
      all-nodes)))

(defun org-mindmap--connector-symbol (has-above has-below has-left has-right)
  "Determine correct box-drawing character based on connection directions.
HAS-ABOVE, HAS-BELOW, HAS-LEFT, HAS-RIGHT are booleans."
  (let ((pack (car org-mindmap-parser-connectors)))
    (cond
     ((and has-above has-below has-left has-right) (char-to-string (nth 6 pack))) ; ┼
     ((and has-above has-below has-left (not has-right)) (char-to-string (nth 5 pack))) ; ┤
     ((and has-above has-below (not has-left) has-right) (char-to-string (nth 4 pack))) ; ├
     ((and has-above has-below (not has-left) (not has-right)) (char-to-string (nth 1 pack))) ; │
     ((and has-above (not has-below) has-left has-right) (char-to-string (nth 3 pack))) ; ┴
     ((and has-above (not has-below) has-left (not has-right)) (char-to-string (nth 10 pack))) ; ╯
     ((and has-above (not has-below) (not has-left) has-right) (char-to-string (nth 9 pack))) ; ╰
     ((and (not has-above) has-below has-left has-right) (char-to-string (nth 2 pack))) ; ┬
     ((and (not has-above) has-below has-left (not has-right)) (char-to-string (nth 8 pack))) ; ╮
     ((and (not has-above) has-below (not has-left) has-right) (char-to-string (nth 7 pack))) ; ╭
     ((and (not has-above) (not has-below) has-left has-right) (char-to-string (nth 0 pack))) ; ─
     (t (char-to-string (nth 1 pack))))))

(defun org-mindmap-draw-subtree (node props &optional color)
  "Write NODE text and connectors to buffer, using PROPS and optional COLOR."
  (let* ((node-row (org-mindmap-parser-node-row node))
         (node-col (org-mindmap-parser-node-col node))
         (box (org-mindmap--node-box node props))
         (node-len (car box))
         (node-lines (cddr box)))
    ;; Insert the node text.
    (cl-loop for i below (length node-lines) do
             (org-mindmap--move-to (+ node-row i) node-col)
             (let ((start (point)))
               (move-to-column (+ node-col node-len))
               (delete-region start (point)))
             (insert (org-mindmap--propertize-text (nth i node-lines) color)))
    ;; Draw children:
    (dolist (side (list 'left 'right))
      (when-let* ((children (org-mindmap--side-children node side))
                  (child-rows (mapcar #'org-mindmap-parser-node-row children))
                  (min-row (min (apply #'min child-rows) node-row))
                  (max-row (max (apply #'max child-rows) node-row))
                  ;; TODO replace 2 with `spacing' var.
                  (conn-col (if (eq side 'left) (- node-col 2) (+ node-col node-len 1))))
        ;; Iterate over rows between the first and the last child (a row may or may not contain a child).
        (cl-loop for row from min-row to max-row do
                 (org-mindmap--move-to row conn-col)
                 (let* ((has-above (> row min-row))
                        (has-below (< row max-row))
                        (has-left (if (eq side 'left) (memq row child-rows) (= row node-row)))
                        (has-right (if (eq side 'left) (= row node-row) (memq row child-rows)))
                        (sym (org-mindmap--connector-symbol has-above has-below has-left has-right))
                        (conn-str (if (eq side 'left) (concat "─" sym) (concat sym "─"))))
                   (cond ((and (eq side 'left) has-left)
                          (org-mindmap--move-to row (1- conn-col))
                          (let ((start (point)))
                            (move-to-column (+ (1- conn-col) 2))
                            (delete-region start (point)))
                          (insert (org-mindmap--propertize-connector conn-str color)))
                         ((and (eq side 'right) has-right)
                          (org-mindmap--move-to row conn-col)
                          (let ((start (point)))
                            (move-to-column (+ conn-col 2))
                            (delete-region start (point)))
                          (insert (org-mindmap--propertize-connector conn-str color)))
                         (t
                          (org-mindmap--move-to row conn-col)
                          (let ((start (point)))
                            (move-to-column (+ conn-col 1))
                            (delete-region start (point)))
                          (insert (org-mindmap--propertize-connector sym color)))))
                 ;; Fontify empty space to the right so that connector face background,
                 ;; if set non-standard, is applied to the whole map rectangle.
                 ;; TODO this works subpar with line wrapping, which is on in org buffers
                 ;; most of the time.
                 ;; (org-mindmap--move-to row (- (window-width) 2))
                 )
        (let ((depth (or (org-mindmap-parser-node-depth node) 0))
              (paint-depth (plist-get props :paint-depth)))
          (cl-loop for child in children
                   and i below (length children)
                   do (let ((child-color (if (and paint-depth (= depth paint-depth))
                                             (funcall org-mindmap-color-assign-fn child i)
                                           color)))
                        (org-mindmap-draw-subtree child props child-color))))))))

(defun org-mindmap-render-tree (roots &optional props)
  "Render ROOTS with given :layout and :spacing from PROPS.
When :compacted is non-nil, nodes fill vacant vertical spaces."
  (if (null roots)
      ""
    (org-mindmap-build-tree-layout roots props)
    (with-temp-buffer
      (setq indent-tabs-mode nil)
      (let ((inhibit-read-only t)
            (org-mindmap-color-palette (funcall org-mindmap-color-palette-fn)))
        (cl-loop for root in roots
                 do (org-mindmap-draw-subtree root props nil)))
      (buffer-string))))

;;
;; Alignment, Properties, and Regeneration
;;

(defun org-mindmap-parse-properties (start &optional props-string roots)
  "Extract property list from block header at START or PROPS-STRING.
ROOTS are used for default value calculation.
Handles legacy :layout left/compact/centered migration."
  (when (not props-string)
    (save-excursion
      (goto-char start)
      (when (re-search-forward "^[ \t]*#\\+begin_mindmap\\(.*\\)$" (line-end-position) t)
        (setq props-string (match-string 1)))))
  (when props-string
    (let ((props nil))
      ;; Parse mindmap block properties:
      (while (string-match "\\(:[a-zA-Z-]+\\)[ \t]+\\([^ \t\n]+\\)" props-string)
        (let ((key (intern (match-string 1 props-string)))
              (val (match-string 2 props-string)))
          (cond
           ((eq key :layout)
            (cond
             ;; legacy layouts:
             ;; ... "left" means top and sparse
             ((string= val "left")
              (setq props (plist-put props :layout "top"))
              (setq props (plist-put props :compacted nil)))
             ;; ... "compact" means top and compacted
             ((string= val "compact")
              (setq props (plist-put props :layout "top"))
              (setq props (plist-put props :compacted t)))
             (t
              (setq props (plist-put props :layout val)))))
           ((eq key :compacted)
            (setq props (plist-put props key (not (string= val "nil")))))
           ((eq key :max-width)
            (setq props (plist-put props key val)))
           ((eq key :wrap-leaves)
            (setq props (plist-put props key val)))
           (t
            (setq props (plist-put props key val)))))
        (setq props-string (substring props-string (match-end 0))))
      ;; Initialize caches
      (setq props (plist-put props :node-cache (make-hash-table :test 'eq)))
      ;; Fill in the default values.
      (org-mindmap--populate-properties props roots))))

(defun org-mindmap--populate-properties (&optional props roots)
  "Populate PROPS plist with default values for missing keys.
ROOTS are used to determine auto max-width."
  (setq props (plist-put props :layout
                         (intern
                          (or (plist-get props :layout)
                              (symbol-name org-mindmap-default-layout)))))
  (setq props (plist-put props :spacing
                         (if (plist-member props :spacing)
                             (string-to-number (plist-get props :spacing))
                           org-mindmap-default-spacing)))
  (setq props (plist-put props :compacted
                         (if (plist-member props :compacted)
                             (plist-get props :compacted)
                           org-mindmap-default-compacted)))
  (setq props (plist-put props :paint-depth
                         (if (plist-member props :paint-depth)
                             (pcase (plist-get props :paint-depth)
                               ("nil" nil)
                               (_ (string-to-number (plist-get props :paint-depth))))
                           org-mindmap-default-paint-depth)))
  (setq props (plist-put props :max-width
                         (let ((val
                                (if (plist-member props :max-width)
                                    (pcase (plist-get props :max-width)
                                      ("nil" nil)
                                      ("auto" 'auto)
                                      (_ (string-to-number (plist-get props :max-width))))
                                  org-mindmap-default-max-width)))
                           (if (eq val 'auto)
                               (when roots
                                 (org-mindmap--calculate-max-width roots))
                             val))))
  (setq props (plist-put props :wrap-leaves
                         (if (plist-member props :wrap-leaves)
                             (pcase (plist-get props :wrap-leaves)
                               ("nil" nil)
                               ("t" t)
                               (_ (string-to-number (plist-get props :wrap-leaves))))
                           org-mindmap-default-wrap-leaves)))
  props)

(defun org-mindmap--node-at-point (roots)
  "Traverse ROOTS and return the first node with `point-offset'."
  (catch 'found
    (let ((traverse nil))
      (setq traverse
            (lambda (node)
              (when (org-mindmap-parser-node-point-offset node)
                (org-mindmap-parser--debug "Found node at point: %s" (org-mindmap-parser-node-text node))
                (throw 'found node))
              (mapc traverse (org-mindmap-parser-node-children node))))
      (mapc traverse roots)
      (org-mindmap-parser--debug "No node at point found.")
      nil)))

(defun org-mindmap--calculate-max-width (roots)
  "Return optimal max-width for the current window and tree ROOTS."
  (let* ((root (car roots))
         (descendants (org-mindmap--descendants root))
         (sides (seq-group-by #'org-mindmap-parser-node-side descendants))
         (side-depths (mapcar #'(lambda (nodes) (mapcar #'org-mindmap-parser-node-depth nodes))
                              (mapcar #'cdr sides)))
         (depth (apply #'+ (mapcar #'seq-max side-depths))))
    (floor (/ (- (window-width) (* 4 (1+ depth))) (1+ depth)))))

(defun org-mindmap--get-state ()
  "Parse current region, return (start end props roots target-node)."
  (let* ((region (org-mindmap-parser-get-region)))
    (unless region (error "Not inside a mindmap region"))
    (let* ((start (car region))
           (end (cdr region))
           (roots (org-mindmap-parser-parse-region start end))
           (props (org-mindmap-parse-properties start nil roots))
           (target-node (org-mindmap--node-at-point roots)))
      (org-mindmap-parser--debug "Target node: %s" (when target-node (org-mindmap-parser-node-text target-node)))
      (list start end props roots target-node))))

(defun org-mindmap--restore-point (node props)
  "Return (row col) buffer-relative coordinates for NODE using PROPS.
Handles multi-line wrapped nodes, left/right padding, and root delimiters."
  (when-let* ((box (org-mindmap--node-box node props))
              (lines (cddr box))
              (row (org-mindmap-parser-node-row node))
              (col (org-mindmap-parser-node-col node))
              (offset (or (org-mindmap-parser-node-point-offset node) 0))
              (remaining offset)
              (row-offset 0)
              (col-offset (or (cl-loop for line in lines do
                                       (let* ((trimmed (string-trim line))
                                              (char-len (length trimmed))
                                              (len (1+ char-len)))
                                         (when (< remaining len)
                                           (cl-return (string-width (substring trimmed 0 remaining))))
                                         (setq remaining (- remaining len))
                                         (cl-incf row-offset)))
                              0)))
    (org-mindmap-parser--debug "Restoring position at %s: row=%d col=%d"
                               (org-mindmap-parser-node-text node) row-offset col-offset)
    ;; If offset fell past the last line, clamp to end of last line
    (when (>= row-offset (length lines))
      (setq row-offset (1- (length lines))
            col-offset (string-width (string-trim (car (last lines)))))
      (org-mindmap-parser--debug "... corrected: row=%d col=%d" row-offset col-offset))
    ;; Compute target column
    (let* ((display-line (nth row-offset lines))
           (trimmed-line (string-trim display-line))
           (text-start (string-match (regexp-quote trimmed-line) display-line))
           (visual-text-start (if text-start (string-width (substring display-line 0 text-start)) 0))
           (target-col (+ col visual-text-start col-offset)))
      ;; Handle root delimiter: first line has opening delimiter + space
      ;; (let ((is-root (null (org-mindmap-parser-node-parent node))))
      ;; (when (and is-root (= row-offset 0))
      ;;   (let* ((pair (car org-mindmap-parser-root-delimiters))
      ;;          (left-delim (car pair)))
      ;;     (setq target-col (+ col (string-width left-delim) 1 col-offset)))))
      (list (+ (org-mindmap-parser-node-row node) row-offset) target-col))))

(defun org-mindmap-switch-layout ()
  "Cycle layout mode between top and centered for the current mindmap region."
  (interactive)
  (let* ((region (org-mindmap-parser-get-region))
         (start (car region))
         (props (org-mindmap-parse-properties start))
         (current (or (plist-get props :layout)
                      (symbol-name org-mindmap-default-layout)))
         (next (if (eq current 'centered) 'top 'centered)))
    ;; TODO Fetch properties writer into a helper function.
    (save-excursion
      (goto-char start)
      (if (re-search-forward "\\(^[ \t]*#\\+begin_mindmap\\)\\(.*\\)$" (line-end-position) t)
          (let ((args (match-string 2)))
            (save-match-data
              (if (string-match " :layout [a-zA-Z]+" args)
                  (setq args (replace-match (format " :layout %s" next) t t args))
                (setq args (concat args (format " :layout %s" next)))))
            (replace-match args t t nil 2))))
    (org-mindmap-align)))

(defun org-mindmap-switch-compaction ()
  "Toggle :compacted property on the current mindmap block."
  (interactive)
  (let* ((region (org-mindmap-parser-get-region))
         (start (car region))
         (props (org-mindmap-parse-properties start))
         (new-compacted (not (plist-get props :compacted))))
    (save-excursion
      (goto-char start)
      (if (re-search-forward "\\(^[ \t]*#\\+begin_mindmap\\)\\(.*\\)$" (line-end-position) t)
          (let ((args (match-string 2)))
            (save-match-data
              (if (string-match " :compacted \\(t\\|nil\\)" args)
                  (setq args (replace-match (format " :compacted %s" (if new-compacted "t" "nil")) t t args))
                (if new-compacted
                    ;; Add :compacted t if toggling on and not present
                    (setq args (concat args " :compacted t"))
                  ;; Remove :compacted entirely if toggling off and not present with explicit value
                  ;; (but this branch only hit if :compacted wasn't in the string, so nothing to do)
                  nil)))
            (replace-match args t t nil 2))))
    (org-mindmap-align)))

(defun org-mindmap--update-buffer (start end roots &optional target-node props)
  "Replace region START to END with rendered ROOTS, focus TARGET-NODE.
Uses PROPS for rendering."
  (org-mindmap-parser-with-debug-batch
    "Update buffer"
    ;; Folds are a view over the old text; a redraw invalidates them.
    (org-mindmap--clear-folds)
    (let ((rendered (org-mindmap-render-tree roots props)))
      ;; Draw the map.
      (save-excursion
        (goto-char start)
        (forward-line 1)
        (let ((inhibit-read-only t))
          (delete-region (point) (save-excursion (goto-char end) (line-beginning-position)))
          (insert rendered "\n")))
      ;; Set the point on its last place.
      (goto-char start)
      (when target-node
        (if-let* ((pos (org-mindmap--restore-point target-node props))
                  (target-row (car pos))
                  (target-col (cadr pos)))
            ;; Restore point at exact logical offset
            (progn
              (forward-line (1+ target-row))
              (move-to-column target-col)
              ;; (while (looking-at " ") (forward-char))
              )
          ;; No offset: position at node start (existing behavior)
          (forward-line (1+ (org-mindmap-parser-node-row target-node)))
          (move-to-column (org-mindmap-parser-node-col target-node))
          (while (looking-at " ") (forward-char))
          (goto-char start))))))

(defun org-mindmap-align ()
  "Align and format the current mindmap region based on block properties."
  (interactive)
  (org-mindmap-parser-with-debug-batch
    "Align"
    (org-mindmap-parser--debug "Start aligning...")
    (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
      (org-mindmap--update-buffer start end roots target-node props)
      ;; Export to an image when the block header carries `:file'.
      (when (plist-get props :file)
        (org-mindmap-export-maybe start end roots props)))
    (org-mindmap-parser--debug "Finish aligning.")))

;;
;; Structural Editing — Insert and Delete
;;

(defun org-mindmap-find-node-at-point ()
  "Locate the node corresponding to the cursor position."
  (cl-destructuring-bind (_start _end _props _roots target-node) (org-mindmap--get-state)
    target-node))

(defcustom org-mindmap-node-paste-command nil
  "Command used for paste while editing a node in the minibuffer.
When non-nil (e.g. `org-paste-plus-dwim'), the node-edit minibuffer
remaps `yank' to it, so whatever key you normally paste with (e.g. the
macOS Cmd-V) runs it there -- letting a clipboard image be inserted as a
file link (with any `#+ATTR' lines) into the node text.  nil keeps the
default `yank'."
  :type '(choice (const :tag "Default yank" nil) (function :tag "Command"))
  :group 'org-mindmap)

(defun org-mindmap--node-paste ()
  "Paste in the node-edit minibuffer via `org-mindmap-node-paste-command'.
The command runs in a scratch buffer that borrows the source Org buffer's
file name and directory, so it can save a clipboard image into the right
`.assets' folder; the text it produces is then inserted into the
minibuffer (a trailing newline is dropped, inner newlines stay as an
in-node break).  Falls back to `yank' when the option is unset."
  (interactive)
  (let ((cmd org-mindmap-node-paste-command))
    (if (not (and cmd (commandp cmd)))
        (call-interactively #'yank)
      (let* ((src (window-buffer (minibuffer-selected-window)))
             (bfn (and src (buffer-local-value 'buffer-file-name src)))
             (dd  (and src (buffer-local-value 'default-directory src)))
             (text (with-temp-buffer
                     (when dd (setq default-directory dd))
                     (when bfn (setq buffer-file-name bfn))
                     (delay-mode-hooks (when (fboundp 'org-mode) (org-mode)))
                     (unwind-protect
                         (let ((enable-recursive-minibuffers t))
                           (call-interactively cmd)
                           (buffer-substring-no-properties (point-min) (point-max)))
                       (setq buffer-file-name nil)
                       (set-buffer-modified-p nil)))))
        (insert (replace-regexp-in-string "\n+\\'" "" text))))))

(defun org-mindmap-resize-step (delta)
  "Return the pixel step for DELTA (+1/-1), from `org-paste-plus-resize-step'."
  (* delta (if (boundp 'org-paste-plus-resize-step)
               org-paste-plus-resize-step 50)))

(defun org-mindmap--latex-width (px)
  "Return the LaTeX `\\linewidth' multiplier string for PX pixels."
  (let* ((ref (if (boundp 'org-paste-plus-latex-reference-width)
                  org-paste-plus-latex-reference-width 800.0))
         (ratio (/ (float px) ref)))
    (if (>= ratio 1.0) "1.0" (number-to-string ratio))))

(defun org-mindmap--bump-widths (text step)
  "Return TEXT with every integer `:width N' changed by STEP px (min 1).
Matches a px width bounded by a non-digit (space, the `\\\\' break marker,
`:', newline) or end.  A fractional `:width F\\linewidth' (LaTeX) is not
bumped directly but recomputed from the new pixel width."
  (with-temp-buffer
    (insert text)
    (let (new-px)
      ;; Pass 1: integer px widths (ATTR_ORG / ATTR_HTML / plain).
      (goto-char (point-min))
      (while (re-search-forward ":width \\([0-9]+\\)\\(?:[^0-9.]\\|$\\)" nil t)
        (setq new-px (max 1 (+ (string-to-number (match-string 1)) step)))
        (replace-match (number-to-string new-px) t t nil 1))
      ;; Pass 2: recompute the LaTeX \linewidth fraction from that px width.
      (when new-px
        (goto-char (point-min))
        (when (re-search-forward ":width \\([0-9.]+\\)\\\\linewidth" nil t)
          (replace-match (org-mindmap--latex-width new-px) t t nil 1))))
    (buffer-string)))

(defun org-mindmap--node-resize (delta)
  "Bump every integer `:width N' in the node-edit minibuffer by DELTA steps.
The in-minibuffer equivalent of `org-paste-plus-increase' / `-decrease':
it edits the size that the next `C-c C-c' will render."
  (let* ((cur (minibuffer-contents))
         (new (org-mindmap--bump-widths cur (org-mindmap-resize-step delta))))
    (unless (string= new cur)
      (delete-minibuffer-contents)
      (insert new))))

(defvar org-mindmap-read-node-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "S-<return>")
                (lambda () (interactive) (insert "\n")))
    ;; Route paste (whatever key runs `yank', e.g. Cmd-V) through the
    ;; configurable paste command so clipboard images can be inserted.
    (define-key map [remap yank] #'org-mindmap--node-paste)
    ;; Resize an image's :width in place, mirroring org-paste-plus's C-+/C--.
    (define-key map (kbd "C-+") (lambda () (interactive) (org-mindmap--node-resize 1)))
    (define-key map (kbd "C--") (lambda () (interactive) (org-mindmap--node-resize -1)))
    map)
  "Minibuffer keymap for node text: `S-<return>' inserts an in-node break;
paste is routed through `org-mindmap-node-paste-command'; `C-+' / `C--'
adjust an image `:width'.")

(defun org-mindmap--read-node-text (prompt &optional text point-offset)
  "Read node text with PROMPT, allowing `S-<return>' for an in-node break.
Stored `org-mindmap-line-break' markers show as real newlines while
editing and are restored on return.  TEXT is the initial content."
  (let* ((init (replace-regexp-in-string
                ;; eat the space the reparse round-trip leaves after a marker
                (concat (regexp-quote org-mindmap-line-break) " *")
                "\n" (or text "")))
         (input (read-from-minibuffer
                 prompt (cons init (when point-offset (1+ point-offset)))
                 org-mindmap-read-node-map)))
    ;; LITERAL=t: the marker contains backslashes, which the replacement
    ;; syntax would otherwise eat (turning "\\\\" into "\\").
    (replace-regexp-in-string "\n" org-mindmap-line-break input nil t)))

(defun org-mindmap-edit-node ()
  "Edit the text of the node at point and refresh the mindmap."
  (interactive)
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (let* ((point-offset (org-mindmap-parser-node-point-offset target-node))
           (text (org-mindmap-parser-node-text target-node))
           (new-text (org-mindmap--read-node-text "Edit node: " text point-offset)))
      (setf (org-mindmap-parser-node-text target-node) new-text)
      (org-mindmap--update-buffer start end roots target-node props))))

(defun org-mindmap--insert-after (lst target new-item)
  "Insert NEW-ITEM into LST immediately after TARGET."
  (let ((res nil))
    (dolist (item lst)
      (push item res)
      (when (eq item target)
        (push new-item res)))
    (nreverse res)))

(defun org-mindmap-insert-child (&optional text)
  "Create new child node with optional TEXT under node at cursor position.
If TEXT is nil or empty, creates an empty node for immediate editing.
With prefix argument at root node, creates a child on the left side."
  (interactive (list (org-mindmap--read-node-text "Child text: ")))
  (setq text (or text ""))
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (let* ((side (if (null (org-mindmap-parser-node-parent target-node))
                     (if current-prefix-arg 'left 'right)
                   (org-mindmap-parser-node-side target-node)))
           (new-node (org-mindmap-parser-make-node :id (gensym "node")
                                                   :text text
                                                   :parent target-node
                                                   :side side)))
      (setf (org-mindmap-parser-node-children target-node)
            (append (org-mindmap-parser-node-children target-node) (list new-node)))
      (org-mindmap--update-buffer start end roots new-node props)
      new-node)))

(defun org-mindmap-insert-sibling (&optional text)
  "Create new sibling node with optional TEXT after node at cursor position.
If TEXT is nil or empty, creates an empty node for immediate editing.
If target-node is the root node, it calls `org-mindmap-insert-child`."
  (interactive (list (org-mindmap--read-node-text "Sibling text: ")))
  (setq text (or text ""))
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (if (not (equal (car roots) target-node))
        (let* ((parent (org-mindmap-parser-node-parent target-node))
               (new-node (org-mindmap-parser-make-node :id (gensym "node")
                                                       :text text
                                                       :parent parent
                                                       :side (if target-node (org-mindmap-parser-node-side target-node) 'right))))
          (if parent
              (let ((siblings (org-mindmap-parser-node-children parent)))
                (setf (org-mindmap-parser-node-children parent)
                      (org-mindmap--insert-after siblings target-node new-node)))
            (setq roots (org-mindmap--insert-after roots target-node new-node)))
          (org-mindmap--update-buffer start end roots new-node props)
          new-node)
      (org-mindmap-insert-child text))))

(defun org-mindmap-insert-root (&optional text)
  "Create new root node with optional TEXT at end of existing roots.
If TEXT is nil or empty, creates an empty node for immediate editing.
In the single-root model, this is only allowed if no root exists."
  (interactive (list (org-mindmap--read-node-text "Root text: ")))
  (setq text (or text ""))
  (cl-destructuring-bind (start end props roots _target-node) (org-mindmap--get-state)
    (if (and roots (> (length roots) 0))
        (user-error "A root node already exists.  This mindmap only supports a single root")
      (let ((new-node (org-mindmap-parser-make-node :id (gensym "node") :text text)))
        (setq roots (list new-node))
        (org-mindmap--update-buffer start end roots new-node props)))))

(defun org-mindmap--get-next-focus (lst target fallback-parent)
  "Get the NODE to focus after TARGET is deleted from LST."
  (when-let* ((pos (cl-position target lst)))
    (cond ((< (1+ pos) (length lst)) (nth (1+ pos) lst)) ; next sibling
          ((> pos 0) (nth (1- pos) lst)) ; previous sibling
          (fallback-parent fallback-parent))))

(defun org-mindmap-delete-node ()
  "Remove node at cursor position and all descendants."
  (interactive)
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (when (and (org-mindmap-parser-node-children target-node)
               org-mindmap-confirm-delete
               (not (y-or-n-p "Node has children.  Delete anyway? ")))
      (user-error "Aborted"))
    (let* ((parent (org-mindmap-parser-node-parent target-node))
           (next-focus-node nil)
           (side (org-mindmap-parser-node-side target-node)))
      (if parent
          (let* ((all-siblings (org-mindmap-parser-node-children parent))
                 (siblings (if side
                               (cl-remove-if-not (lambda (n) (eq (org-mindmap-parser-node-side n) side)) all-siblings)
                             all-siblings)))
            (setq next-focus-node (org-mindmap--get-next-focus siblings target-node parent))
            (setf (org-mindmap-parser-node-children parent) (remq target-node all-siblings)))
        ;; Root node
        (if (= (length roots) 1)
            (error "Cannot delete the last root node")
          (setq next-focus-node (org-mindmap--get-next-focus roots target-node nil))
          (setq roots (remq target-node roots))))
      (org-mindmap--update-buffer start end roots next-focus-node props))))

;;
;; Movement Operations — Reorder and Restructure
;;

(defun org-mindmap--list-swap (lst i j)
  "Swap elements at index I and J in LST."
  (let* ((vec (vconcat lst))
         (tmp (aref vec i)))
    (aset vec i (aref vec j))
    (aset vec j tmp)
    (append vec nil)))

(defun org-mindmap-validate-move (operation target-node siblings pos)
  "Validate that move OPERATION is legal for TARGET-NODE with SIBLINGS at POS."
  (when (null (org-mindmap-parser-node-parent target-node))
    (user-error "Cannot move the root node"))
  (pcase operation
    ('up (when (or (null pos) (= pos 0))
           (user-error "Cannot move up: already first sibling")))
    ('down (when (or (null pos) (= pos (1- (length siblings))))
             (user-error "Cannot move down: already last sibling")))
    ('promote nil) ; Promotion of top-level child is now side-shift, so it's always valid
    ('demote (when (or (null pos) (= pos 0))
               (user-error "Cannot demote: requires a previous sibling")))))

(defun org-mindmap-move-up ()
  "Swap node with previous sibling."
  (interactive)
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (let* ((parent (org-mindmap-parser-node-parent target-node))
           (all-siblings (if parent (org-mindmap-parser-node-children parent) roots))
           (side (org-mindmap-parser-node-side target-node))
           (siblings (if side (cl-remove-if-not (lambda (n) (eq (org-mindmap-parser-node-side n) side)) all-siblings) all-siblings))
           (pos (cl-position target-node siblings)))
      (org-mindmap-validate-move 'up target-node siblings pos)
      (let ((prev (nth (1- pos) siblings)))
        (setq all-siblings (org-mindmap--list-swap all-siblings
                                                   (cl-position target-node all-siblings)
                                                   (cl-position prev all-siblings)))
        (if parent
            (setf (org-mindmap-parser-node-children parent) all-siblings)
          (setq roots all-siblings))
        (org-mindmap--update-buffer start end roots target-node props)))))

(defun org-mindmap-move-down ()
  "Swap node with next sibling."
  (interactive)
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (let* ((parent (org-mindmap-parser-node-parent target-node))
           (all-siblings (if parent (org-mindmap-parser-node-children parent) roots))
           (side (org-mindmap-parser-node-side target-node))
           (siblings (if side (cl-remove-if-not (lambda (n) (eq (org-mindmap-parser-node-side n) side)) all-siblings) all-siblings))
           (pos (cl-position target-node siblings)))
      (org-mindmap-validate-move 'down target-node siblings pos)
      (let ((next (nth (1+ pos) siblings)))
        (setq all-siblings (org-mindmap--list-swap all-siblings
                                                   (cl-position target-node all-siblings)
                                                   (cl-position next all-siblings)))
        (if parent
            (setf (org-mindmap-parser-node-children parent) all-siblings)
          (setq roots all-siblings))
        (org-mindmap--update-buffer start end roots target-node props)))))

(defun org-mindmap-promote ()
  "Move node up one level (becomes sibling of parent) or shift side if at root."
  (interactive)
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (org-mindmap-validate-move 'promote target-node nil nil)
    (let* ((parent (org-mindmap-parser-node-parent target-node))
           (grandparent (org-mindmap-parser-node-parent parent)))
      (if (null grandparent)
          ;; Case: target-node is a child of the root node. Shift side.
          (let ((new-side (if (eq (org-mindmap-parser-node-side target-node) 'left) 'right 'left)))
            (org-mindmap--set-side-recursive target-node new-side)
            ;; Move to the end of siblings list to be at the "bottom" of the other side
            (setf (org-mindmap-parser-node-children parent)
                  (append (remq target-node (org-mindmap-parser-node-children parent))
                          (list target-node))))
        ;; Case: Normal promotion to sibling of parent.
        (setf (org-mindmap-parser-node-children parent)
              (remq target-node (org-mindmap-parser-node-children parent)))
        (setf (org-mindmap-parser-node-parent target-node) grandparent)
        (setf (org-mindmap-parser-node-children grandparent)
              (org-mindmap--insert-after (org-mindmap-parser-node-children grandparent) parent target-node))
        ;; Inherit side from new parent (grandparent) if it has one
        (when (org-mindmap-parser-node-side grandparent)
          (org-mindmap--set-side-recursive target-node (org-mindmap-parser-node-side grandparent)))))
    (org-mindmap--update-buffer start end roots target-node props)))

(defun org-mindmap-demote ()
  "Move node down one level (becomes child of previous sibling)."
  (interactive)
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    (unless target-node (error "No node at point"))
    (let* ((parent (org-mindmap-parser-node-parent target-node))
           (all-siblings (if parent (org-mindmap-parser-node-children parent) roots))
           (side (org-mindmap-parser-node-side target-node))
           (siblings (if side (cl-remove-if-not (lambda (n) (eq (org-mindmap-parser-node-side n) side)) all-siblings) all-siblings))
           (pos (cl-position target-node siblings)))
      (org-mindmap-validate-move 'demote target-node siblings pos)
      (let ((prev-sibling (nth (1- pos) siblings)))
        (setq all-siblings (remq target-node all-siblings))
        (if parent
            (setf (org-mindmap-parser-node-children parent) all-siblings)
          (setq roots all-siblings))
        (setf (org-mindmap-parser-node-parent target-node) prev-sibling)
        (setf (org-mindmap-parser-node-children prev-sibling)
              (append (org-mindmap-parser-node-children prev-sibling) (list target-node)))
        ;; Inherit side from new parent
        (org-mindmap--set-side-recursive target-node (org-mindmap-parser-node-side prev-sibling))
        (org-mindmap--update-buffer start end roots target-node props)))))

;;
;; Auxilliary functions: conversion from and to org lists.
;;

(declare-function org-list-struct "org-list")
(declare-function org-list-get-top-point "org-list")
(declare-function org-list-to-lisp "org-list")
(declare-function org-at-item-p "org-list")
(declare-function org-element-property "org-element")
(declare-function org-element-type "org-element")
(declare-function org-element-at-point "org-element")


(defun org-mindmap--set-side-recursive (node side)
  "Set SIDE of NODE and all its descendants to SIDE."
  (setf (org-mindmap-parser-node-side node) side)
  (dolist (child (org-mindmap-parser-node-children node))
    (org-mindmap--set-side-recursive child side)))


(defun org-mindmap--lisp-to-nodes (lisp-list &optional parent side-override)
  "Convert an org-list LISP-LIST into a list of `org-mindmap-parser-node's.
The nodes are created as children of PARENT.  If SIDE-OVERRIDE is set,
all nodes and their descendants get that side."
  (let ((items (cdr lisp-list))
        (nodes nil)
        (current-side (or side-override 'right))
        (pivot-found nil))
    (dolist (item items)
      (let ((texts nil)
            (sublists nil)
            (is-empty t))
        (dolist (elem item)
          (if (stringp elem)
              (let ((trimmed (string-trim elem)))
                (push elem texts)
                (when (not (string= trimmed ""))
                  (setq is-empty nil)))
            (push elem sublists)
            (setq is-empty nil)))

        (if (and is-empty (not side-override) (not pivot-found))
            (setq current-side 'left
                  pivot-found t)
          (unless is-empty
            (let* ((full-text (replace-regexp-in-string
                               "[ \t\n\r]+" " "
                               (string-trim (mapconcat #'identity (nreverse texts) " "))))
                   (node (org-mindmap-parser-make-node :id (gensym "node")
                                                       :text full-text
                                                       :parent parent
                                                       :depth (if parent (1+ (org-mindmap-parser-node-depth parent)) 0)
                                                       :side current-side)))
              (when sublists
                (mapc (lambda (sl) (org-mindmap--lisp-to-nodes sl node current-side))
                      (nreverse sublists)))
              (push node nodes))))))
    (setf (org-mindmap-parser-node-children parent) (nreverse nodes))
    (nreverse nodes)))

(defun org-mindmap--get-list-context ()
  "Return (root-text begin-pos end-pos list-elem) if at a list or root-paragraph."
  (save-excursion
    (let* ((element (org-element-at-point))
           list-elem paragraph-elem
           tmp-list)
      ;; 1. Identify the top-most plain-list in the ancestry
      (let ((tmp element))
        (while tmp
          (when (eq (org-element-type tmp) 'plain-list)
            (setq tmp-list tmp))
          (setq tmp (org-element-property :parent tmp))))

      (if tmp-list
          (progn
            (setq list-elem tmp-list)
            ;; Look for a root paragraph immediately above the list
            (goto-char (org-element-property :begin list-elem))
            (let ((list-begin (point)))
              (forward-line -1)
              (while (and (not (bobp)) (looking-at-p "^[ \t]*$"))
                (forward-line -1))
              (let ((prev (org-element-at-point)))
                (when (and (eq (org-element-type prev) 'paragraph)
                           ;; Ensure it's not a list item itself
                           (not (eq (org-element-type (org-element-property :parent prev)) 'item))
                           ;; Ensure it actually ends right before the list (possibly with whitespace)
                           (save-excursion
                             (goto-char (org-element-property :end prev))
                             (while (and (< (point) list-begin) (looking-at-p "^[ \t]*$"))
                               (forward-line 1))
                             (>= (point) list-begin)))
                  (setq paragraph-elem prev)))))

        ;; 2. If not inside a list, check if we are on a paragraph followed by a list
        (when (and (eq (org-element-type element) 'paragraph)
                   (not (eq (org-element-type (org-element-property :parent element)) 'item)))
          (setq paragraph-elem element)
          (goto-char (org-element-property :end paragraph-elem))
          (while (and (not (eobp)) (looking-at-p "^[ \t]*$"))
            (forward-line 1))
          (let ((nxt (org-element-at-point)))
            (if (eq (org-element-type nxt) 'plain-list)
                (setq list-elem nxt)
              (setq paragraph-elem nil)))))

      (when list-elem
        (list (when paragraph-elem
                (string-trim (buffer-substring-no-properties
                              (org-element-property :contents-begin paragraph-elem)
                              (org-element-property :contents-end paragraph-elem))))
              (org-element-property :begin (or paragraph-elem list-elem))
              (org-element-property :end list-elem)
              list-elem)))))


(defun org-mindmap-list-to-mindmap ()
  "Convert the `org-mode' plain list at point into an `org-mindmap' block."
  (interactive)
  (let ((context (org-mindmap--get-list-context)))
    (unless context
      (user-error "Not at a list or a list's root paragraph"))
    (cl-destructuring-bind (root-text begin end list-elem) context
      (let* ((lisp-list (save-excursion
                          (goto-char (org-element-property :begin list-elem))
                          (org-list-to-lisp)))
             (root-node (org-mindmap-parser-make-node :id (gensym "node") :text (or root-text "") :depth 0))
             (_children (org-mindmap--lisp-to-nodes lisp-list root-node))
             (inhibit-read-only t)
             (props (org-mindmap-parse-properties nil (or root-text "") (list root-node))))
        (delete-region begin end)
        (let ((rendered (org-mindmap-render-tree (list root-node) props)))
          (save-excursion
            (goto-char begin)
            (insert "#+begin_mindmap\n" rendered "\n#+end_mindmap\n")))))))

(defun org-mindmap--nodes-to-list-string (nodes indent &optional side-filter)
  "Convert a list of `org-mindmap-parser-node's NODES into a plain list string.
Uses INDENT for the level.  If SIDE-FILTER is set, only include
nodes of that side."
  (let ((res nil)
        (prefix (make-string indent ?\ )))
    (dolist (node (if side-filter
                      (cl-remove-if-not (lambda (n) (eq (org-mindmap-parser-node-side n) side-filter)) nodes)
                    nodes))
      (push (concat prefix "- " (org-mindmap-parser-node-text node)) res)
      (when (org-mindmap-parser-node-children node)
        (let ((child-str (org-mindmap--nodes-to-list-string
                          (org-mindmap-parser-node-children node) (+ indent 2))))
          (when (not (string= child-str ""))
            (push child-str res)))))
    (mapconcat #'identity (nreverse res) "\n")))

(defun org-mindmap-to-list ()
  "Convert the `org-mindmap' block at point into an `org-mode' plain list."
  (interactive)
  (let ((region (org-mindmap-parser-get-region)))
    (unless region
      (user-error "Not inside an `org-mindmap' region"))
    (let* ((start (car region))
           (end (cdr region))
           (roots (org-mindmap-parser-parse-region start end)))
      (when (and roots (= (length roots) 1))
        (let* ((root (car roots))
               (root-text (org-mindmap-parser-node-text root))
               (children (org-mindmap-parser-node-children root))
               (right-children-str (org-mindmap--nodes-to-list-string children 0 'right))
               (left-children-str (org-mindmap--nodes-to-list-string children 0 'left))
               (result-list nil))
          (when (not (string= right-children-str ""))
            (push right-children-str result-list))
          (when (not (string= left-children-str ""))
            (push "-" result-list)
            (push left-children-str result-list))

          (save-excursion
            (goto-char start)
            (let ((inhibit-read-only t))
              (delete-region start (save-excursion
                                     (goto-char end)
                                     (forward-line 1)
                                     (point)))
              (when (and root-text (not (string= root-text "")))
                (insert root-text "\n"))
              (insert (mapconcat #'identity (nreverse result-list) "\n") "\n"))))))))

;;
;; Keybindings and Templates
;;

(defun org-mindmap--metaup ()
  "Hijack Org's M-<up>: move node at point upwrads if possible."
  (when (org-mindmap-parser-region-active-p)
    (org-mindmap-move-up)
    t))

(defun org-mindmap--metadown ()
  "Hijack Org's M-<down>: move node at point downwards if possible."
  (when (org-mindmap-parser-region-active-p)
    (org-mindmap-move-down)
    t))

(defun org-mindmap--metaleft ()
  "Hijack Org's M-<left>: move node at point left if possible."
  (when (org-mindmap-parser-region-active-p)
    (let ((node (org-mindmap-find-node-at-point)))
      (if (and node (eq (org-mindmap-parser-node-side node) 'left))
          (org-mindmap-demote)
        (org-mindmap-promote)))
    t))

(defun org-mindmap--metaright ()
  "Hijack Org's M-<right>: move node at point right if possible."
  (when (org-mindmap-parser-region-active-p)
    (let ((node (org-mindmap-find-node-at-point)))
      (if (and node (eq (org-mindmap-parser-node-side node) 'left))
          (org-mindmap-promote)
        (org-mindmap-demote)))
    t))

(defun org-mindmap--ctrl-c-ctrl-c ()
  "Hijack Org's `\\[org-ctrl-c-ctrl-c]': redraw the map and reallign the nodes."
  (when (org-mindmap-parser-region-active-p)
    (org-mindmap-align)
    t))

(defun org-mindmap--tab ()
  "Hijack Org's TAB key: insert a child node."
  (when (org-mindmap-parser-region-active-p)
    (let ((node (org-mindmap-find-node-at-point)))
      (when node
        (org-mindmap-insert-child)
        t))))

(defun org-mindmap--metareturn ()
  "Hijack Org's M-RET key: edit the node at point."
  (when (org-mindmap-parser-region-active-p)
    (org-mindmap-edit-node)
    t))

(defun org-mindmap--pad-other-body-lines (start end)
  "Prepend a space to every block body line except the one point is on.
Widens the whole map's left margin by one column so a left node whose text
has reached column 0 can keep growing leftward while its connector stays
in place (it moves right with every other line)."
  (let ((endm (copy-marker end))
        (cur (line-number-at-pos)))
    (save-excursion
      ;; START sits on the `#+begin_src' line; body begins one line down.
      (goto-char start)
      (forward-line 1)
      (while (< (point) endm)
        (when (and (/= (line-number-at-pos) cur) (not (eolp)))
          (insert " "))
        (forward-line 1)))))

(defun org-mindmap--left-justify-after-insert ()
  "Keep a left node's connector fixed while typing into it.
Left nodes are right-anchored: their `─╯'/`─╮'/`─┼' connector sits under
the parent's gutter and the text grows leftward.  Plain typing inserts
left-to-right, shoving that connector out of column and breaking the art
on the next re-parse.  After a single self-inserted character in a left
node, keep the connector column fixed: reclaim one leading space on this
line if there is one, otherwise widen the left margin by padding every
other body line (so the node grows leftward without limit).  `C-c C-c'
trims the margin back on the next render."
  (when (and (memq this-command '(self-insert-command org-self-insert-command))
             (org-mindmap-parser-region-active-p))
    (let ((state (ignore-errors (org-mindmap--get-state))))
      (when state
        (cl-destructuring-bind (start end _props _roots target) state
          (when (and target (eq (org-mindmap-parser-node-side target) 'left))
            (save-excursion
              (beginning-of-line)
              (if (eq (char-after) ?\s)
                  (delete-char 1)
                (org-mindmap--pad-other-body-lines start end)))))))))

(defun org-mindmap-return ()
  "Insert a sibling when on a mindmap node; else run RET's normal command.
The minor-mode map shadows RET buffer-wide, so outside a mindmap region
\(or inside one but not on a node) we must not force `org-return' -- that
would bypass whatever else binds RET.  Fall through to the real binding
instead, exactly as the navigation keys do."
  (interactive)
  (if (and (org-mindmap-parser-region-active-p)
           (org-mindmap-find-node-at-point))
      (org-mindmap-insert-sibling)
    (org-mindmap--fall-through)))

(defun org-mindmap-resize-node-image (delta)
  "Change the `:width' of the image in the mindmap node at point by DELTA steps.
Re-aligns the block so the new size takes effect on the next `C-c C-c'."
  (cl-destructuring-bind (start end props roots target-node) (org-mindmap--get-state)
    (unless target-node (user-error "No node at point"))
    (let* ((text (org-mindmap-parser-node-text target-node))
           (new (org-mindmap--bump-widths text (org-mindmap-resize-step delta))))
      (if (string= new text)
          (user-error "No :width to resize in this node")
        (setf (org-mindmap-parser-node-text target-node) new)
        (org-mindmap--update-buffer start end roots target-node props)))))

;; `org-mindmap-mode' is defined below by `define-minor-mode'; declare it
;; special here so the `let' in the dispatcher is a *dynamic* binding.
(defvar org-mindmap-mode)

(defun org-mindmap--resize-dispatch (delta)
  "Resize the node image when in a mindmap region; else run the normal key.
Falls through with `org-mindmap-mode' disabled so a bound `C-+' / `C--'
elsewhere (e.g. `org-paste-plus') still works."
  (if (org-mindmap-parser-region-active-p)
      (org-mindmap-resize-node-image delta)
    (let* ((org-mindmap-mode nil)
           (cmd (key-binding (this-command-keys-vector))))
      (when (commandp cmd)
        (setq this-command cmd)
        (call-interactively cmd)))))

(defun org-mindmap-increase ()
  "Grow the node image at point (in a mindmap region); else fall through."
  (interactive)
  (org-mindmap--resize-dispatch 1))

(defun org-mindmap-decrease ()
  "Shrink the node image at point (in a mindmap region); else fall through."
  (interactive)
  (org-mindmap--resize-dispatch -1))

;;
;; Point Navigation — move the cursor between nodes
;;

(defun org-mindmap--fall-through ()
  "Run the current key's normal binding with `org-mindmap-mode' disabled.
Lets a mode-map key stay inert outside a mindmap region."
  (let* ((org-mindmap-mode nil)
         (cmd (key-binding (this-command-keys-vector))))
    (when (commandp cmd)
      (setq this-command cmd)
      (call-interactively cmd))))

(defun org-mindmap--goto-node (node start)
  "Move point onto NODE within the region beginning at START."
  (goto-char start)
  (forward-line (1+ (org-mindmap-parser-node-row node)))
  (move-to-column (org-mindmap-parser-node-col node))
  (while (looking-at " ") (forward-char)))

(defun org-mindmap--navigate (fn)
  "Move point to the node FN returns for (TARGET ROOTS), if any.
Falls through to the normal key binding outside a mindmap region."
  (if (org-mindmap-parser-region-active-p)
      (cl-destructuring-bind (start _end _props roots target) (org-mindmap--get-state)
        (if (null target)
            (org-mindmap--fall-through)
          (let ((dest (funcall fn target roots)))
            (if dest
                (org-mindmap--goto-node dest start)
              (message "No node in that direction")))))
    (org-mindmap--fall-through)))

(defun org-mindmap--horizontal-move (target dir)
  "Return the node in screen direction DIR (`left'/`right') from TARGET.
Geometry is mirrored on the left: a left node's children lie further left
and its parent lies to the right.  So moving outward (DIR = node's side)
descends to a child; moving inward ascends to the parent.  On the root
\(side nil), DIR selects the first child on that side."
  (let ((side (org-mindmap-parser-node-side target)))
    (if side
        (if (eq dir side)
            (car (org-mindmap-parser-node-children target))
          (org-mindmap-parser-node-parent target))
      (cl-find dir (org-mindmap-parser-node-children target)
               :key #'org-mindmap-parser-node-side))))

(defun org-mindmap-goto-left ()
  "Move point one node to the left (toward a left child or a right parent)."
  (interactive)
  (org-mindmap--navigate
   (lambda (target _roots) (org-mindmap--horizontal-move target 'left))))

(defun org-mindmap-goto-right ()
  "Move point one node to the right (toward a right child or a left parent)."
  (interactive)
  (org-mindmap--navigate
   (lambda (target _roots) (org-mindmap--horizontal-move target 'right))))

(defun org-mindmap--all-nodes (roots)
  "Return every node under ROOTS as a flat list (pre-order)."
  (let (acc)
    (cl-labels ((walk (n)
                  (push n acc)
                  (mapc #'walk (org-mindmap-parser-node-children n))))
      (mapc #'walk roots))
    (nreverse acc)))

(defun org-mindmap--siblings (target roots)
  "Return TARGET's same-side siblings (including TARGET), row-sorted.
Top-level nodes are siblings of one another (parent is nil).  The side
filter matters at the root, whose children span both wings: a left branch
must not treat a right branch as its vertical neighbour."
  (let* ((parent (org-mindmap-parser-node-parent target))
         (side (org-mindmap-parser-node-side target))
         (sibs (cl-remove-if-not
                (lambda (n) (eq (org-mindmap-parser-node-side n) side))
                (if parent (org-mindmap-parser-node-children parent) roots))))
    (sort (copy-sequence sibs)
          (lambda (a b) (< (org-mindmap-parser-node-row a)
                           (org-mindmap-parser-node-row b))))))

(defun org-mindmap--vertical-move (target roots dir)
  "Return the node DIR (+1 down / -1 up) from TARGET.
Prefers the adjacent same-parent sibling; at the sibling edge, falls back
to the nearest node at the same depth in that vertical direction."
  (let* ((sibs (org-mindmap--siblings target roots))
         (idx (cl-position target sibs))
         (j (and idx (+ idx dir)))
         (sib (and j (<= 0 j) (nth j sibs))))
    (or sib
        ;; Sibling edge: nearest node further along DIR by row that is at the
        ;; same depth AND on the same side (so a right branch never jumps to
        ;; a left one just because it is nearer in raw row order).
        (let ((depth (org-mindmap-parser-node-depth target))
              (side (org-mindmap-parser-node-side target))
              (row (org-mindmap-parser-node-row target))
              best best-d)
          (dolist (n (org-mindmap--all-nodes roots) best)
            (let ((dr (* dir (- (org-mindmap-parser-node-row n) row))))
              (when (and (= (org-mindmap-parser-node-depth n) depth)
                         (eq (org-mindmap-parser-node-side n) side)
                         (> dr 0)
                         (or (null best-d) (< dr best-d)))
                (setq best n best-d dr))))))))

(defun org-mindmap-goto-down ()
  "Move down to the next sibling, or the nearest same-level node below."
  (interactive)
  (org-mindmap--navigate
   (lambda (target roots) (org-mindmap--vertical-move target roots 1))))

(defun org-mindmap-goto-up ()
  "Move up to the previous sibling, or the nearest same-level node above."
  (interactive)
  (org-mindmap--navigate
   (lambda (target roots) (org-mindmap--vertical-move target roots -1))))

(defun org-mindmap-delete ()
  "Delete the node at point in a mindmap region; else fall through."
  (interactive)
  (if (org-mindmap-parser-region-active-p)
      (org-mindmap-delete-node)
    (org-mindmap--fall-through)))

;;
;; Folding (display-only)
;;
;; Hide a node's subtree behind invisible overlays and show a small marker on
;; the node.  This is a *view* operation: the buffer text and the parse are
;; untouched, so export and alignment ignore folds.  A redraw clears folds
;; (`org-mindmap--clear-folds' runs in `org-mindmap--update-buffer').

(defface org-mindmap-fold-marker
  '((t :inherit shadow :weight bold))
  "Face for the fold indicator shown next to a folded node."
  :group 'org-mindmap)

(defvar-local org-mindmap--fold-overlays nil
  "Overlays implementing the current folds in this buffer.")

(defun org-mindmap--row-pos (start row col)
  "Return the buffer position of grid ROW/COL in the map beginning at START.
Mirrors how `org-mindmap--goto-node' walks from the block header, so fold
geometry lands on the same characters the renderer drew."
  (save-excursion
    (goto-char start)
    (forward-line (1+ row))
    (move-to-column col)
    (point)))

(defun org-mindmap--fold-row-range (node props)
  "Return (MIN-ROW . MAX-ROW), the grid rows NODE's whole subtree spans."
  (let ((min-r most-positive-fixnum) (max-r 0))
    (dolist (n (org-mindmap--subtree node) (cons min-r max-r))
      (let* ((r0 (org-mindmap-parser-node-row n))
             (r1 (+ r0 (1- (cadr (org-mindmap--node-box n props))))))
        (setq min-r (min min-r r0) max-r (max max-r r1))))))

(defun org-mindmap--clear-folds ()
  "Remove every fold overlay and marker in this buffer."
  (mapc #'delete-overlay org-mindmap--fold-overlays)
  (setq org-mindmap--fold-overlays nil))

(defun org-mindmap--fold-overlays-for (row)
  "Return this buffer's fold overlays anchored on grid ROW."
  (cl-remove-if-not (lambda (o) (eql (overlay-get o 'org-mindmap-fold-row) row))
                    org-mindmap--fold-overlays))

(defun org-mindmap--fold-node (node start props)
  "Fold NODE's subtree: hide it and add a marker.  Return non-nil on success.
A right-side subtree lies to the right of the node, a left-side subtree to
the left, so the hidden span on each of the subtree's rows runs from the
node's outward edge to the line end (right) or from the line start to the
node's inward edge (left)."
  (let* ((side (org-mindmap-parser-node-side node))
         (right (eq side 'right))
         (box (org-mindmap--node-box node props))
         (col (org-mindmap-parser-node-col node))
         (node-row (org-mindmap-parser-node-row node))
         (edge (if right (+ col (car box)) col))
         (range (org-mindmap--fold-row-range node props))
         (overlays nil))
    (cl-loop for r from (car range) to (cdr range) do
             (let* ((bol (org-mindmap--row-pos start r 0))
                    (eol (save-excursion (goto-char bol) (line-end-position)))
                    (edge-pos (min eol (org-mindmap--row-pos start r edge)))
                    (beg (if right edge-pos bol))
                    (fin (if right eol edge-pos)))
               (when (< beg fin)
                 (let ((ov (make-overlay beg fin)))
                   (overlay-put ov 'invisible t)
                   (overlay-put ov 'evaporate t)
                   (overlay-put ov 'org-mindmap-fold-row node-row)
                   (push ov overlays)))))
    ;; Marker hugging the node box on its own row.  Kept as a zero-length
    ;; overlay (evaporate nil) so a blank connector row can't drop it; folds
    ;; are torn down explicitly on unfold and on redraw.
    (let* ((anchor (min (save-excursion
                          (goto-char (org-mindmap--row-pos start node-row 0))
                          (line-end-position))
                        (org-mindmap--row-pos start node-row edge)))
           (mk (make-overlay anchor anchor)))
      (overlay-put mk (if right 'before-string 'after-string)
                   (propertize (if right " ▸" "◂ ") 'face 'org-mindmap-fold-marker))
      (overlay-put mk 'org-mindmap-fold-row node-row)
      (push mk overlays))
    (setq org-mindmap--fold-overlays (nconc overlays org-mindmap--fold-overlays))
    t))

(defun org-mindmap-toggle-fold ()
  "Fold or unfold the subtree of the node at point (display only).
Bound to \\`<C-tab>' inside a mindmap block; falls through elsewhere."
  (interactive)
  (if (not (org-mindmap-parser-region-active-p))
      (org-mindmap--fall-through)
    (cl-destructuring-bind (start _end props _roots node) (org-mindmap--get-state)
      (cond
       ((null node) (message "No node at point"))
       ((null (org-mindmap-parser-node-side node)) (message "Cannot fold the root"))
       ((null (org-mindmap-parser-node-children node)) (message "Leaf node — nothing to fold"))
       (t
        (let ((existing (org-mindmap--fold-overlays-for
                         (org-mindmap-parser-node-row node))))
          (if existing
              (progn
                (mapc #'delete-overlay existing)
                (setq org-mindmap--fold-overlays
                      (cl-set-difference org-mindmap--fold-overlays existing))
                (message "Unfolded"))
            (org-mindmap--fold-node node start props)
            (message "Folded"))))))))

(defvar org-mindmap-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'org-mindmap-return)
    ;; Resize an image's :width on its link/ATTR line inside a mindmap block,
    ;; mirroring org-paste-plus's C-+/C--; outside a region these fall through.
    (define-key map (kbd "C-+") #'org-mindmap-increase)
    (define-key map (kbd "C--") #'org-mindmap-decrease)
    ;; Move point between nodes (tree-semantic, not spatial).  These shadow
    ;; any `C-c <arrow>' binding (e.g. `winner') only *inside* a mindmap
    ;; region; each falls through to the normal command everywhere else in
    ;; the buffer (see `org-mindmap--navigate').  Plain `C-c <arrow>' is
    ;; used, not `C-c C-<arrow>', which macOS grabs for Mission Control.
    (define-key map (kbd "C-c <left>")  #'org-mindmap-goto-left)
    (define-key map (kbd "C-c <right>") #'org-mindmap-goto-right)
    (define-key map (kbd "C-c <up>")    #'org-mindmap-goto-up)
    (define-key map (kbd "C-c <down>")  #'org-mindmap-goto-down)
    ;; Delete the node at point (and its descendants).
    (define-key map (kbd "C-c C-d")     #'org-mindmap-delete)
    ;; Fold/unfold the subtree at point (display only).  <C-tab> is delivered
    ;; in GUI Emacs; a terminal frame usually cannot see it.
    (define-key map (kbd "<C-tab>")     #'org-mindmap-toggle-fold)
    map))

(define-minor-mode org-mindmap-mode
  "Editable mindmap visualization within `org-mode'."
  :lighter " ⅄"
  :keymap org-mindmap-mode-map
  (if org-mindmap-mode
      (progn
        (add-hook 'org-metaup-hook #'org-mindmap--metaup nil t)
        (add-hook 'org-metadown-hook #'org-mindmap--metadown nil t)
        (add-hook 'org-metaleft-hook #'org-mindmap--metaleft nil t)
        (add-hook 'org-metaright-hook #'org-mindmap--metaright nil t)
        (add-hook 'org-tab-first-hook #'org-mindmap--tab nil t)
        (add-hook 'org-metareturn-hook #'org-mindmap--metareturn nil t)
        (add-hook 'org-ctrl-c-ctrl-c-hook #'org-mindmap--ctrl-c-ctrl-c nil t)
        (add-hook 'post-self-insert-hook #'org-mindmap--left-justify-after-insert nil t))
    (remove-hook 'post-self-insert-hook #'org-mindmap--left-justify-after-insert t)
    (remove-hook 'org-metaup-hook #'org-mindmap--metaup t)
    (remove-hook 'org-metadown-hook #'org-mindmap--metadown t)
    (remove-hook 'org-metaleft-hook #'org-mindmap--metaleft t)
    (remove-hook 'org-metaright-hook #'org-mindmap--metaright t)
    (remove-hook 'org-tab-first-hook #'org-mindmap--tab t)
    (remove-hook 'org-metareturn-hook #'org-mindmap--metareturn t)
    (remove-hook 'org-ctrl-c-ctrl-c-hook #'org-mindmap--ctrl-c-ctrl-c t)))

(add-to-list 'org-structure-template-alist '("m" . "mindmap"))

(defun org-mindmap-unload-function ()
  "Clean up global state when `org-mindmap' is unloaded.
Removes entries from `minor-mode-map-alist', `minor-mode-alist',
and `org-structure-template-alist' that `unload-feature' cannot
reach on its own."
  (setq minor-mode-map-alist
        (assq-delete-all 'org-mindmap-mode minor-mode-map-alist))
  (setq minor-mode-alist
        (assq-delete-all 'org-mindmap-mode minor-mode-alist))
  (setq minor-mode-list
        (delq 'org-mindmap-mode minor-mode-list))
  (setq org-structure-template-alist
        (cl-delete "mindmap" org-structure-template-alist
                   :test #'string= :key #'cdr))
  ;; Return nil so standard unloading proceeds (hook removal, etc).
  nil)

(provide 'org-mindmap)

;;; org-mindmap.el ends here
