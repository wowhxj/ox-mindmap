;;; org-mindmap-svg.el --- SVG/image export for org-mindmap -*- lexical-binding: t; -*-

;; This file is part of org-mindmap.

;;; Commentary:

;; Render a parsed mindmap tree into a nice-looking SVG image, reusing the
;; layout coordinates computed by the text engine so the picture matches the
;; Unicode map node-for-node.  When the `#+begin_mindmap' header carries a
;; `:file NAME.EXT' argument, `C-c C-c' exports the map to that file (SVG
;; written directly; PNG/PDF converted via an external tool when available,
;; gracefully falling back to SVG otherwise).  An optional `:width N' header
;; controls the inserted `#+RESULTS' block attributes.

;;; Code:

(require 'cl-lib)
(require 'color)
;; `org-mindmap' requires us back, so avoid a hard `require' loop here; the
;; layout/box/color helpers we use all live in org-mindmap.el and are loaded
;; by the time export runs.
;;
;; Mark the dynamic vars we rebind below as special *for the byte-compiler*.
;; Without these forward declarations, compiling this file without
;; org-mindmap.el loaded would turn the `let' on `org-mindmap-color-palette'
;; in `org-mindmap-svg-string' into a lexical binding that `assign-colors'
;; (which reads the dynamic value) could not see -- silently falling back to
;; the engine's default palette.  Valueless defvars do not clobber the real
;; definitions in org-mindmap.el.
(defvar org-mindmap-color-palette)
(defvar org-mindmap-color-palette-fn)

;;
;; Customization
;;

(defgroup org-mindmap-svg nil
  "SVG/image export for `org-mindmap'."
  :group 'org-mindmap)

(defcustom org-mindmap-svg-cell-width 9
  "Pixel width of one character cell when mapping the text grid to SVG."
  :type 'number
  :group 'org-mindmap-svg)

(defcustom org-mindmap-svg-cell-height 24
  "Pixel height of one character cell (one text row) when rendering SVG."
  :type 'number
  :group 'org-mindmap-svg)

(defcustom org-mindmap-svg-margin 20
  "Pixel margin around the whole map in the exported image."
  :type 'number
  :group 'org-mindmap-svg)

(defcustom org-mindmap-svg-node-radius 8
  "Corner radius (px) of node rectangles."
  :type 'number
  :group 'org-mindmap-svg)

(defcustom org-mindmap-svg-box-vgap 6
  "Vertical gap (px) carved out between vertically adjacent node boxes.
The text engine packs sibling rows tightly, which makes the SVG node
borders touch.  Each box is shrunk by half this amount top and bottom so
neighbours keep some breathing room.  Node text and connectors are left
where they are (centred), so only the visible border moves."
  :type 'number
  :group 'org-mindmap-svg)

(defcustom org-mindmap-svg-palette
  '("#e6194b" "#f58231" "#f5c518" "#3cb44b" "#4363d8" "#4b2bb5" "#911eb4")
  "Branch colour palette for the SVG export (a 7-colour rainbow set).
Each first-level branch is given the next colour in turn (cycling when
there are more branches than colours) and its whole subtree inherits it,
so the picture stays legible and theme-independent.  Set to nil to fall
back to `org-mindmap-color-palette-fn' instead."
  :type '(choice (const :tag "Use theme palette" nil)
                 (repeat string))
  :group 'org-mindmap-svg)

(defcustom org-mindmap-svg-font-family "sans-serif"
  "Font family used for node text in the SVG."
  :type 'string
  :group 'org-mindmap-svg)

(defcustom org-mindmap-svg-font-size 14
  "Font size (px) used for node text in the SVG."
  :type 'number
  :group 'org-mindmap-svg)

(defcustom org-mindmap-svg-stroke-width 2.0
  "Stroke width (px) for node borders and connector curves."
  :type 'number
  :group 'org-mindmap-svg)

(defcustom org-mindmap-svg-background "white"
  "Background color of the exported image, or nil for transparent."
  :type '(choice (const :tag "Transparent" nil) (string :tag "Color"))
  :group 'org-mindmap-svg)

(defcustom org-mindmap-svg-neutral-color "#5b6b7a"
  "Color used for unpainted nodes (e.g. the root)."
  :type 'string
  :group 'org-mindmap-svg)

(defcustom org-mindmap-svg-depth-fade 0.16
  "How much each level lightens its parent's branch color (0 disables).
A branch keeps one hue, but every deeper level is blended this fraction
further toward white, so a subtree fades from a saturated root to pale
leaves instead of being one flat color.  Set to 0 for the old behaviour."
  :type 'number
  :group 'org-mindmap-svg)

(defcustom org-mindmap-svg-converters
  '(("rsvg-convert" . (lambda (in out) (list "rsvg-convert" "-o" out in)))
    ("magick"       . (lambda (in out) (list "magick" in out)))
    ("convert"      . (lambda (in out) (list "convert" in out)))
    ("inkscape"     . (lambda (in out) (list "inkscape" in (concat "--export-filename=" out)))))
  "Alist of (EXECUTABLE . COMMAND-BUILDER) used to convert SVG to PNG/PDF.
COMMAND-BUILDER takes the input SVG path and output path and returns
the argument list (program first).  The first executable found on
`exec-path' is used."
  :type '(alist :key-type string :value-type function)
  :group 'org-mindmap-svg)

;;
;; Color helpers
;;

(defun org-mindmap-svg--rgb (color)
  "Return COLOR as an (R G B) list of 0..1 floats.
A `#rrggbb' string is parsed directly; anything else (named colors like
\"white\") falls back to `color-name-to-rgb'.  Parsing hex ourselves keeps
the result lossless and display-independent, so colors survive headless
or TTY export instead of being snapped to the nearest terminal color."
  (or (and (stringp color)
           (string-match
            "\\`#\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\'"
            color)
           (list (/ (string-to-number (match-string 1 color) 16) 255.0)
                 (/ (string-to-number (match-string 2 color) 16) 255.0)
                 (/ (string-to-number (match-string 3 color) 16) 255.0)))
      (and color (color-name-to-rgb color))))

(defun org-mindmap-svg--hex (color)
  "Return COLOR as a #rrggbb hex string, or the neutral color if unknown."
  (let ((rgb (org-mindmap-svg--rgb color)))
    (if rgb
        (apply #'org-mindmap--color-rgb-to-hex (append rgb '(2)))
      (or (and (org-mindmap-svg--rgb org-mindmap-svg-neutral-color)
               org-mindmap-svg-neutral-color)
          "#5b6b7a"))))

(defun org-mindmap-svg--blend (color other alpha)
  "Blend COLOR with OTHER by ALPHA (COLOR weight), returning a hex string."
  (let ((a (org-mindmap-svg--rgb (or color org-mindmap-svg-neutral-color)))
        (b (org-mindmap-svg--rgb other)))
    (apply #'org-mindmap--color-rgb-to-hex
           (append (org-mindmap--color-blend a b alpha) '(2)))))

(defun org-mindmap-svg--assign-colors (node props color table &optional counter)
  "Fill TABLE mapping each node under NODE to its branch color.
Coloring is deterministic and structural: a node sitting at the
`:paint-depth' boundary opens a new branch whose color is the next entry
of `org-mindmap-color-palette', taken in order and shared across both
sides (so sibling branches never collide and left/right stay in step);
every descendant inherits that branch color.  COLOR is the inherited
color (nil = the neutral root); COUNTER is an internal one-element list
holding the next palette index."
  (setq counter (or counter (list 0)))
  (puthash node color table)
  (let* ((depth (or (org-mindmap-parser-node-depth node) 0))
         (paint-depth (plist-get props :paint-depth))
         (palette org-mindmap-color-palette)
         (at-boundary (and paint-depth palette (= depth paint-depth))))
    (dolist (side '(left right))
      (dolist (child (org-mindmap--side-children node side))
        ;; Store the *base* branch color; the per-depth fade is applied at
        ;; render time to the fill only, so borders and text stay legible.
        (let ((child-color
               (if at-boundary
                   (let ((idx (car counter)))
                     (setcar counter (1+ idx))
                     (nth (mod idx (length palette)) palette))
                 color)))
          (org-mindmap-svg--assign-colors child props child-color table counter))))))

;;
;; Geometry & text helpers
;;

(defun org-mindmap-svg--xml-escape (s)
  "Escape XML special characters in string S."
  (let ((s (or s "")))
    (setq s (replace-regexp-in-string "&" "&amp;" s t t))
    (setq s (replace-regexp-in-string "<" "&lt;" s t t))
    (setq s (replace-regexp-in-string ">" "&gt;" s t t))
    s))

(defun org-mindmap-svg--node-text-lines (node props)
  "Return clean display lines for NODE (root delimiters stripped) using PROPS."
  (let* ((box (org-mindmap--node-box node props))
         (lines (cddr box))
         (delim-chars (delete-dups
                       (apply #'append
                              (mapcar (lambda (pair)
                                        (append (string-to-list (car pair))
                                                (string-to-list (cdr pair))))
                                      org-mindmap-parser-root-delimiters)))))
    (mapcar (lambda (line)
              (let ((clean (string-trim
                            (apply #'string
                                   (cl-remove-if (lambda (c) (memq c delim-chars))
                                                 (string-to-list line))))))
                ;; Drop the trailing in-node line-break marker (the split
                ;; into separate lines already happened at display-lines).
                (string-trim
                 (if (string-suffix-p org-mindmap-line-break clean)
                     (substring clean 0 (- (length clean)
                                           (length org-mindmap-line-break)))
                   clean))))
            lines)))

(defconst org-mindmap-svg--link-re
  "\\[\\[\\([^]]+\\)\\]\\(?:\\[\\([^]]*\\)\\]\\)?\\]"
  "Org link: submatch 1 is the target, optional submatch 2 the description.")

(defconst org-mindmap-svg--emphasis-re
  "\\([*/_=~+]\\)\\([^ \t\n][^\n]*?\\)\\1"
  "Org inline emphasis: submatch 1 is the marker, submatch 2 the body.")

(defun org-mindmap-svg--marker-attrs (marker)
  "Return the tspan attribute plist for an org emphasis MARKER char."
  (pcase marker
    (?* '(:font-weight "bold"))
    (?/ '(:font-style "italic"))
    (?_ '(:text-decoration "underline"))
    (?+ '(:text-decoration "line-through"))
    ((or ?= ?~) '(:font-family "monospace"))))

(defun org-mindmap-svg--markup-runs (line)
  "Split LINE into (TEXT . ATTRS) runs, parsing org links and emphasis.
ATTRS is a tspan attribute plist (nil for plain text).  A markup token
that is not closed within LINE (e.g. split by wrapping) is left literal.
A leading-`*' Org heading line renders as a single bold run."
  (if (string-match "\\`\\(\\*+\\)[ \t]+\\(.*\\)\\'" line)
      ;; heading: bold and enlarged, shrinking one step per nesting level
      (let* ((level (length (match-string 1 line)))
             (size (round (* org-mindmap-svg-font-size
                             (max 1.0 (- 1.5 (* 0.15 (1- level))))))))
        (list (cons (match-string 2 line)
                    (list :font-weight "bold" :font-size size))))
    (org-mindmap-svg--markup-runs-1 line)))

(defun org-mindmap-svg--markup-runs-1 (line)
  "Tokenize LINE into runs by Org links and inline emphasis (no heading rule)."
  (let ((runs '()) (i 0) (n (length line)))
    (while (< i n)
      (let ((lpos (string-match org-mindmap-svg--link-re line i))
            (epos (string-match org-mindmap-svg--emphasis-re line i)))
        (cond
         ((and (null lpos) (null epos))
          (push (cons (substring line i) nil) runs) (setq i n))
         ((and lpos (or (null epos) (<= lpos epos)))
          (string-match org-mindmap-svg--link-re line i) ; restore match data
          (when (> lpos i) (push (cons (substring line i lpos) nil) runs))
          (push (cons (or (match-string 2 line) (match-string 1 line))
                      '(:fill "#3b6ea5" :text-decoration "underline"))
                runs)
          (setq i (match-end 0)))
         (t
          (string-match org-mindmap-svg--emphasis-re line i) ; restore match data
          (when (> epos i) (push (cons (substring line i epos) nil) runs))
          (push (cons (match-string 2 line)
                      (org-mindmap-svg--marker-attrs (aref line epos)))
                runs)
          (setq i (match-end 0))))))
    (nreverse runs)))

(defun org-mindmap-svg--runs-to-tspans (runs)
  "Render RUNS (from `org-mindmap-svg--markup-runs') as escaped SVG tspans."
  (mapconcat
   (lambda (run)
     (let ((text (org-mindmap-svg--xml-escape (car run)))
           (attrs (cdr run)))
       (if (null attrs)
           text
         (format "<tspan%s>%s</tspan>"
                 (cl-loop for (k v) on attrs by #'cddr
                          concat (format " %s=\"%s\"" (substring (symbol-name k) 1) v))
                 text))))
   runs ""))

(defun org-mindmap-svg--node-geometry (node props)
  "Return plist (:x :y :w :h) in pixels for NODE using PROPS."
  (let* ((box (org-mindmap--node-box node props))
         (cw org-mindmap-svg-cell-width)
         (ch org-mindmap-svg-cell-height)
         (col (org-mindmap-parser-node-col node))
         (row (org-mindmap-parser-node-row node))
         (width (car box))
         (height (cadr box)))
    (list :x (* col cw)
          :y (* row ch)
          :w (* width cw)
          :h (* height ch))))

;;
;; SVG emission
;;

(defun org-mindmap-svg--node-svg (node props color-table out)
  "Append SVG for NODE rectangle and text to buffer OUT, using PROPS.
COLOR-TABLE maps nodes to inherited colors."
  (let* ((g (org-mindmap-svg--node-geometry node props))
         (x (plist-get g :x)) (y (plist-get g :y))
         (w (plist-get g :w)) (h (plist-get g :h))
         (color (or (gethash node color-table) org-mindmap-svg-neutral-color))
         ;; Depth fade lightens the *fill* only; a deeper subtree looks
         ;; paler while its border and text keep the branch color and stay
         ;; readable (`steps' = levels below the branch head).
         (depth (or (org-mindmap-parser-node-depth node) 0))
         (paint-depth (or (plist-get props :paint-depth) 0))
         (steps (max 0 (- depth paint-depth 1)))
         (fade (min 0.7 (* steps org-mindmap-svg-depth-fade)))
         (faded (org-mindmap-svg--blend color "white" (- 1.0 fade)))
         ;; keep a hint of the gradient on the border but never invisible
         (stroke (org-mindmap-svg--hex
                  (org-mindmap-svg--blend color "white" (- 1.0 (min 0.3 fade)))))
         (fill (org-mindmap-svg--blend faded "white" 0.12))
         (text-color (org-mindmap-svg--blend color "black" 0.7))
         (lines (org-mindmap-svg--node-text-lines node props))
         (r org-mindmap-svg-node-radius)
         ;; Shrink the box vertically so tightly-stacked siblings keep a
         ;; visible gap; text and connectors stay on the original centre.
         (vpad (min (/ org-mindmap-svg-box-vgap 2.0) (/ h 3.0)))
         (ry (+ y vpad))
         (rh (- h (* 2 vpad))))
    (with-current-buffer out
      (insert (format "  <rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" rx=\"%s\" ry=\"%s\" fill=\"%s\" stroke=\"%s\" stroke-width=\"%s\"/>\n"
                      x ry w rh r r fill stroke org-mindmap-svg-stroke-width))
      (cl-loop with multiline = (cdr lines)
               for line in lines
               for i from 0
               for rowc = (+ y (* (+ i 0.5) org-mindmap-svg-cell-height))
               do (cond
                   ;; Org horizontal rule (>=5 dashes): a full-width line.
                   ((string-match-p "\\`-\\{5,\\}\\'" (string-trim line))
                    (insert (format "  <line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" stroke=\"%s\" stroke-width=\"1\"/>\n"
                                    (+ x org-mindmap-svg-cell-width) rowc
                                    (- (+ x w) org-mindmap-svg-cell-width) rowc
                                    text-color)))
                   (t
                    (let* ((ty (+ rowc (* 0.35 org-mindmap-svg-font-size)))
                           (cx (+ x (/ w 2.0)))
                           ;; prettify a leading list bullet (- or +) as a dot
                           (line (replace-regexp-in-string
                                  "\\`\\([-+]\\)[ \t]+" "• " line))
                           (runs (org-mindmap-svg--markup-runs line))
                           (est (* (string-width (mapconcat #'car runs ""))
                                   org-mindmap-svg-font-size 0.5))
                           ;; Multi-line nodes read as a text block -> left-align
                           ;; every line to a common inset; single-line stays
                           ;; centered.  Flowing runs need text-anchor=start (a
                           ;; middle anchor piles tspans up).
                           anchor tx)
                      (cond
                       (multiline
                        (setq anchor "start" tx (+ x org-mindmap-svg-cell-width)))
                       ((cdr runs)
                        (setq anchor "start" tx (- cx (/ est 2.0))))
                       (t (setq anchor "middle" tx cx)))
                      (insert (format "  <text x=\"%s\" y=\"%s\" font-family=\"%s\" font-size=\"%s\" fill=\"%s\" text-anchor=\"%s\">%s</text>\n"
                                      tx ty org-mindmap-svg-font-family
                                      org-mindmap-svg-font-size text-color anchor
                                      (org-mindmap-svg--runs-to-tspans runs))))))))))

(defun org-mindmap-svg--connector-svg (parent child side props color-table out)
  "Append an SVG curve from PARENT to CHILD on SIDE to buffer OUT using PROPS.
COLOR-TABLE maps nodes to inherited colors."
  (let* ((pg (org-mindmap-svg--node-geometry parent props))
         (cg (org-mindmap-svg--node-geometry child props))
         (color (or (gethash child color-table) org-mindmap-svg-neutral-color))
         (stroke (org-mindmap-svg--hex color))
         (py (+ (plist-get pg :y) (/ (plist-get pg :h) 2.0)))
         (cy (+ (plist-get cg :y) (/ (plist-get cg :h) 2.0)))
         (px (if (eq side 'left)
                 (plist-get pg :x)
               (+ (plist-get pg :x) (plist-get pg :w))))
         (cx (if (eq side 'left)
                 (+ (plist-get cg :x) (plist-get cg :w))
               (plist-get cg :x)))
         (dx (/ (- cx px) 2.0)))
    (with-current-buffer out
      (insert (format "  <path d=\"M %s %s C %s %s %s %s %s %s\" fill=\"none\" stroke=\"%s\" stroke-width=\"%s\"/>\n"
                      px py (+ px dx) py (- cx dx) cy cx cy
                      stroke org-mindmap-svg-stroke-width)))))

(defun org-mindmap-svg--draw-subtree (node props color-table out)
  "Recursively emit SVG for NODE's connectors then nodes into OUT."
  (dolist (side '(left right))
    (dolist (child (org-mindmap--side-children node side))
      (org-mindmap-svg--connector-svg node child side props color-table out)
      (org-mindmap-svg--draw-subtree child props color-table out)))
  (org-mindmap-svg--node-svg node props color-table out))

(defun org-mindmap-svg-string (roots &optional props)
  "Return an SVG string rendering ROOTS, using map PROPS."
  (when roots
    (org-mindmap-build-tree-layout roots props)
    (let* ((org-mindmap-color-palette (or org-mindmap-svg-palette
                                           (funcall org-mindmap-color-palette-fn)))
           (color-table (make-hash-table :test 'eq))
           (color-counter (list 0))
           (all-nodes (cl-loop for root in roots append (org-mindmap--subtree root)))
           (cw org-mindmap-svg-cell-width)
           (ch org-mindmap-svg-cell-height)
           (m org-mindmap-svg-margin)
           (max-x (cl-loop for n in all-nodes maximize
                           (let ((g (org-mindmap-svg--node-geometry n props)))
                             (+ (plist-get g :x) (plist-get g :w)))))
           (max-y (cl-loop for n in all-nodes maximize
                           (let ((g (org-mindmap-svg--node-geometry n props)))
                             (+ (plist-get g :y) (plist-get g :h)))))
           (width (+ max-x (* 2 m)))
           (height (+ max-y (* 2 m))))
      (ignore cw ch)
      (dolist (root roots)
        (org-mindmap-svg--assign-colors root props nil color-table color-counter))
      (with-temp-buffer
        (insert (format "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"))
        (insert (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%s\" height=\"%s\" viewBox=\"0 0 %s %s\">\n"
                        width height width height))
        (when org-mindmap-svg-background
          (insert (format "  <rect x=\"0\" y=\"0\" width=\"%s\" height=\"%s\" fill=\"%s\"/>\n"
                          width height org-mindmap-svg-background)))
        ;; Shift everything by the margin via a group transform.
        (insert (format "  <g transform=\"translate(%s,%s)\">\n" m m))
        (dolist (root roots)
          (org-mindmap-svg--draw-subtree root props color-table (current-buffer)))
        (insert "  </g>\n</svg>\n")
        (buffer-string)))))

;;
;; File export & conversion
;;

(defun org-mindmap-svg--find-converter ()
  "Return (EXECUTABLE . BUILDER) for the first available SVG converter, or nil."
  (cl-find-if (lambda (entry) (executable-find (car entry)))
              org-mindmap-svg-converters))

(defun org-mindmap-svg-export-to-file (path roots props)
  "Render ROOTS to PATH (with PROPS).  Return the path actually written.
PATH may be SVG (written directly) or another format (converted via an
external tool).  When no converter is available for a non-SVG target,
falls back to writing a sibling .svg file and signals via `message'."
  (let* ((path (expand-file-name path))
         (ext (downcase (or (file-name-extension path) "svg")))
         (svg (org-mindmap-svg-string roots props)))
    (when-let* ((dir (file-name-directory path)))
      (unless (file-directory-p dir) (make-directory dir t)))
    (if (string= ext "svg")
        (progn
          (with-temp-file path (insert svg))
          path)
      (let ((converter (org-mindmap-svg--find-converter))
            (tmp (make-temp-file "org-mindmap-" nil ".svg")))
        (unwind-protect
            (progn
              (with-temp-file tmp (insert svg))
              (if converter
                  (let ((args (funcall (cdr converter) tmp path)))
                    (if (zerop (apply #'call-process (car args) nil nil nil (cdr args)))
                        path
                      (let ((fallback (concat (file-name-sans-extension path) ".svg")))
                        (with-temp-file fallback (insert svg))
                        (message "org-mindmap: %s failed; wrote %s instead"
                                 (car converter) fallback)
                        fallback)))
                (let ((fallback (concat (file-name-sans-extension path) ".svg")))
                  (with-temp-file fallback (insert svg))
                  (message "org-mindmap: no SVG converter found (need one of %s); wrote %s"
                           (mapconcat #'car org-mindmap-svg-converters ", ")
                           fallback)
                  fallback)))
          (when (file-exists-p tmp) (delete-file tmp)))))))

;;
;; #+RESULTS block insertion
;;

(defun org-mindmap-export--results-string (file width)
  "Return the `#+RESULTS' block text linking FILE.
When WIDTH is non-nil, emit a full results block with `#+ATTR_*' lines."
  (if width
      (concat "#+RESULTS:\n"
              "#+begin_results\n"
              "#+CAPTION:\n"
              (format "#+ATTR_ORG: :width %s\n" width)
              "#+ATTR_LATEX: :width 0.5\\linewidth :float nil\n"
              (format "#+ATTR_HTML: :width %s :class zoomImage :border 1\n" width)
              (format "[[file:%s]]\n" file)
              "#+end_results\n")
    (concat "#+RESULTS:\n"
            (format "[[file:%s]]\n" file))))

(defun org-mindmap-export-insert-results (end file width)
  "Insert or replace a `#+RESULTS' block after the map ending at END.
FILE is the link target; WIDTH controls attribute lines.  Idempotent:
an existing results block (with any leading blank lines) immediately
following the map is replaced, leaving exactly one blank separator line
between `#+end_mindmap' and `#+RESULTS:'."
  (save-excursion
    (goto-char end)
    (forward-line 1)
    (let ((seg-start (point)))
      ;; Remove an existing results block, consuming any leading blank
      ;; separator lines so repeated runs don't accumulate blanks.
      (save-excursion
        (goto-char seg-start)
        (while (and (not (eobp)) (looking-at-p "^[ \t]*$"))
          (forward-line 1))
        (when (looking-at-p "^[ \t]*#\\+RESULTS:")
          (forward-line 1)
          (cond
           ;; begin_results ... end_results form
           ((looking-at-p "^[ \t]*#\\+begin_results")
            (when (re-search-forward "^[ \t]*#\\+end_results.*$" nil t)
              (forward-line 1)))
           ;; bare link line(s) until blank
           (t
            (while (and (not (eobp)) (not (looking-at-p "^[ \t]*$")))
              (forward-line 1))))
          (delete-region seg-start (point))))
      (goto-char seg-start)
      ;; One blank separator line, then the results block.
      (insert "\n" (org-mindmap-export--results-string file width)))))

;;
;; Entry point used by `org-mindmap-align'
;;

(defun org-mindmap-export-maybe (_start _end roots props)
  "Export the map to an image when PROPS carries `:file'.
ROOTS is the parsed tree.  The map end is re-detected from the current
region, since the buffer may have been rewritten by a redraw.
Honors `:width' for the inserted results block."
  (when-let* ((file (plist-get props :file))
              (region (org-mindmap-parser-get-region))
              (end (cdr region)))
    (let* ((width (plist-get props :width))
           (written (org-mindmap-svg-export-to-file file roots props))
           ;; Link the path as written (may differ if a fallback happened),
           ;; expressed relative to the buffer when possible.
           (link (if (and buffer-file-name
                          (file-in-directory-p written default-directory))
                     (file-relative-name written default-directory)
                   written)))
      (org-mindmap-export-insert-results end link width))))

(provide 'org-mindmap-svg)
;;; org-mindmap-svg.el ends here
