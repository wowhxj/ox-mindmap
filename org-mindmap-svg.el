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

;;
;; Image nodes: a node whose text is a bare image link renders the image
;;

(defcustom org-mindmap-svg-image-max-width 260
  "Maximum display width (px) for an embedded image node."
  :type 'number :group 'org-mindmap-svg)

(defcustom org-mindmap-svg-image-max-height 200
  "Maximum display height (px) for an embedded image node."
  :type 'number :group 'org-mindmap-svg)

(defconst org-mindmap-svg--image-link-re
  "\\`\\[\\[\\(?:file:\\)?\\([^][]+\\.\\(?:png\\|jpe?g\\|gif\\|svg\\|webp\\)\\)\\]\\]\\'"
  "A node whose whole text matches is rendered as the image in submatch 1.")

(defun org-mindmap-svg--image-link (text)
  "Return the image path if TEXT is a bare Org image link, else nil."
  (when text
    (let ((s (string-trim text)) (case-fold-search t))
      (when (string-match org-mindmap-svg--image-link-re s)
        (match-string 1 s)))))

(defun org-mindmap-svg--u (bytes i n big)
  "Read an N-byte unsigned int at I in unibyte string BYTES (BIG endian if t)."
  (let ((v 0))
    (dotimes (k n)
      (setq v (+ (* v 256) (aref bytes (+ i (if big k (- n 1 k)))))))
    v))

(defun org-mindmap-svg--parse-image-size (b ext)
  "Return (W . H) in px parsed from header bytes B for extension EXT, or nil."
  (pcase ext
    ("png" (when (and (>= (length b) 24) (= (aref b 0) #x89) (= (aref b 1) ?P))
             (cons (org-mindmap-svg--u b 16 4 t) (org-mindmap-svg--u b 20 4 t))))
    ("gif" (when (>= (length b) 10)
             (cons (org-mindmap-svg--u b 6 2 nil) (org-mindmap-svg--u b 8 2 nil))))
    ((or "jpg" "jpeg")
     (let ((i 2) (n (length b)) res)
       (while (and (< (+ i 9) n) (not res))
         (if (/= (aref b i) #xFF) (cl-incf i)
           (let ((m (aref b (1+ i))))
             (cond
              ((memq m '(#xC0 #xC1 #xC2 #xC3 #xC5 #xC6 #xC7 #xC9 #xCA #xCB #xCD #xCE #xCF))
               (setq res (cons (org-mindmap-svg--u b (+ i 7) 2 t)
                               (org-mindmap-svg--u b (+ i 5) 2 t))))
              ((or (= m #xD8) (= m #xD9)) (cl-incf i 2))
              (t (cl-incf i (+ 2 (org-mindmap-svg--u b (+ i 2) 2 t))))))))
       res))
    ("svg" (let ((s (ignore-errors (decode-coding-string b 'utf-8))))
             (cond
              ((and s (string-match "viewBox=\"[-0-9.]+[ ,]+[-0-9.]+[ ,]+\\([0-9.]+\\)[ ,]+\\([0-9.]+\\)" s))
               (cons (round (string-to-number (match-string 1 s)))
                     (round (string-to-number (match-string 2 s)))))
              ((and s (string-match "width=\"\\([0-9.]+\\)" s)
                    (string-match "height=\"\\([0-9.]+\\)" s))
               (cons (round (string-to-number (progn (string-match "width=\"\\([0-9.]+\\)" s) (match-string 1 s))))
                     (round (string-to-number (progn (string-match "height=\"\\([0-9.]+\\)" s) (match-string 1 s)))))))))
    (_ nil)))

(defvar org-mindmap-svg--image-size-cache (make-hash-table :test 'equal)
  "Memoize (abs-path -> (W . H)) so layout does not re-read files.")

(defun org-mindmap-svg--image-size (path)
  "Return (WIDTH . HEIGHT) in px for image PATH, reading its header, or nil."
  (let ((abs (expand-file-name path)))
    (if (gethash abs org-mindmap-svg--image-size-cache)
        (gethash abs org-mindmap-svg--image-size-cache)
      (let ((size (and (file-readable-p abs)
                       (ignore-errors
                         (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert-file-contents-literally abs nil 0 65536)
                           (org-mindmap-svg--parse-image-size
                            (buffer-string)
                            (downcase (or (file-name-extension abs) ""))))))))
        (puthash abs size org-mindmap-svg--image-size-cache)
        size))))

(defun org-mindmap-svg--image-data-uri (path)
  "Return a data: URI embedding image PATH, or nil if unreadable."
  (let ((abs (expand-file-name path))
        (ext (downcase (or (file-name-extension path) ""))))
    (when (file-readable-p abs)
      (let ((mime (pcase ext ("png" "image/png") ((or "jpg" "jpeg") "image/jpeg")
                         ("gif" "image/gif") ("svg" "image/svg+xml")
                         ("webp" "image/webp") (_ "application/octet-stream"))))
        (format "data:%s;base64,%s" mime
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert-file-contents-literally abs)
                  (base64-encode-string (buffer-string) t)))))))

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

(defun org-mindmap-svg--attr-line-p (line)
  "Non-nil if LINE is an Org keyword line (`#+KEY:'), hidden metadata.
Covers #+ATTR_*, #+CAPTION, #+NAME, #+DOWNLOADED, etc."
  (string-match-p "\\`[ \t]*#\\+[A-Za-z_]+:" line))

(defun org-mindmap-svg--line-width-spec (line)
  "Return the `:width' value (px) declared on LINE, or nil."
  (when (string-match ":width[ \t]+\\([0-9]+\\)" line)
    (string-to-number (match-string 1 line))))

(defun org-mindmap-svg--line-specs (lines)
  "Classify display LINES into render specs (text, hidden attr, or image).
Each spec is a plist: :kind (text|attr|image), :text, :wpx (box-width
contribution) and :hpx (row height in px).  Image specs add :uri and
:iwpx (draw width).  A `:width' on a preceding attr line sizes the image."
  (let ((cw org-mindmap-svg-cell-width)
        (ch org-mindmap-svg-cell-height)
        (pending nil) (specs nil))
    (dolist (line lines (nreverse specs))
      (let ((path (org-mindmap-svg--image-link line)))
        (cond
         ((org-mindmap-svg--attr-line-p line)
          (when-let* ((w (org-mindmap-svg--line-width-spec line))) (setq pending w))
          (push (list :kind 'attr :text line :wpx 0 :hpx 0) specs))
         ((and path (org-mindmap-svg--image-data-uri path))
          (let* ((uri (org-mindmap-svg--image-data-uri path))
                 (size (org-mindmap-svg--image-size path))
                 (ratio (if size (/ (float (car size)) (cdr size)) 1.6))
                 (iwpx (cond (pending (float pending))
                             (size (min (float org-mindmap-svg-image-max-width)
                                        (* org-mindmap-svg-image-max-height ratio)
                                        (float (car size))))
                             (t (float org-mindmap-svg-image-max-width))))
                 (ihpx (/ iwpx ratio)))
            (setq pending nil)
            (push (list :kind 'image :text line :uri uri :iwpx iwpx :ihpx ihpx
                        :hpx (+ ihpx org-mindmap-svg-box-vgap)
                        :wpx (max iwpx (* cw (string-width line))))
                  specs)))
         (t
          (push (list :kind 'text :text line
                      :wpx (* cw (string-width line)) :hpx ch)
                specs)))))))

(defun org-mindmap-svg--node-geometry (node props)
  "Return plist (:x :y :w :h) in pixels for NODE using PROPS.
Row Y comes from the `:row-y' cumulative map when present, so rows that
hold a tall image expand in the SVG without any blank rows in the source."
  (let* ((box (org-mindmap--node-box node props))
         (cw org-mindmap-svg-cell-width)
         (ch org-mindmap-svg-cell-height)
         (row-y (plist-get props :row-y))
         (col (org-mindmap-parser-node-col node))
         (row (org-mindmap-parser-node-row node))
         (width (car box))
         (height (cadr box))
         (y0 (if row-y (aref row-y row) (* row ch)))
         (y1 (if row-y (aref row-y (+ row height)) (* (+ row height) ch))))
    (list :x (* col cw) :y y0 :w (* width cw) :h (- y1 y0))))

;;
;; SVG emission
;;

(defun org-mindmap-svg--node-svg (node props color-table out)
  "Append SVG for NODE (background, text lines and inline images) to OUT.
COLOR-TABLE maps nodes to their branch colors."
  (let* ((g (org-mindmap-svg--node-geometry node props))
         (x (plist-get g :x)) (y (plist-get g :y))
         (w (plist-get g :w)) (h (plist-get g :h))
         (ch org-mindmap-svg-cell-height)
         (specs (org-mindmap-svg--line-specs
                 (org-mindmap-svg--node-text-lines node props)))
         (has-text (cl-some (lambda (s) (eq (plist-get s :kind) 'text)) specs))
         (color (or (gethash node color-table) org-mindmap-svg-neutral-color))
         ;; Depth fade lightens the *fill* only; border and text keep the
         ;; branch color so deep nodes stay legible.
         (depth (or (org-mindmap-parser-node-depth node) 0))
         (paint-depth (or (plist-get props :paint-depth) 0))
         (steps (max 0 (- depth paint-depth 1)))
         (fade (min 0.7 (* steps org-mindmap-svg-depth-fade)))
         (faded (org-mindmap-svg--blend color "white" (- 1.0 fade)))
         (stroke (org-mindmap-svg--hex
                  (org-mindmap-svg--blend color "white" (- 1.0 (min 0.3 fade)))))
         (fill (org-mindmap-svg--blend faded "white" 0.12))
         (text-color (org-mindmap-svg--blend color "black" 0.7))
         (r org-mindmap-svg-node-radius)
         ;; Pack the node's visible lines locally (text = a row, image = its
         ;; own height, hidden attr = 0) and center that block in the space
         ;; the layout reserved.  This keeps text and image adjacent even when
         ;; the metadata rows between them cannot collapse (shared grid rows).
         (packed (cl-loop for s in specs sum
                          (pcase (plist-get s :kind)
                            ('attr 0) ('image (plist-get s :ihpx)) (_ ch))))
         (top (+ y (max 0.0 (/ (- h packed) 2.0))))
         (vpad (min (/ org-mindmap-svg-box-vgap 2.0) (/ packed 3.0)))
         (multiline (cdr specs))
         (cw org-mindmap-svg-cell-width)
         ;; Border width hugs the *visible* content (text/image), not the
         ;; reserved box (which is sized to the long link text).  Content is
         ;; inset by one cell on each side inside a bordered node.
         (content-w (cl-loop for s in specs maximize
                             (pcase (plist-get s :kind)
                               ('attr 0)
                               ('image (plist-get s :iwpx))
                               (_ (* cw (string-width (plist-get s :text)))))))
         (rect-w (min w (+ content-w (* 2 cw))))
         (ix (if has-text (+ x cw) x)))
    (with-current-buffer out
      ;; Background box only when the node has real text; a pure image floats
      ;; borderless.  The box hugs the packed content, not the reserved space.
      (when has-text
        (insert (format "  <rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" rx=\"%s\" ry=\"%s\" fill=\"%s\" stroke=\"%s\" stroke-width=\"%s\"/>\n"
                        x (+ top vpad) rect-w (- packed (* 2 vpad)) r r fill stroke
                        org-mindmap-svg-stroke-width)))
      (cl-loop with cy = top
               for s in specs
               do (pcase (plist-get s :kind)
                    ('attr nil)         ; metadata line: hidden, zero height
                    ('image
                     (let ((ih (plist-get s :ihpx)))
                       (insert (format "  <image x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" preserveAspectRatio=\"xMinYMid meet\" xlink:href=\"%s\"/>\n"
                                       ix cy (plist-get s :iwpx) ih (plist-get s :uri)))
                       (setq cy (+ cy ih))))
                    (_
                     (let* ((line (plist-get s :text))
                            (rowc (+ cy (* 0.5 ch)))
                            (ty (+ rowc (* 0.35 org-mindmap-svg-font-size)))
                            (cx (+ x (/ w 2.0))))
                       (setq cy (+ cy ch))
                       (cond
                        ;; Org horizontal rule (>=5 dashes): a full-width line.
                        ((string-match-p "\\`-\\{5,\\}\\'" (string-trim line))
                         (insert (format "  <line x1=\"%s\" y1=\"%s\" x2=\"%s\" y2=\"%s\" stroke=\"%s\" stroke-width=\"1\"/>\n"
                                         (+ x cw) rowc
                                         (- (+ x rect-w) cw) rowc
                                         text-color)))
                        (t
                         (let* ((line (replace-regexp-in-string
                                       "\\`\\([-+]\\)[ \t]+" "• " line))
                                (runs (org-mindmap-svg--markup-runs line))
                                (est (* (string-width (mapconcat #'car runs ""))
                                        org-mindmap-svg-font-size 0.5))
                                ;; multi-line -> left-align block; single-line
                                ;; centers.  Flowing runs need anchor=start.
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
                                           (org-mindmap-svg--runs-to-tspans runs)))))))))))))

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
           ;; Build a row -> Y map so a row holding a tall image expands in the
           ;; SVG (pushing rows below it down) without adding blank rows to the
           ;; source.  extra[r] = how many px beyond one cell row r needs.
           (maxrow (1+ (cl-loop for n in all-nodes maximize
                                (+ (org-mindmap-parser-node-row n)
                                   (cadr (org-mindmap--node-box n props))))))
           (extra (make-vector (+ maxrow 1) 0))
           (_ (dolist (n all-nodes)
                (let ((r0 (org-mindmap-parser-node-row n))
                      (specs (org-mindmap-svg--line-specs
                              (org-mindmap-svg--node-text-lines n props))))
                  (cl-loop for s in specs for i from 0
                           for r = (+ r0 i)
                           when (< r (length extra))
                           do (aset extra r (max (aref extra r)
                                                 (- (plist-get s :hpx) ch)))))))
           (row-y (let ((v (make-vector (+ maxrow 2) 0)))
                    (dotimes (r (1+ maxrow))
                      (aset v (1+ r) (+ (aref v r) ch (aref extra r))))
                    v))
           (props (plist-put props :row-y row-y))
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
        (insert (format "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" width=\"%s\" height=\"%s\" viewBox=\"0 0 %s %s\">\n"
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
