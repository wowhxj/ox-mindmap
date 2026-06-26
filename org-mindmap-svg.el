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

(defun org-mindmap-svg--hex (color)
  "Return COLOR as a #rrggbb hex string, or the neutral color if unknown."
  (let ((rgb (and color (color-name-to-rgb color))))
    (if rgb
        (apply #'org-mindmap--color-rgb-to-hex (append rgb '(2)))
      (or (and (color-name-to-rgb org-mindmap-svg-neutral-color)
               org-mindmap-svg-neutral-color)
          "#5b6b7a"))))

(defun org-mindmap-svg--blend (color other alpha)
  "Blend COLOR with OTHER by ALPHA (COLOR weight), returning a hex string."
  (let ((a (color-name-to-rgb (or color org-mindmap-svg-neutral-color)))
        (b (color-name-to-rgb other)))
    (apply #'org-mindmap--color-rgb-to-hex
           (append (org-mindmap--color-blend a b alpha) '(2)))))

(defun org-mindmap-svg--assign-colors (node props color table)
  "Fill TABLE mapping each node under NODE to its color.
Mirrors the paint-depth propagation used by the text renderer.  COLOR is
the inherited color (nil for the root); PROPS holds :paint-depth."
  (puthash node color table)
  (let ((depth (or (org-mindmap-parser-node-depth node) 0))
        (paint-depth (plist-get props :paint-depth)))
    (dolist (side '(left right))
      (let ((children (org-mindmap--side-children node side)))
        (cl-loop for child in children
                 for i from 0
                 do (let ((child-color (if (and paint-depth (= depth paint-depth))
                                           (funcall org-mindmap-color-assign-fn child i)
                                         color)))
                      (org-mindmap-svg--assign-colors child props child-color table)))))))

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
              (string-trim
               (apply #'string
                      (cl-remove-if (lambda (c) (memq c delim-chars))
                                    (string-to-list line)))))
            lines)))

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
         (stroke (org-mindmap-svg--hex color))
         (fill (org-mindmap-svg--blend color "white" 0.12))
         (text-color (org-mindmap-svg--blend color "black" 0.7))
         (lines (org-mindmap-svg--node-text-lines node props))
         (r org-mindmap-svg-node-radius))
    (with-current-buffer out
      (insert (format "  <rect x=\"%s\" y=\"%s\" width=\"%s\" height=\"%s\" rx=\"%s\" ry=\"%s\" fill=\"%s\" stroke=\"%s\" stroke-width=\"%s\"/>\n"
                      x y w h r r fill stroke org-mindmap-svg-stroke-width))
      (cl-loop for line in lines
               for i from 0
               do (let ((ty (+ y (* (+ i 0.5) org-mindmap-svg-cell-height)
                                (* 0.35 org-mindmap-svg-font-size)))
                        (tx (+ x (/ w 2.0))))
                    (insert (format "  <text x=\"%s\" y=\"%s\" font-family=\"%s\" font-size=\"%s\" fill=\"%s\" text-anchor=\"middle\">%s</text>\n"
                                    tx ty org-mindmap-svg-font-family
                                    org-mindmap-svg-font-size text-color
                                    (org-mindmap-svg--xml-escape line))))))))

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
    (let* ((org-mindmap-color-palette (funcall org-mindmap-color-palette-fn))
           (color-table (make-hash-table :test 'eq))
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
        (org-mindmap-svg--assign-colors root props nil color-table))
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
