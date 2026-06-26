;;; ox-mindmap.el --- Org Babel backend for org-mindmap maps -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Randolph HUANG

;; Author: Randolph HUANG
;; Maintainer: Randolph HUANG
;; URL: https://github.com/wowhxj/ox-mindmap
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org-mindmap "0.3"))
;; Keywords: outlines, hypermedia, convenience, org

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ox-mindmap lets an `org-mindmap' map live inside a native Org Babel
;; source block, so it composes with the standard Babel machinery
;; (`:exports', `:eval', `:results', `:file', `#+RESULTS', `:var', export
;; to HTML/LaTeX, ...) instead of a bespoke `#+begin_mindmap' block.
;;
;;   #+begin_src mindmap :file map.svg :width 400
;;   « Root node » ┬─ child a
;;                 ╰─ child b
;;   #+end_src
;;
;; Pressing `C-c C-c' runs `org-babel-execute:mindmap', which:
;;
;;   1. re-aligns the Unicode map in the block body in place (so the body
;;      stays the editable, human-friendly source of truth), and
;;   2. renders a nice image to the `:file' target,
;;
;; while Org Babel inserts and maintains the `#+RESULTS' link.
;;
;; The heavy lifting -- parsing the map, computing the layout, drawing the
;; SVG -- is reused from the `org-mindmap' package, which ox-mindmap
;; requires.  ox-mindmap itself is only the thin Babel glue layer.
;;
;; Image attributes (`#+CAPTION', `#+ATTR_ORG/LATEX/HTML') are intentionally
;; NOT generated here: `:width' is exposed as a standard header argument so
;; it can be consumed by the same attribute-injection setup you use for
;; other graphics languages (ditaa, plantuml, dot, ...).  See the README.

;;; Code:

(require 'org-mindmap)
(require 'org-mindmap-svg)
(require 'ob)
(require 'org-element)

(defgroup ox-mindmap nil
  "Org Babel backend for `org-mindmap' maps."
  :group 'org-mindmap
  :prefix "ox-mindmap-")

(defcustom ox-mindmap-realign t
  "When non-nil, executing a mindmap src block re-aligns its body in place.
This keeps the Unicode map in the block body as the editable source of
truth (model A).  Set to nil to leave the body untouched and only export
the image."
  :type 'boolean
  :group 'ox-mindmap)

;; Graphics-style defaults, mirroring `ob-plantuml' / `ob-ditaa': the
;; execute function writes the file itself and Babel inserts the link.
;; `:wrap results' makes the link land inside a `#+begin_results' drawer,
;; which is the anchor used by attribute-injection setups.
(defvar org-babel-default-header-args:mindmap
  '((:results . "file link replace")
    (:exports . "results")
    (:wrap . "results"))
  "Default header arguments for mindmap src blocks.")

(defun ox-mindmap--parse-body (body)
  "Parse mindmap BODY string into a list of root nodes.
The body is wrapped in a throwaway `#+begin_mindmap' region so the
existing `org-mindmap' parser can read it."
  (with-temp-buffer
    (insert "#+begin_mindmap\n" body)
    (unless (bolp) (insert "\n"))
    (insert "#+end_mindmap\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((region (org-mindmap-parser-get-region)))
      (org-mindmap-parser-parse-region (car region) (cdr region)))))

(defun ox-mindmap--props (roots params)
  "Build render PROPS from ROOTS and Babel PARAMS.
Recognised header args: `:layout', `:max-width', `:compacted',
`:wrap-leaves', `:paint-depth'."
  (let ((props-string ""))
    (dolist (key '(:layout :max-width :compacted :wrap-leaves :paint-depth))
      (when-let* ((val (cdr (assq key params))))
        (setq props-string (concat props-string (format " %s %s" key val)))))
    (org-mindmap-parse-properties nil props-string roots)))

(defun ox-mindmap--realign-body (roots props)
  "Replace the body of the src block at point with re-rendered ROOTS.
Uses PROPS for rendering.  Does nothing if point is not in a src block."
  (let ((el (org-element-at-point)))
    (when (eq (org-element-type el) 'src-block)
      (save-excursion
        (goto-char (org-element-property :begin el))
        (when (re-search-forward "^[ \t]*#\\+begin_src[^\n]*\n"
                                 (org-element-property :end el) t)
          (let ((bstart (point)))
            (when (re-search-forward "^[ \t]*#\\+end_src"
                                     (org-element-property :end el) t)
              (let ((bend (line-beginning-position))
                    (new (org-mindmap-render-tree roots props)))
                (delete-region bstart bend)
                (goto-char bstart)
                (insert (substring-no-properties new) "\n")))))))))

;;;###autoload
(defun org-babel-execute:mindmap (body params)
  "Execute a mindmap src block: re-align the body and export an image.
BODY is the map text; PARAMS the header arguments.  Requires a `:file'
header argument naming the export target (`.svg' written directly,
`.png'/`.pdf' converted via an external tool when available).  Returns
nil; the file link is inserted by Org Babel."
  (let ((file (cdr (assq :file params))))
    (unless file
      (error "ox-mindmap: a mindmap src block requires a :file header argument"))
    (let* ((roots (ox-mindmap--parse-body body))
           (props (ox-mindmap--props roots params)))
      (unless roots
        (error "ox-mindmap: could not parse a map from the block body"))
      (when ox-mindmap-realign
        (ox-mindmap--realign-body roots props))
      (org-mindmap-svg-export-to-file file roots props)
      nil)))

;;;###autoload
(with-eval-after-load 'org-src
  ;; Avoid `C-c '\'' trying to load a non-existent `mindmap-mode'.
  (add-to-list 'org-src-lang-modes '("mindmap" . fundamental)))

(provide 'ox-mindmap)
;;; ox-mindmap.el ends here
