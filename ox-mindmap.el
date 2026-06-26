;;; ox-mindmap.el --- Org Babel backend for org-mindmap maps -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Randolph HUANG

;; Author: Randolph HUANG
;; Maintainer: Randolph HUANG
;; URL: https://github.com/wowhxj/ox-mindmap
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
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
;; SVG -- is done by the org-mindmap engine, originally by krvkir
;; (https://github.com/krvkir/org-mindmap).  That engine is BUNDLED with
;; ox-mindmap (the `org-mindmap*.el' files in this package), so ox-mindmap
;; is fully self-contained and has no external package dependency.
;;
;; NOTE: the bundled engine still `provide's the `org-mindmap' feature.  Do
;; not also install the standalone org-mindmap package alongside ox-mindmap,
;; or the two copies will collide.  Use one or the other.
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

;;; Interactive Unicode-canvas editing inside `#+begin_src mindmap' blocks
;;
;; The bundled engine already ships a full interactive editor as the
;; `org-mindmap-mode' minor mode: TAB inserts a child node, RET a sibling,
;; M-RET edits the node, and M-<arrows> move it.  Every one of its key
;; handlers is gated by `org-mindmap-parser-region-active-p', which calls
;; `org-mindmap-parser-get-region' -- and that only recognises native
;; `#+begin_mindmap ... #+end_mindmap' blocks, not Babel src blocks.
;;
;; We teach the *region detector* (not the whole engine) about Babel
;; `#+begin_src mindmap ... #+end_src' blocks via advice and enable the
;; minor mode in buffers that contain such a block.  Because every handler
;; falls through to the default Org behaviour whenever point is outside a
;; region, this stays inert everywhere else in the buffer.

(defcustom ox-mindmap-interactive t
  "When non-nil, make `#+begin_src mindmap' blocks interactively editable.
Enabling this installs advice so the `org-mindmap' canvas editor
(`org-mindmap-mode': TAB child, RET sibling, M-RET edit, M-<arrows> move)
works inside a mindmap Babel block, exactly as it does inside a native
`#+begin_mindmap' block.  `C-c C-c' is intentionally left to Org Babel so
it runs `org-babel-execute:mindmap' (re-align, export, `#+RESULTS')."
  :type 'boolean
  :group 'ox-mindmap
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'ox-mindmap--apply-interactive)
           (ox-mindmap--apply-interactive))))

(defconst ox-mindmap--begin-src-re
  "^[ \t]*#\\+begin_src[ \t]+mindmap\\b\\(.*\\)$"
  "Regexp matching the opening line of a mindmap Babel src block.
Submatch 1 is the trailing header-argument string.")

(defun ox-mindmap--src-region ()
  "Return (START . END) of the mindmap `#+begin_src' block around point.
START is the beginning of the `#+begin_src mindmap' line; END is the
beginning of the closing `#+end_src' line, so the engine parser treats
exactly the block body as the map (mirroring the bounds that
`org-mindmap-parser-get-region' returns for a native block)."
  (save-excursion
    (let ((orig (point)) start end)
      (end-of-line)
      (when (re-search-backward ox-mindmap--begin-src-re nil t)
        (setq start (line-beginning-position))
        (when (re-search-forward "^[ \t]*#\\+end_src\\b" nil t)
          (setq end (line-beginning-position))
          (when (and (<= start orig) (< orig end))
            (cons start end)))))))

(defun ox-mindmap--get-region-advice (orig)
  "Fall back to a Babel mindmap block for `org-mindmap-parser-get-region'.
ORIG is the original function."
  (or (funcall orig) (ox-mindmap--src-region)))

(defun ox-mindmap--parse-properties-advice (orig start &optional props-string roots)
  "Feed Babel-header args to `org-mindmap-parse-properties'.
ORIG is the original function; START, PROPS-STRING and ROOTS are its
arguments.  When the caller did not supply PROPS-STRING and START sits on
a `#+begin_src mindmap' line, use that line's header arguments."
  (when (and (null props-string) start)
    (save-excursion
      (goto-char start)
      (when (looking-at ox-mindmap--begin-src-re)
        (setq props-string (match-string 1)))))
  (funcall orig start props-string roots))

(defun ox-mindmap--ctrl-c-ctrl-c-advice (orig)
  "Let Org Babel own `C-c C-c' inside a mindmap src block.
ORIG is `org-mindmap--ctrl-c-ctrl-c'.  Returning nil from the hook lets
`org-ctrl-c-ctrl-c' proceed to `org-babel-execute:mindmap', which
re-aligns the body, (re)exports the image and keeps `#+RESULTS' in sync."
  (unless (ox-mindmap--src-region)
    (funcall orig)))

(defun ox-mindmap--maybe-enable ()
  "Enable `org-mindmap-mode' in an Org buffer that holds a mindmap block.
Buffers with neither a `#+begin_src mindmap' nor a `#+begin_mindmap' block
are left untouched."
  (when (and (derived-mode-p 'org-mode)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward
                "^[ \t]*#\\+begin_\\(?:src[ \t]+mindmap\\|mindmap\\)\\b" nil t)))
    (org-mindmap-mode 1)))

(defun ox-mindmap--apply-interactive ()
  "Install or remove the interactive-editing hooks per `ox-mindmap-interactive'."
  (if ox-mindmap-interactive
      (progn
        (advice-add 'org-mindmap-parser-get-region :around
                    #'ox-mindmap--get-region-advice)
        (advice-add 'org-mindmap-parse-properties :around
                    #'ox-mindmap--parse-properties-advice)
        (advice-add 'org-mindmap--ctrl-c-ctrl-c :around
                    #'ox-mindmap--ctrl-c-ctrl-c-advice)
        (add-hook 'org-mode-hook #'ox-mindmap--maybe-enable)
        ;; Catch buffers already open when ox-mindmap loads.
        (dolist (buf (buffer-list))
          (with-current-buffer buf (ox-mindmap--maybe-enable))))
    (advice-remove 'org-mindmap-parser-get-region #'ox-mindmap--get-region-advice)
    (advice-remove 'org-mindmap-parse-properties #'ox-mindmap--parse-properties-advice)
    (advice-remove 'org-mindmap--ctrl-c-ctrl-c #'ox-mindmap--ctrl-c-ctrl-c-advice)
    (remove-hook 'org-mode-hook #'ox-mindmap--maybe-enable)))

(ox-mindmap--apply-interactive)

(provide 'ox-mindmap)
;;; ox-mindmap.el ends here
