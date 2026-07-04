;;; ox-mindmap-ai.el --- Generate/enhance mindmap blocks with an LLM -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: ox-mindmap contributors
;; Keywords: org, tools, outlines, ai
;; Package-Requires: ((emacs "27.1"))

;; This file is part of ox-mindmap and shares its license (GPL-3.0-or-later).

;;; Commentary:

;; One command, `ox-mindmap-ai-fill', that fills a `#+begin_src mindmap'
;; block from natural language via an LLM (using `gptel', a soft optional
;; dependency).  On an empty block it *generates* a map; on a non-empty
;; block it *enhances* the existing one.  It only rewrites the block body
;; -- it never runs `C-c C-c', so you review the text and export yourself.
;;
;; The LLM never touches the Unicode connector art.  It works purely in an
;; org indented outline (the same format the engine already round-trips via
;; `org-mindmap-to-list' / `org-mindmap-list-to-mindmap'); this file only
;; wraps the engine's existing outline<->tree<->art converters.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'ox-mindmap)

(declare-function gptel-request "gptel")

(defgroup ox-mindmap-ai nil
  "LLM-assisted authoring of mindmap blocks."
  :group 'ox-mindmap)

(defconst ox-mindmap-ai-system-prompt
  "You write mind maps as a plain indented OUTLINE, never as connector art.

Format rules (obey exactly):
- Line 1 is the ROOT node text, with no bullet.
- Every other node is a `- ' list item; indent children two more spaces
  than their parent.
- To place branches on the LEFT of the root, put a single bare `-' (an
  empty item) at the top level; every top-level branch AFTER it goes left.
  Branches BEFORE it (or all of them, if you omit it) go right. Nesting
  inherits its branch's side, so you only choose left/right once per
  top-level branch.
- For a node whose text spans several lines, join the lines with a literal
  backslash-backslash `\\\\' inside the one bullet (e.g. `- first\\\\second').
- Keep any `⟦MEDIA-n⟧' token EXACTLY as given, unchanged, on its own bullet.
  These stand for images you must not alter.
- Write node text in the same language as the user's request.

Output ONLY the outline. No code fences, no commentary, no blank leading
or trailing lines.

Example (root with both sides and a nested child):

Project
- Frontend
  - React
- Backend
-
- Notes\\\\see roadmap"
  "System prompt constraining the model to the engine's outline format.")

(defconst ox-mindmap-ai--media-re "\\[\\[[^][]+\\]\\]"
  "Matches an Org bracket link; used to detect media bullets to shield.")

;;; Outline <-> art (thin wrappers over engine converters)

(defun ox-mindmap-ai--outline->body (outline)
  "Render OUTLINE (root line + `- ' list) to mindmap connector art.
Signals a `user-error' if OUTLINE is not a usable outline."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert outline)
    (goto-char (point-min))
    (let ((ctx (org-mindmap--get-list-context)))
      (unless ctx
        (user-error "Model output is not a valid outline"))
      (cl-destructuring-bind (root-text _begin _end list-elem) ctx
        (goto-char (org-element-property :begin list-elem))
        (let* ((lisp (org-list-to-lisp))
               (root (org-mindmap-parser-make-node
                      :id (gensym "node") :text (or root-text "") :depth 0)))
          (org-mindmap--lisp-to-nodes lisp root)
          (let ((props (org-mindmap-parse-properties
                        nil (or root-text "") (list root))))
            (string-trim-right
             (substring-no-properties
              (org-mindmap-render-tree (list root) props)))))))))

(defun ox-mindmap-ai--body->outline (body)
  "Serialize mindmap connector-art BODY back to an indented outline string.
Returns nil when BODY does not parse to a single-rooted map."
  (let ((roots (ox-mindmap--parse-body body)))
    (when (and roots (= (length roots) 1))
      (let* ((root (car roots))
             (children (org-mindmap-parser-node-children root))
             (right (org-mindmap--nodes-to-list-string children 0 'right))
             (left (org-mindmap--nodes-to-list-string children 0 'left))
             (parts (list (org-mindmap-parser-node-text root))))
        (unless (string= right "") (setq parts (append parts (list right))))
        (unless (string= left "") (setq parts (append parts (list "-" left))))
        (mapconcat #'identity parts "\n")))))

;;; Media shielding (protect image links from the LLM on the enhance path)

(defun ox-mindmap-ai--shield (outline)
  "Replace media bullets in OUTLINE with opaque tokens.
Return (SHIELDED-OUTLINE . ALIST) where ALIST maps token -> original text."
  (let ((n 0) (alist nil))
    (cons
     (mapconcat
      (lambda (line)
        (if (and (string-match "\\`\\([ \t]*-[ \t]+\\)\\(.*\\)\\'" line)
                 (string-match-p ox-mindmap-ai--media-re (match-string 2 line)))
            (let ((token (format "⟦MEDIA-%d⟧" (cl-incf n))))
              (push (cons token (match-string 2 line)) alist)
              (concat (match-string 1 line) token))
          line))
      (split-string outline "\n")
      "\n")
     (nreverse alist))))

(defun ox-mindmap-ai--unshield (text alist)
  "Restore the original media text in TEXT for every token in ALIST.
Uses literal search/replace so backslashes in the media text survive."
  (with-temp-buffer
    (insert text)
    (dolist (pair alist)
      (goto-char (point-min))
      (while (search-forward (car pair) nil t)
        (replace-match (cdr pair) t t)))
    (buffer-string)))

;;; LLM plumbing

(defun ox-mindmap-ai--sanitize (s)
  "Strip code fences and normalize tabs in a model response S."
  (let ((s (string-trim s)))
    (when (string-match "\\````[^\n]*\n\\(\\(?:.\\|\n\\)*?\\)\n?```[ \t]*\\'" s)
      (setq s (match-string 1 s)))
    (string-trim (replace-regexp-in-string "\t" "  " s))))

(defun ox-mindmap-ai--prompt (instruction context)
  "Build the user prompt from INSTRUCTION and optional existing CONTEXT."
  (if context
      (format "Here is the current mind map outline:\n\n%s\n\nRevise it according to this request:\n%s"
              context instruction)
    (format "Create a mind map outline for this request:\n%s" instruction)))

(defun ox-mindmap-ai--apply (buf bstart bend outline)
  "Render OUTLINE and replace BUF's block body between markers BSTART..BEND."
  (let ((body (condition-case err
                  (ox-mindmap-ai--outline->body outline)
                (error
                 (message "ox-mindmap-ai: could not render model output: %s"
                          (error-message-string err))
                 nil))))
    (when (and body (buffer-live-p buf))
      (with-current-buffer buf
        (save-excursion
          (goto-char bstart)
          (delete-region bstart bend)
          (insert body "\n"))
        (message "ox-mindmap-ai: block filled -- review the text, then C-c C-c")))))

;;;###autoload
(defun ox-mindmap-ai-fill (instruction)
  "Fill the `#+begin_src mindmap' block at point from INSTRUCTION via an LLM.
An empty block is generated from scratch; a non-empty one is enhanced from
its current outline.  Only the block body is rewritten; run `C-c C-c'
yourself after reviewing.  Requires the `gptel' package."
  (interactive
   (progn
     (unless (ox-mindmap--src-region)
       (user-error "Point is not inside a #+begin_src mindmap block"))
     (list (read-string "Describe the mindmap (natural language): "))))
  (unless (require 'gptel nil t)
    (user-error "ox-mindmap-ai needs the `gptel' package; please install it"))
  (let* ((region (ox-mindmap--src-region))
         (bstart (save-excursion (goto-char (car region)) (forward-line 1)
                                 (copy-marker (point))))
         (bend (copy-marker (cdr region)))
         (body (string-trim (buffer-substring-no-properties bstart bend)))
         (existing (unless (string-empty-p body)
                     (ox-mindmap-ai--body->outline body)))
         (shield (and existing (ox-mindmap-ai--shield existing)))
         (buf (current-buffer)))
    (message "ox-mindmap-ai: asking the model...")
    (gptel-request (ox-mindmap-ai--prompt instruction (car shield))
      :system ox-mindmap-ai-system-prompt
      :callback
      (lambda (resp info)
        (if (not (stringp resp))
            (message "ox-mindmap-ai: request failed: %s"
                     (or (plist-get info :status) "no response"))
          (ox-mindmap-ai--apply
           buf bstart bend
           (ox-mindmap-ai--unshield (ox-mindmap-ai--sanitize resp)
                                    (cdr shield))))))))

;;; Self-check: the outline<->art converters must be lossless (the LLM is
;;; not, but the plumbing around it must be).  Run with M-x.
(defun ox-mindmap-ai--selftest ()
  "Assert an outline survives outline->art->outline.  Signals on failure."
  (let* ((outline "Root\n- A\n  - A1\n- B\n-\n- Left1")
         (back (ox-mindmap-ai--body->outline
                (ox-mindmap-ai--outline->body outline))))
    (cl-assert (string-match-p "A1" back) nil "nested child lost")
    (cl-assert (string-match-p "Left1" back) nil "left branch lost")
    (message "ox-mindmap-ai selftest OK:\n%s" back)))

(provide 'ox-mindmap-ai)
;;; ox-mindmap-ai.el ends here
