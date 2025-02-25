;;; kdl-ts-mode.el --- Major mode for the KDL Document language -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Merrick Luo
;;
;; Author: Merrick Luo <merrick@luois.me>
;; Maintainer: Merrick Luo <merrick@luois.me>
;; Created: February 23, 2025
;; Modified: February 23, 2025
;; Version: 0.0.1
;; Keywords: languages tree-sitter
;; Homepage: https://github.com/merrickluo/kdl-ts-mode
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; KDL Document Language: https://kdl.dev/#overview
;; tree-sitter-kdl: https://github.com/tree-sitter-grammars/tree-sitter-kdl
;;
;;; Code:

(require 'treesit)
(require 'c-ts-common)
(eval-when-compile (require 'rx))

(if (>= emacs-major-version 31)
    (treesit-declare-unavailable-functions)
  (progn
    (declare-function treesit-parser-create "treesit.c")
    (declare-function treesit-node-child-by-field-name "treesit.c")
    (declare-function treesit-node-start "treesit.c")
    (declare-function treesit-node-type "treesit.c")
    (declare-function treesit-search-subtree "treesit.c")
    (declare-function treesit-filter-child "treesit.c")
    (declare-function treesit-node-text "treesit.c")))

(defgroup kdl nil
  "Major mode for editing KDL files."
  :group 'languages)

(defcustom kdl-ts-mode-indent-offset 4
  "Number of spaces for each indentation step in `kdl-ts-mode'."
  :version "29.1"
  :type 'integer
  :safe 'integerp
  :group 'kdl)

(defvar kdl-ts-mode--indent-rules
  `((kdl
     ((parent-is "document") column-0 0)
     ((node-is "}") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((parent-is "node") parent-bol kdl-ts-mode-indent-offset)
     ((parent-is "node_children") parent-bol kdl-ts-mode-indent-offset)
     ((parent-is "value") parent-bol kdl-ts-mode-indent-offset)
     ((parent-is "prop") parent-bol kdl-ts-mode-indent-offset)
     ((parent-is "type") parent-bol kdl-ts-mode-indent-offset)
     ((parent-is "multi_line_comment") prev-adaptive-prefix 0)
     (no-node parent-bol 0)))
  "Tree-sitter indent rules for `kdl-ts-mode'.")

(defvar kdl-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'kdl
   :feature 'comment
   '((single_line_comment) @font-lock-comment-face
     (multi_line_comment) @font-lock-comment-face
     ;; If a node contains node_comment, the entire node is a comment
     (node (node_comment)) @font-lock-comment-face)

   :language 'kdl
   :feature 'constant
   '(["true" "false" "null"] @font-lock-constant-face)

   :language 'kdl
   :feature 'number
   '((number) @font-lock-number-face)

   :language 'kdl
   :feature 'string
   '((string) @font-lock-string-face
     (escape) @font-lock-escape-face)

   :language 'kdl
   :feature 'type
   '((type) @font-lock-type-face
     (annotation_type) @font-lock-builtin-face)

   :language 'kdl
   :feature 'property
   '((prop (identifier) @font-lock-property-name-face)
     (value) @font-lock-variable-use-face)

   :language 'kdl
   :feature 'node
   '((node (identifier) @font-lock-function-name-face))

   :language 'kdl
   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

   :language 'kdl
   :feature 'operator
   '((["=" "-" "\\" "#"]) @font-lock-operator-face)

   :language 'kdl
   :feature 'error
   :override t
   '((ERROR) @font-lock-warning-face))
  "Tree-sitter font-lock settings for `kdl-ts-mode'.")

(defun kdl-ts-mode--defun-name (node)
  "Return the defun name of NODE.
Return nil if there is no name or if NODE is not a defun node."
  (pcase (treesit-node-type node)
    ("node"
     (when-let ((identifier (treesit-filter-child
                             node
                             (lambda (child) (string= (treesit-node-type child) "identifier")))))
       (treesit-node-text (car identifier) t)))))

(defun kdl-ts-mode--node-has-children-p (node)
  "Return t if NODE has children blocks."
  (and node
       (string-equal "node" (treesit-node-type node))
       (treesit-node-child-by-field-name node "children")))

(defvar-keymap kdl-ts-mode-map
  :doc "Keymap used in KDL mode, powered by tree-sitter"
  :parent prog-mode-map)

(defun kdl-ts-mode--beginning-of-node ()
  "Move to the beginning of the current KDL node."
  (interactive)
  (when-let ((node (kdl-ts-mode--node-at-point)))
    (goto-char (treesit-node-start node))))

(defun kdl-ts-mode--end-of-node ()
  "Move to the end of the current KDL node."
  (interactive)
  (when-let ((node (kdl-ts-mode--node-at-point)))
    (goto-char (treesit-node-end node))))

;; Add these to the keymap
(define-key kdl-ts-mode-map (kbd "C-c C-b") #'kdl-ts-mode--beginning-of-node)
(define-key kdl-ts-mode-map (kbd "C-c C-e") #'kdl-ts-mode--end-of-node)

;;;###autoload
(define-derived-mode kdl-ts-mode prog-mode "KDL"
  "Major mode for editing KDL, powered by tree-sitter.

\\{kdl-ts-mode-map}"
  :group 'kdl

  (when (treesit-ready-p 'kdl)
    (treesit-parser-create 'kdl)

    ;; Comments
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip (rx "//" (* (syntax whitespace))))

    ;; Navigation
    (setq-local treesit-defun-type-regexp
                (regexp-opt '("node")))
    (setq-local treesit-defun-name-function #'kdl-ts-mode--defun-name)

    ;; Indent
    (setq-local treesit-simple-indent-rules kdl-ts-mode--indent-rules)
    (setq-local indent-tabs-mode nil) ; KDL examples use spaces

    ;; Font-lock
    (setq-local treesit-font-lock-settings kdl-ts-mode--font-lock-settings)
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (node string type)
                  (constant number property)
                  (bracket operator error)))

    ;; Imenu
    (setq-local treesit-simple-imenu-settings
                '(("Node" "\\`node\\'" nil nil)))

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'kdl)
    (add-to-list 'auto-mode-alist '("\\.kdl\\'" . kdl-ts-mode)))

(provide 'kdl-ts-mode)
;;; kdl-ts-mode.el ends here
