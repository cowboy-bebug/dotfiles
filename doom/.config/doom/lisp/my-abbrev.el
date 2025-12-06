;;; my-abbrev.el --- Helpers for abbrev -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun +my/add-abbrev-word-constituents ()
  "Treat selected punctuation characters as part of words.
This is intended for better abbrev expansion that includes these symbols."
  (unless (derived-mode-p 'org-src-mode)
    (dolist (ch '(?! ?+ ?- ?. ?< ?= ?>))
      (modify-syntax-entry ch "w"))))

(provide 'my-abbrev)
;;; my-abbrev.el ends here
