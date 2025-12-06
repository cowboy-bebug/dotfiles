;;; functions.el -*- lexical-binding: t; -*-

(defun +my/add-abbrev-word-constituents ()
  "Treat selected punctuation characters as part of words.
This is intended for better abbrev expansion that includes these symbols."
  (unless (derived-mode-p 'org-src-mode)
    (dolist (ch '(?! ?+ ?- ?. ?< ?= ?>))
      (modify-syntax-entry ch "w"))))

(defconst +my/denote-silo-directories
  '(:blogs "~/github.com/cowboy-bebug/org-work/blogs"
    :notes "~/github.com/cowboy-bebug/org-work/notes"
    :personal-blogs "~/github.com/cowboy-bebug/org/blogs"
    :personal-notes "~/github.com/cowboy-bebug/org/notes"
    :reading-notes "~/github.com/cowboy-bebug/org/reading")
  "Plist of directory paths used for denote-silo.")

(defun +my/denote-create-blog ()
  "Create a new blog using denote-silo."
  (interactive)
  (denote-silo-create-note
   (plist-get +my/denote-silo-directories :blogs)))

(defun +my/denote-create-note ()
  "Create a new note using denote-silo."
  (interactive)
  (denote-silo-create-note
   (plist-get +my/denote-silo-directories :notes)))

(defun +my/denote-create-personal-blog ()
  "Create a new personal blog using denote-silo."
  (interactive)
  (denote-silo-create-note
   (plist-get +my/denote-silo-directories :personal-blogs)))

(defun +my/denote-create-personal-note ()
  "Create a new personal note using denote-silo."
  (interactive)
  (denote-silo-create-note
   (plist-get +my/denote-silo-directories :personal-notes)))

(defun +my/denote-create-reading-note ()
  "Create a new reading note using denote-silo."
  (interactive)
  (denote-silo-create-note
   (plist-get +my/denote-silo-directories :reading-notes)))
