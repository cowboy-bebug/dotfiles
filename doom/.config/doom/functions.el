;;; functions.el -*- lexical-binding: t; -*-

(defun +my/make-word-constituents ()
  "Make specific symbols into word constituents so they can be used in abbrevs."
  (modify-syntax-entry ?! "w")
  (modify-syntax-entry ?+ "w")
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?. "w")
  (modify-syntax-entry ?< "w")
  (modify-syntax-entry ?= "w")
  (modify-syntax-entry ?> "w"))

(defconst +my/denote-silo-directories
  '(:blogs "~/github.com/cowboy-bebug/org-work/blogs"
    :notes "~/github.com/cowboy-bebug/org-work/notes"
    :personal-notes "~/github.com/cowboy-bebug/org/notes")
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

(defun +my/denote-create-personal-note ()
  "Create a new personal note using denote-silo."
  (interactive)
  (denote-silo-create-note
   (plist-get +my/denote-silo-directories :personal-notes)))
