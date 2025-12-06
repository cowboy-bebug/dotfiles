;;; my-denote.el --- Helpers for denote -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defconst +my/denote-silo-directories
  '(:blogs          "~/github.com/cowboy-bebug/org-work/blogs"
    :notes          "~/github.com/cowboy-bebug/org-work/notes"
    :personal-blogs "~/github.com/cowboy-bebug/org/blogs"
    :personal-notes "~/github.com/cowboy-bebug/org/notes"
    :reading-notes  "~/github.com/cowboy-bebug/org/reading")
  "Plist mapping silo types to their directories.")

(defun +my/denote-silo-create-note (type)
  "Create a denote note of TYPE using `+my/denote-silo-directories`."
  (denote-silo-create-note
   (plist-get +my/denote-silo-directories type)))

;;;###autoload
(defun +my/denote-create-blog ()
  "Create a new blog using denote-silo."
  (interactive) (+my/denote-silo-create-note :blogs))

;;;###autoload
(defun +my/denote-create-note ()
  "Create a new note using denote-silo."
  (interactive) (+my/denote-silo-create-note :notes))

;;;###autoload
(defun +my/denote-create-personal-blog ()
  "Create a new personal blog using denote-silo."
  (interactive) (+my/denote-silo-create-note :personal-blogs))

;;;###autoload
(defun +my/denote-create-personal-note ()
  "Create a new personal note using denote-silo."
  (interactive) (+my/denote-silo-create-note :personal-notes))

;;;###autoload
(defun +my/denote-create-reading-note ()
  "Create a new reading note using denote-silo."
  (interactive) (+my/denote-silo-create-note :reading-notes))

(provide 'my-denote)
;;; my-denote.el ends here
