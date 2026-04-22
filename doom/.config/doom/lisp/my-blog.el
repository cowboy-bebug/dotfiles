;;; my-blog.el --- Helpers for exporting blogs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org-element)

(defconst +my/blog-content-dir
  "~/github.com/cowboy-bebug/ericlim.dev/content/posts"
  "Root directory for blog posts.")

(defconst +my/denote-directory
  "~/github.com/cowboy-bebug/org/notes"
  "The single folder where all Denote files reside.")

(defun +my/org-get-keyword (key)
  "Return the first value of Org KEYWORD KEY in the current buffer, or nil."
  (let ((kw-alist (org-collect-keywords (list key))))
    (car (alist-get key kw-alist nil nil #'string=))))

;;;###autoload
(defun +my/org-export-blog ()
  "Export the current Org buffer to a Markdown file."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))

  (let* ((org-file   (buffer-file-name))
         (base       (file-name-base org-file))
         ;; Removes the ID and tags from the filename for a clean slug
         (clean-base (replace-regexp-in-string "__.*$" "" base))
         (out-dir    (file-name-as-directory +my/blog-content-dir))
         (out-file   (expand-file-name (concat clean-base ".md") out-dir))
         (title    (+my/org-get-keyword "TITLE"))
         (raw-date (+my/org-get-keyword "DATE"))
         (filetags (+my/org-get-keyword "FILETAGS")))

    (unless title (user-error "Missing #+TITLE keyword"))
    (unless raw-date (user-error "Missing #+DATE keyword"))
    (unless filetags (user-error "Missing #+FILETAGS keyword"))

    (let* ((time     (and raw-date (org-time-string-to-time raw-date)))
           (date-str (format-time-string "%Y-%m-%dT%H:%M:%S%:z" time))
           (tags     (when filetags
                       (seq-remove #'string-empty-p
                                   (split-string filetags ":" t))))
           (tags-block (if tags (format "tags: [%s]\n" (mapconcat #'identity tags ", ")) ""))
           (yaml-front (format "---\ntitle: %s\ndate: %s\n%s---\n\n"
                               (prin1-to-string title) date-str tags-block))
           (body (org-export-as 'md nil nil t
                                '(:auto-id nil :headeline-anchors nil :with-toc nil :section-numbers nil :broken-links mark))))

      (make-directory out-dir t)
      (with-temp-file out-file
        (insert yaml-front)
        (insert body))
      (message "Markdown written to %s" out-file))))

(defconst +my/blog-directories
  (list +my/denote-directory))

(defun +my/blog-collect-org-files ()
  "Return a list of all Org files in `+my/blog-directories` that have the _blog tag."
  (seq-mapcat
   (lambda (dir)
     (let ((full (expand-file-name dir)))
       (when (file-directory-p full)
         ;; Updated regex: Matches filenames containing '_blog' ending in '.org'
         (directory-files full t (rx "_blog" (0+ any) ".org" eos)))))
   +my/blog-directories))

(defconst +my/blog-asset-directories
  (list (expand-file-name "assets" +my/denote-directory)))

(defun +my/blog-copy-assets ()
  "Copy the consolidated asset directory into content/assets directory."
  (let* ((dest (expand-file-name "assets" +my/blog-content-dir)))
    (make-directory dest t)
    (dolist (srcdir +my/blog-asset-directories)
      (when (file-directory-p srcdir)
        (copy-directory srcdir dest nil t t)))
    (message "Assets copied to %s" dest)))

;;;###autoload
(defun +my/export-blogs ()
  "Export all blog Org files to markdown."
  (interactive)
  (let ((files (+my/blog-collect-org-files)))
    (if (not files)
        (message "No files with '_blog' tag found in %s" +my/denote-directory)
      (let ((i 1) (total (length files)))
        (dolist (file files)
          (message "[%d/%d] Exporting %s" i total file)
          (let ((buf (find-file-noselect file)))
            (unwind-protect
                (with-current-buffer buf
                  (save-excursion
                    (save-restriction
                      (widen)
                      (unless (derived-mode-p 'org-mode) (org-mode))
                      (+my/org-export-blog))))
              (kill-buffer buf)))
          (cl-incf i)))
      (+my/blog-copy-assets)
      (message "Blog export complete: %d files processed" (length files)))))

(provide 'my-blog)
;;; my-blog.el ends here
