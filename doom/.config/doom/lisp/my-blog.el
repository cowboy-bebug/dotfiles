;;; my-blog.el --- Helpers for exporting blogs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org-element)
(require 'my-denote)

(defconst +my/blog-content-dir
  "~/github.com/cowboy-bebug/ericlim.dev/content/posts"
  "Root directory for blog posts.")

(defun +my/org-get-keyword (key)
  "Return the first value of Org KEYWORD KEY in the current buffer, or nil.
KEY should be a string such as \"TITLE\" or \"DATE\"."
  (let ((kw-alist (org-collect-keywords (list key))))
    (car (alist-get key kw-alist nil nil #'string=))))

;;;###autoload
(defun +my/org-export-blog ()
  "Export the current Org buffer to a Markdown file.
Extracts #+TITLE, #+DATE and #+FILETAGS, then writes a Markdown
file with YAML front matter into `+my/blog-content-dir`."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))

  (let* ((org-file   (buffer-file-name))
         (base       (file-name-base org-file))
         (clean-base (replace-regexp-in-string "__.*$" "" base))
         (out-dir    (file-name-as-directory +my/blog-content-dir))
         (out-file   (expand-file-name (concat clean-base ".md") out-dir))

         ;; Extract Org metadata
         (title    (+my/org-get-keyword "TITLE"))
         (raw-date (+my/org-get-keyword "DATE"))
         (filetags (+my/org-get-keyword "FILETAGS")))

    ;; Validate metadata
    (unless title (user-error "Missing #+TITLE keyword in this Org buffer"))
    (unless raw-date (user-error "Missing #+DATE keyword in this Org buffer"))
    (unless filetags (user-error "Missing #+FILETAGS keyword in this Org buffer"))

    ;; Derived values
    (let* ((time     (and raw-date (org-time-string-to-time raw-date)))
           (date-str (format-time-string "%Y-%m-%dT%H:%M:%S%:z" time))
           (tags     (when filetags
                       (seq-remove #'string-empty-p
                                   (split-string filetags ":" t))))
           (tags-block
            (if tags
                (format "tags: [%s]\n"
                        (mapconcat #'identity tags ", "))
              ""))

           ;; YAML header
           (yaml-front
            (format "---\ntitle: %s\ndate: %s\n%s---\n\n"
                    (prin1-to-string title)
                    date-str
                    tags-block))

           ;; Org -> Markdown body
           (body (org-export-as 'md nil nil t
                                '(:auto-id nil
                                  :headeline-anchors nil
                                  :with-toc nil
                                  :section-numbers nil
                                  :broken-links mark))))

      ;; Write output file
      (make-directory out-dir t)
      (with-temp-file out-file
        (insert yaml-front)
        (insert body))
      (message "Markdown written to %s" out-file))))

(defconst +my/blog-directories
  (list (plist-get +my/denote-silo-directories :blogs)
        (plist-get +my/denote-silo-directories :personal-blogs))
  "List of denote blog directories.")

(defun +my/blog-collect-org-files ()
  "Return a list of all Org files in `+my/blog-directories`."
  (seq-mapcat
   (lambda (dir)
     (let ((full (expand-file-name dir)))
       (when (file-directory-p full)
         (directory-files full t (rx ".org" eos)))))
   +my/blog-directories))

(defconst +my/blog-asset-directories
  (seq-map
   (lambda (dir)
     (expand-file-name "assets" dir))
   (list (plist-get +my/denote-silo-directories :blogs)
         (plist-get +my/denote-silo-directories :personal-blogs)))
  "List of asset directories.")

(defun +my/blog-copy-assets ()
  "Copy all blog asset directories into content/assets directory.
The contents of each asset directory listed in `+my/blog-asset-directories`
are merged into the destination directory."
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
    (unless files
      (user-error "No Org files found in blog directories"))

    (let ((i 1)
          (total (length files)))
      (dolist (file files)
        (message "[%d/%d] Exporting %s" i total file)
        (let ((buf (find-file-noselect file)))
          (unwind-protect
              (with-current-buffer buf
                (save-excursion
                  (save-restriction
                    (widen)
                    (unless (derived-mode-p 'org-mode)
                      (org-mode))
                    (+my/org-export-blog))))
            (kill-buffer buf)))
        (cl-incf i)))

    (+my/blog-copy-assets)
    (message "Blog export complete: %d files processed" (length files))))

(provide 'my-blog)
;;; my-blog.el ends here
