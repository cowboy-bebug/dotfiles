;;; my-hugo.el -*- lexical-binding: t; -*-

(defconst +my/hugo-content-dir
  "~/github.com/cowboy-bebug/ericlim.dev/content/posts"
  "Path to the root of your Hugo content directory.")

(defun +my/org-get-keyword (key)
  "Return first value of #+KEYWORD KEY in current Org buffer, or nil.
  KEY is a string like \"TITLE\" or \"DATE\"."
  (let* ((all (org-collect-keywords (list key)))
         (vals (cdr (assoc key all))))
    (and vals (car vals))))

;;;###autoload
(defun +my/org-export-to-hugo-md ()
  "Export current Org buffer to a Hugo ready Markdown file.

  Reads #+title, #+date, #+filetags and writes a file with the same
  basename as the Org file into `+my/hugo-content-dir` as YAML front matter
  plus Markdown body."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Run this in an org-mode buffer"))
  (unless (buffer-file-name)
    (user-error "Buffer is not visiting a file"))

  (let* ((org-file (buffer-file-name))
         (base     (file-name-base org-file))
         (out-dir  (file-name-as-directory +my/hugo-content-dir))
         (out-file (expand-file-name (concat base ".md") out-dir))

         ;; Keywords
         (title    (or (+my/org-get-keyword "TITLE") base))
         (raw-date (+my/org-get-keyword "DATE"))
         (time     (and raw-date (org-time-string-to-time raw-date)))
         (date-str (if time
                       (format-time-string "%Y-%m-%dT%H:%M:%S%:z" time)
                     (format-time-string "%Y-%m-%dT%H:%M:%S%:z")))
         (filetags (+my/org-get-keyword "FILETAGS"))
         (tags     (when filetags
                     (seq-filter
                      (lambda (s) (not (string-empty-p s)))
                      (split-string filetags ":" t))))

         ;; YAML front matter
         (yaml-front
          (concat
           "---\n"
           "title: " (prin1-to-string title) "\n"
           "date: " date-str "\n"
           "author: \"Eric Lim\"\n"
           (if tags
               (concat
                "tags:\n"
                (mapconcat (lambda (tag) (concat "- " tag)) tags "\n")
                "\n")
             "")
           "---\n\n"))

         ;; Body as markdown via Org export
         ;; ox-md is usually loaded by Doom org, but you can (require 'ox-md)
         ;; if you ever hit a backend not available error.
         (body (org-export-as 'md nil nil t nil)))

    (make-directory out-dir t)
    (with-temp-file out-file
      (insert yaml-front)
      (insert body))
    (message "Hugo markdown written to %s" out-file)))

(defun +my/blog-collect-org-files ()
  "Return a list of all blog org files from both blog directories."
  (cl-loop for dir in '("~/github.com/cowboy-bebug/org/blogs"
                        "~/github.com/cowboy-bebug/org-work/blogs")
           for full = (expand-file-name dir)
           when (file-directory-p full)
           append (directory-files full t "\\.org$")))

(defun +my/blog-copy-assets ()
  "Copy assets from both blog asset directories into Hugo content/assets."
  (let* ((dest (expand-file-name "assets" +my/hugo-content-dir)))
    (make-directory dest t)
    (dolist (src '("~/github.com/cowboy-bebug/org/blogs/assets"
                   "~/github.com/cowboy-bebug/org-work/blogs/assets"))
      (let ((srcdir (expand-file-name src)))
        (when (file-directory-p srcdir)
          ;; Merge directory contents
          (copy-directory srcdir dest nil t t))))))

;;;###autoload
(defun +my/export-blogs ()
  "Export every blog org file to Hugo style markdown."
  (interactive)
  (let ((files (+my/blog-collect-org-files)))
    (unless files
      (user-error "No .org files found in blog directories"))

    (dolist (file files)
      (message "Exporting %s" file)
      (with-current-buffer (find-file-noselect file)
        ;; Ensure org-mode for export
        (org-mode)
        (+my/org-export-to-hugo-md)))

    (+my/blog-copy-assets)

    (message "Blog export complete: %d files processed" (length files))))

(provide 'my-hugo)
