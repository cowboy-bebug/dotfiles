;;; my-leetcode.el --- Org-mode workflow for leetcode.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(declare-function org-element-at-point "org-element")
(declare-function org-element-type "org-element")
(declare-function org-element-property "org-element")

;; leetcode.el bakes its GraphQL operation names in at load time via
;; `s-lower-camel-case', which breaks under Org's syntax table ("global-data"
;; instead of "globalData"), 400ing every request for the rest of the session.
;; This allows M-x leetcode from an org buffer.
(defmacro my/leetcode--defer-safely (cmd)
  "Shim CMD's autoload so leetcode.el is always loaded from a plain buffer."
  `(defun ,cmd (&rest args)
     (interactive)
     (unless (featurep 'leetcode)
       (with-temp-buffer (require 'leetcode)))
     (if (called-interactively-p 'any)
         (call-interactively #',cmd)
       (apply #',cmd args))))

(my/leetcode--defer-safely leetcode)
(my/leetcode--defer-safely leetcode-daily)
(my/leetcode--defer-safely leetcode-show-problem-by-slug)

(defun my/leetcode--code-buffers ()
  "Return the list of open `leetcode-solution-mode' buffers."
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (bound-and-true-p leetcode-solution-mode)))
              (buffer-list)))

(defun my/leetcode--read-code-buffer ()
  "Prompt for one of the open LeetCode solution buffers."
  (let ((buffers (my/leetcode--code-buffers)))
    (cond
     ((null buffers) (user-error "No open LeetCode solution buffer"))
     ((null (cdr buffers)) (car buffers))
     (t (get-buffer
         (completing-read "LeetCode buffer: " (mapcar #'buffer-name buffers) nil t))))))

;;;###autoload
(defun my/leetcode-org-insert-snippet ()
  "Insert an open LeetCode solution buffer's code as an Org src block at point."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (let* ((code-buf (my/leetcode--read-code-buffer))
         (lang (with-current-buffer code-buf
                 (string-remove-suffix "-mode" (symbol-name major-mode))))
         (code (with-current-buffer code-buf
                 (buffer-substring-no-properties (point-min) (point-max)))))
    (insert (format "#+begin_src %s\n%s%s#+end_src\n"
                    lang code (if (string-suffix-p "\n" code) "" "\n")))))

;;;###autoload
(defun my/leetcode-org-submit ()
  "Push the Org src block at point into its LeetCode solution buffer and submit."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not an Org buffer"))
  (let ((element (org-element-at-point)))
    (unless (eq (org-element-type element) 'src-block)
      (user-error "Point is not inside a src block"))
    (let ((code (org-element-property :value element))
          (code-buf (my/leetcode--read-code-buffer)))
      (with-current-buffer code-buf
        (erase-buffer)
        (insert code))
      ;; `leetcode-submit' reads `(current-buffer)' across an async await,
      ;; so the buffer must stay selected, not just dynamically current.
      (switch-to-buffer code-buf)
      (leetcode-submit))))

(provide 'my-leetcode)
;;; my-leetcode.el ends here
