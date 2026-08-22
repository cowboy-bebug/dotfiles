;;; my-leetcode.el --- Org-mode workflow for leetcode.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

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

(provide 'my-leetcode)
;;; my-leetcode.el ends here
