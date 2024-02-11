;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; For more info, see `~/.config/emacs/templates/config.example.el'

;; ui
(setq doom-theme 'tango
      doom-font                (font-spec :family "Hack Nerd Font" :size 12)
      doom-variable-pitch-font (font-spec :family "Georgia"        :size 20)
      doom-modeline-vcs-max-length 40 ;; to display branch name
      display-line-numbers-type 'relative)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(when (display-graphic-p)
  (when (eq doom-theme 'tango)
    (let ((current-line-background "#D6D6D6")
          (comment-foreground      "#9E9A95")
          (dired-dir-foreground    "#204A87"))
      (custom-theme-set-faces! 'tango
        `(hl-line                  :background ,current-line-background)
        `(line-number-current-line :background ,current-line-background)
        `(diredfl-dir-heading :background nil :foreground ,dired-dir-foreground :weight bold)
        `(diredfl-dir-name    :background nil :foreground ,dired-dir-foreground)
        `(diredfl-dir-priv    :background nil :foreground ,dired-dir-foreground)
        '(diredfl-no-priv     :background nil)
        '(diredfl-exec-priv   :background nil)
        '(diredfl-read-priv   :background nil)
        '(diredfl-write-priv  :background nil))
      (custom-set-faces!
        `(font-lock-comment-face :foreground ,comment-foreground)
        `(font-lock-doc-face     :foreground ,comment-foreground)))))

;; spelling
(after! spell-fu
  (setq ispell-dictionary "en"
        spell-fu-word-delimit-camel-case t))

;; org
(setq org-directory "~/org/")
(after! org
  (setq
   ;; org-edit-src-content-indentation nil
   org-hide-emphasis-markers t)
  )

;; preloaded packages
(after! magit
  :config (setq magit-log-section-commit-count 30))
