;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; For more info, see `~/.config/emacs/templates/config.example.el'

;; ui
(setq doom-theme 'github-modern
      doom-font                (font-spec :family "Menlo" :size 14)
      doom-variable-pitch-font (font-spec :family "Georgia"        :size 20)
      doom-modeline-vcs-max-length 40 ;; to display branch name
      display-line-numbers-type 'relative)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(when (display-graphic-p)
  (when (member doom-theme '(doom-earl-grey doom-plain))
    (custom-set-faces! '(font-lock-keyword-face :weight bold)))

  (when (eq doom-theme 'doom-homage-white)
    (custom-set-faces! '(font-lock-type-face :slant italic)))

  (when (member doom-theme '(adwaita tango))
    (let ((current-line-background "#D6D6D6")
          (comment-foreground      "#9E9A95")
          (dired-dir-foreground    "#204A87"))
      (custom-theme-set-faces! 'adwaita
        '(hl-line                  :background "#DAE5f1")
        '(line-number-current-line :background "#DAE5f1")
        `(diredfl-dir-heading      :background nil)
        `(diredfl-dir-name         :background nil)
        `(diredfl-dir-priv         :background nil)
        '(diredfl-no-priv          :background nil)
        '(diredfl-exec-priv        :background nil)
        '(diredfl-read-priv        :background nil)
        '(diredfl-write-priv       :background nil))
      (custom-theme-set-faces! 'tango
        `(hl-line                  :background ,current-line-background)
        `(line-number-current-line :background ,current-line-background)
        `(diredfl-dir-heading      :background nil :foreground ,dired-dir-foreground :weight bold)
        `(diredfl-dir-name         :background nil :foreground ,dired-dir-foreground)
        `(diredfl-dir-priv         :background nil :foreground ,dired-dir-foreground)
        '(diredfl-no-priv          :background nil)
        '(diredfl-exec-priv        :background nil)
        '(diredfl-read-priv        :background nil)
        '(diredfl-write-priv       :background nil))
      (custom-set-faces!
        `(font-lock-comment-face :foreground ,comment-foreground)
        `(font-lock-doc-face     :foreground ,comment-foreground)))))

;; spelling
(after! spell-fu
  (setq ispell-dictionary "en"
        spell-fu-word-delimit-camel-case t))

;; org / markdown
(setq org-directory "~/org/")
(after! org
  (add-hook! 'org-mode-hook 'auto-fill-mode)
  (setq fill-column 80
        markdown-hide-markup t
        org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . nil))
        org-hide-emphasis-markers t))

;; magit
(after! magit
  (setq magit-log-section-commit-count 30))

;; formatter
(after! format-all
  (add-hook! 'markdown-mode-hook 'format-all-mode)
  (setq format-all-formatters
        '(("Markdown" (prettier "--proseWrap" "always")))))
