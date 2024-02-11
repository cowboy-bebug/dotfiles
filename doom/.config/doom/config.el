;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; For more info, see `~/.config/emacs/templates/config.example.el'

;; ui
(setq doom-theme 'doom-earl-grey
      doom-font (font-spec :family "Hack Nerd Font" :size 12)
      doom-variable-pitch-font (font-spec :family "Georgia" :size 20)
      doom-modeline-vcs-max-length 40 ;; to display branch name
      display-line-numbers-type 'relative)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

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
