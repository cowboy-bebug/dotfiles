;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; For more info, see `~/.config/emacs/templates/config.example.el'

;; ui
(setq doom-theme 'doom-oksolar-light
      doom-font                (font-spec :family "Hack Nerd Font"  :size 14)
      doom-variable-pitch-font (font-spec :family "Liberation Sans" :size 14)
      doom-modeline-vcs-max-length 40 ;; to display branch name
      display-line-numbers-type 'relative)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; recent files
(after! recentf
  (add-to-list 'recentf-exclude (recentf-expand-file-name "~/.config/emacs/.local/.*")))

;; spelling
(after! spell-fu
  (setq ispell-dictionary "en"
        spell-fu-word-delimit-camel-case t))

;; org
(after! org
  (add-to-list 'org-todo-keyword-faces '("GOAL" . "DarkOliveGreen3") t)
  (add-hook! 'org-mode-hook 'auto-fill-mode)
  (setq org-agenda-files '("~/github.com/cowboy-bebug/org/todo.org"
                           "~/github.com/cowboy-bebug/org/work/todo.org")
        org-directory "~/github.com/cowboy-bebug/org")
  (setq fill-column 80
        org-agenda-start-with-log-mode t
        org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . nil))
        org-hide-emphasis-markers t
        org-log-done 'time
        org-log-into-drawer t
        org-tags-column 80))

(after! org-roam
  (setq org-roam-directory "~/github.com/cowboy-bebug/org/roam"
        org-roam-completion-everywhere t
        org-roam-capture-templates
        `(("d" "default" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n#+date: %u\n#+filetags: :Draft:")
           :unnarrowed t)
          ("m" "moc" plain "%?"
           :if-new (file+head "moc/${slug}.org"
                              "#+title: ${title}\n#+date: %u")
           :immediate-finish t
           :unnarrowed t)
          ("b" "book journal" plain
           (file ,(expand-file-name "templates/book-journal.org"
                                    org-roam-directory))
           :if-new (file+head "book/${slug}.org"
                              "#+title: ${title}\n#+date: %u\n#+filetags: :Draft:")
           :immediate-finish t
           :unnarrowed t))
        org-roam-dailies-capture-templates
        '(("d" "default" entry "* %<%I:%M %p>: %?"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n")))
        org-roam-ui-follow t
        org-roam-ui-open-on-start nil
        org-roam-ui-sync-theme t
        org-roam-ui-update-on-save t))

;; magit
(after! magit
  (setq magit-log-section-commit-count 30))

;; formatter
(after! format-all
  (add-hook! 'markdown-mode-hook 'format-all-mode)
  (setq format-all-formatters
        '(("Markdown" (prettier "--proseWrap" "always")))))
