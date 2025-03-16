;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; For more info, see `~/.config/emacs/templates/config.example.el'

;; ui
(load! "theme")
(setq doom-font                (font-spec :family "Hack Nerd Font"  :size 14)
      doom-variable-pitch-font (font-spec :family "Liberation Sans" :size 14)
      doom-modeline-vcs-max-length 40 ;; to display branch name
      display-line-numbers-type 'relative)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
(add-to-list 'default-frame-alist '(undecorated-round . t))

;; project sidebar
(after! treemacs
  (map! :leader
        (:prefix-map ("o" . "open")
         :desc "Project sidebar" "p" #'treemacs)))

;; recent files
(after! recentf
  (add-to-list 'recentf-exclude (recentf-expand-file-name "~/.config/emacs/.local/.*")))

;; spelling
(after! spell-fu
  ;; Initialise personal dictionary, if not exists
  (let ((personal-dictionary (expand-file-name "ispell/.pws" doom-data-dir)))
    (unless (file-exists-p personal-dictionary)
      (make-directory (file-name-directory personal-dictionary) t)
      (write-region "personal_ws-1.1 en 0\n" nil personal-dictionary))
    (setq ispell-dictionary "en"
          ispell-personal-dictionary personal-dictionary
          spell-fu-word-delimit-camel-case t)))

;; org
(after! org
  (add-to-list 'org-modules 'org-habit t)
  (add-to-list 'org-todo-keyword-faces '("GOAL" . "DarkOliveGreen3") t)
  (add-hook! 'org-mode-hook 'auto-fill-mode)
  (add-hook! 'org-mode-hook 'org-fragtog-mode)
  (add-hook! 'org-mode-hook #'org-modern-mode)
  (add-hook! 'before-save-hook 'doom/delete-trailing-newlines)
  (add-hook! 'before-save-hook 'delete-trailing-whitespace)

  (map! :leader
        (:prefix-map ("t" . "toggle")
         :desc "Present mode" "p" #'org-present))

  (setq org-agenda-files '("~/github.com/cowboy-bebug/org/todo.org"
                           "~/github.com/cowboy-bebug/org-work/20241104_*_onit/todo.org")
        org-babel-default-header-args (cons '(:results . "replace verbatim")
                                            (assq-delete-all :results org-babel-default-header-args))
        org-babel-results-keyword "results"
        org-edit-src-content-indentation 0
        org-id-locations-file "~/.config/emacs/.local/cache/.org-id-locations"
        org-directory "~/github.com/cowboy-bebug/org"
        fill-column 80
        org-agenda-start-with-log-mode t
        org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . nil))
        org-hide-emphasis-markers t
        org-log-done 'time
        org-log-into-drawer t
        org-pretty-entities t
        org-src-preserve-indentation nil
        org-startup-with-inline-images t
        org-startup-with-latex-preview t
        org-tags-column 80))

(after! org-habit
  (setq org-habit-show-habits-only-for-today nil
        org-habit-today-glyph ?◦
        org-habit-completed-glyph ?•))

(after! org-modern
  (setq org-modern-block-name '((t . ("▶ " "▶ "))
                                ("src" . ("󰞷" "󰞷")))
        org-modern-checkbox nil
        org-modern-horizontal-rule t
        org-modern-keyword t
        org-modern-list '((?+ . "➤")
                          (?- . "•")
                          (?* . "◦"))
        org-modern-priority nil
        org-modern-progress nil
        org-modern-table nil
        org-modern-tag nil
        org-modern-timestamp nil
        org-modern-todo nil

        org-modern-star 'replace
        org-modern-replace-stars "●◉◎○"
        org-modern-fold-stars '(("▶" . "▼"))))

(after! org-present
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-display-inline-images)
              (org-present-read-only)
              (org-present-hide-cursor)
              (writeroom-mode 1)
              (display-line-numbers-mode 0)))
  (add-hook 'org-present-mode-quit-hook
            (lambda()
              (org-remove-inline-images)
              (org-present-read-write)
              (org-present-show-cursor)
              (visual-fill-column-mode 0)
              (writeroom-mode 0)
              (display-line-numbers-mode 1)))
  (setq org-present-hide-stars-in-headings nil
        org-present-one-big-page nil
        org-present-startup-folded t))

(after! org-roam
  (setq org-roam-directory "~/github.com/cowboy-bebug/org-work/roam"
        org-roam-completion-everywhere t
        org-roam-capture-templates
        `(("d" "default" plain "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n#+date: %u\n#+filetags: :Draft:")
           :unnarrowed t)
          ("m" "moc" plain "%?"
           :if-new (file+head "moc/${slug}.org"
                              "#+title: ${title}\n#+date: %u")
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

;; denote
(after! denote
  (setq denote-directory "~/github.com/cowboy-bebug/org-work/notes"))

(after! denote-silo-extras
  (setq denote-silo-extras-directories
        '("~/github.com/cowboy-bebug/org-work/notes"
          "~/github.com/cowboy-bebug/org/notes")))

;; elfeed
(after! elfeed
  (setq elfeed-search-filter "@1-month-ago"))

;; magit
(after! magit
  (setq magit-log-section-commit-count 30))

;; formatter
(after! apheleia
  (add-hook! 'before-save-hook 'doom/delete-trailing-newlines)
  (add-hook! 'before-save-hook 'delete-trailing-whitespace)
  (add-hook! 'json-mode-hook (setq js-indent-level 2))
  (add-hook! 'typescript-mode-hook (setq typescript-indent-level 2))
  (setf (alist-get 'prettier-markdown apheleia-formatters)
        '("prettier"
          "--parser" "markdown"
          "--prose-wrap" "always"
          "--embedded-language-formatting=auto"))
  (add-to-list 'apheleia-mode-alist '(gfm-mode . prettier-markdown))
  (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier-markdown)))

;; language
(add-to-list 'auto-mode-alist '("\\.jsonc\\'" . jsonc-mode))
(after! lsp-mode
  (setq lsp-rust-analyzer-binding-mode-hints nil
        lsp-rust-analyzer-display-parameter-hints nil
        lsp-rust-analyzer-closure-capture-hints nil))
