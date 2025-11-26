;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;;; For more info, see `~/.config/emacs/templates/config.example.el'

;; load things
(load! "functions")
(load! "theme")

;; ui
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

;; authentication
(after! auth-source
  (setq auth-sources '("~/github.com/cowboy-bebug/dotfiles-secrets/.authinfo.gpg")))

;; abbrev
(define-abbrev-table 'global-abbrev-table
  '(("--" "—")
    ("->" "→")
    ("<-" "←")
    ("<->" "⟷")
    ("<=" "⇐")
    ("=>" "⇒")
    ("<=>" "⇔")
    ("!=" "≠")
    ("+-" "±")
    ("<=" "≤")
    ("==" "≡")
    (">=" "≥")))
(setq save-abbrevs nil)

;; markdown
(after! markdown-mode
  (add-hook! 'markdown-mode-hook 'abbrev-mode)
  (add-hook! 'markdown-mode-hook '+my/make-word-constituents))

;; org
(after! org
  (add-to-list 'org-capture-templates
               '("b" "Blog Note" entry
                 (file+headline "~/github.com/cowboy-bebug/org-work/notes.org" "Blogs")
                 "\n* %(org-read-date nil nil nil) %^{Title}  :blog:\n:source: %^{URL}\n:read_on: %U\n\n%?"))
  (add-to-list 'org-capture-templates
               '("v" "Video Note" entry
                 (file+headline "~/github.com/cowboy-bebug/org-work/notes.org" "Videos")
                 "\n* %(org-read-date nil nil nil) %^{Title}  :video:\n:source: %^{URL}\n:watched_on: %U\n\n%?"))
  (add-to-list 'org-modules 'org-habit t)
  (add-to-list 'org-todo-keyword-faces '("GOAL" . "DarkOliveGreen3") t)
  (add-hook! 'org-mode-hook 'abbrev-mode)
  (add-hook! 'org-mode-hook 'auto-fill-mode)
  (add-hook! 'org-mode-hook 'org-fragtog-mode)
  (add-hook! 'org-mode-hook 'org-modern-mode)
  (add-hook! 'org-mode-hook '+my/make-word-constituents)
  (add-hook! 'org-mode-hook (setq-local fill-column 80))
  (add-hook! 'org-mode-hook (visual-line-mode -1))
  (add-hook! 'before-save-hook 'doom/delete-trailing-newlines)
  (add-hook! 'before-save-hook 'delete-trailing-whitespace)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((mermaid . t)
     (typescript . t)))

  (map! :leader
        (:prefix-map ("t" . "toggle")
         :desc "Present mode" "p" #'org-present))

  (setq org-babel-default-header-args (cons '(:results . "replace verbatim")
                                            (assq-delete-all :results org-babel-default-header-args))
        org-babel-results-keyword "results"
        org-edit-src-content-indentation 0
        org-id-locations-file "~/.config/emacs/.local/cache/.org-id-locations"
        org-directory "~/github.com/cowboy-bebug/org"
        org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . nil))
        org-hide-emphasis-markers t
        org-log-done 'time
        org-log-into-drawer t
        org-pretty-entities t
        org-src-preserve-indentation nil
        org-startup-folded nil
        org-startup-with-inline-images t
        org-startup-with-latex-preview t
        org-tags-column 80))

(after! org-agenda
  (setq org-agenda-files '("~/github.com/cowboy-bebug/org/todo.org"
                           "~/github.com/cowboy-bebug/org-work/todo.org")
        org-agenda-start-with-log-mode t))

(after! org-habit
  (setq org-habit-show-habits-only-for-today nil
        org-habit-today-glyph ?◦
        org-habit-completed-glyph ?•))

(after! org-journal
  (setq org-journal-dir "~/github.com/cowboy-bebug/org-work/logs/"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-file-type 'weekly))

(after! ob-mermaid
  (setq ob-mermaid-cli-path "npx @mermaid-js/mermaid-cli"))

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

(after! denote-silo
  (setq denote-silo-directories
        '("~/github.com/cowboy-bebug/org-work/notes"
          "~/github.com/cowboy-bebug/org/notes")))

;; elfeed
(after! elfeed
  (setq elfeed-search-filter "@1-month-ago"))

;; eww
(after! eww
  (set-popup-rule! "^\\*eww.*" :side 'right :size 0.5))

;; gptel
(after! gptel
  (setq gptel-directives
        '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
          (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
          (writing . "You are a large language model and a writing assistant. Respond concisely.")
          (chat . "You are a large language model and a conversation partner. Respond concisely.")

          (staff-engineer
           . "You are a Staff Engineer. Adapt your responses based on the request:
- For architecture/design discussions: focus on scalability, reliability, maintainability, and trade-offs. Offer concise, actionable feedback.
- For incident analysis: identify root causes, contributing factors, and prevention steps clearly.
- For mentorship: give clear, supportive, and actionable guidance while encouraging independent thinking.
- For decision-making: summarize context, options, decision, and rationale concisely.
- For code review: identify issues, suggest improvements, and highlight best practices constructively.
- For trade-off analysis: compare technical options by performance, complexity, cost, scalability, and long-term maintainability.
- For strategy: align recommendations with business goals, scalability, and adaptability over 3–5 years.
Always be concise, pragmatic, and professional.")

          (architecture-review . "You are an experienced Staff Engineer. Review technical designs with a focus on scalability, reliability, maintainability, and trade-offs. Offer concise, actionable feedback.")
          (code-review . "You are a Staff Engineer performing a code review. Identify issues, suggest improvements, and highlight best practices. Be specific, constructive, and concise.")
          (decision-record . "You are a senior engineer documenting an Architecture Decision Record (ADR). Summarize the context, options, decision, and rationale in clear, professional language.")
          (incident-analysis . "You are a senior engineer specializing in incident retrospectives. Summarize root causes, contributing factors, and actionable prevention steps clearly.")
          (mentorship . "You are a Staff Engineer mentoring a mid-level developer. Offer clear, supportive, and actionable guidance while encouraging independent thinking.")
          (tech-strategy . "You are a Staff Engineer advising on long-term technical strategy. Focus on alignment with business goals, scalability, and adaptability over 3–5 years.")
          (tradeoff-analysis . "You are a senior engineer explaining trade-offs. Compare technical options by performance, complexity, cost, scalability, and long-term maintainability.")
          )))

;; magit
(after! magit
  (setq magit-log-section-commit-count 30))

;; formatter
(after! apheleia
  (add-hook! 'before-save-hook 'doom/delete-trailing-newlines)
  (add-hook! 'before-save-hook 'delete-trailing-whitespace)
  (add-hook! 'json-mode-hook (setq js-indent-level 2))
  (add-hook! 'typescript-mode-hook (setq typescript-indent-level 2))
  (add-hook! 'sh-mode-hook (setq sh-basic-offset 2))
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

(after! sql
  (setq sql-mysql-program "/opt/homebrew/opt/mysql-client/bin/mysql"))
