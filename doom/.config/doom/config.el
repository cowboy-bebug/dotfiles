(setq doom-theme 'doom-one)

(setq doom-font
      (font-spec
       :family "Roboto Mono"
       :size 14
       :weight 'medium))
(setq doom-variable-pitch-font
      (font-spec
       :family "Georgia"
       :size 20))

(setq display-line-numbers-type 'relative)

(setq org-directory "~/github.com/cowboy-bebug/org")
(after! org
  (setq org-edit-src-content-indentation nil)
  (setq org-hide-emphasis-markers t)
  (use-package! org-superstar
    :hook
    (org-mode . org-superstar-mode))
  (use-package! org-auto-tangle
    :defer t
    :config
    (setq org-auto-tangle-babel-safelist '("~/github.com/cowboy-bebug/dotfiles/README.org"))
    :hook
    (org-mode . org-auto-tangle-mode)))

(after! magit
  :config
  (setq magit-log-section-commit-count 30))

(setq auto-mode-alist
      (append '(("\\.mdx\\'" . markdown-mode))
              auto-mode-alist))

(after! flyspell
  :config
  (let ((aspell-personal-directory
         (string-trim-right
          (shell-command-to-string "aspell config home-dir"))))
    (setq ispell-personal-dictionary
          (expand-file-name ".aspell.en.pws"  aspell-personal-directory))))

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode))

(after! elfeed
  :config
  (setq elfeed-search-filter "@1-month-ago +unread")
  (setq elfeed-feeds
        '(("https://feeds.feedburner.com/TheDailyWtf" programming)
          ("https://lobste.rs/rss" programming)
          ("https://www.news.ycombinator.com/rss" programming)
          ("https://www.reddit.com/r/programming/.rss" programming)
          ("https://techcrunch.com/feed" new tech)))
  (setq browse-url-browser-function 'eww-browse-url)
  (set-popup-rules!
    '(("^\\*eww\\*"
       :side right
       :slot 1
       :size #'+popup-shrink-to-fit
       :select t))))
