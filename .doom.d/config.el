;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Bum Kim"
      user-mail-address "bumcrystlbum@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "Roboto Mono" :size 16))
(setq doom-font "Roboto Mono-12:width=condensed")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-solarized-dark)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Sync/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; my configurations

;; org-mode
(custom-theme-set-faces! 'user
  `(org-level-1 :font "Roboto" :height 1.5 :weight bold)
  `(org-level-2 :font "Roboto" :height 1.25 :weight bold)
  `(org-level-3 :font "Roboto" :height 1.15 :weight bold)
  `(org-level-4 :font "Roboto" :height 1.1 :weight bold)
  `(org-level-5 :font "Roboto" :weight bold)
  `(org-level-6 :font "Roboto" :weight bold)
  `(org-level-7 :font "Roboto" :weight bold)
  `(org-level-8 :font "Roboto" :weight bold)
  `(org-document-title :foreground ,(face-foreground 'default) :height 1.3 :underline nil))

;; keywords
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; org-agenda
(setq org-agenda-files (list "~/Sync/org/tasks/tasks.org"
                             "~/Sync/org/projects/ideas.org"
                             "~/Sync/org/projects/projects.org"))

;; org-refile
;; Show full paths for refiling
(setq org-refile-use-outline-path t)

;; org-capture
;; basic task template
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Sync/org/tasks/tasks.org" "Tasks")
         "* TODO %?\n  %i\n")))

;; disable line numbers in writeroom-mode
(add-hook 'writeroom-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'writeroom-mode-hook (lambda () (hl-line-mode 0)))

(setq org-hide-emphasis-markers t)

;; org journal
;; default journal directory
(customize-set-variable 'org-journal-dir "~/Sync/org/journal/")
;; buffer-specific header format
(customize-set-variable 'org-journal-date-format "%A, %B %d %Y")
(customize-set-variable 'org-journal-time-format "%I:%M %P")
;; file title format
(customize-set-variable 'org-journal-file-format "%Y-%m-%d")
;; don't open new window for editing entry
(customize-set-variable 'org-journal-find-file 'find-file)
;; enable journal file encryption
(setq org-journal-encrypt-journal t)
;; start journals in writeroom-mode
(add-hook 'org-journal-mode-hook (lambda () (writeroom-mode t)))
(require 'org-journal)
