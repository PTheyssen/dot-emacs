;;; init.el  -*- lexical-binding: t; -*-

;; First set the package-archives URLs

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(eval-when-compile
  (setq package-archives
	'(("gnu" . "http://elpa.gnu.org/packages/")
	  ("nongnu" . "http://elpa.nongnu.org/nongnu/")
	  ("melpa" . "http://melpa.org/packages/")))
  (require 'use-package))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; Precompute activation actions to speed up startup
;; (setq package-quickstart t)

;; fix warnings: "cl package deprecated
(setq byte-compile-warnings '(cl-functions))

;; TODO: Load my customization file, for my own functions etc.
;; (setq custom-file "/home/philipp/.emacs.d/emacs-custom.el")
;; (load custom-file)


;; Load my favorite theme
;; (load-theme 'doom-nord t)
(load-theme 'leuven t)
;; (load-theme 'zenburn t)
;; (load-theme 'doom-moonlight t)

;; Start server to use emacsclient
;; (server-start)

;; ensure same path in emacs as in shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


;; Who I am
(setq user-full-name "Philipp")
(setq user-mail-address "p.theyssen@gmail.com")

;; Let's get a backtrace when errors are
;; (setq debug-on-error t)

;; Display byte-compiler warnings on error
(setq byte-compile-debug t)


;; Stop polluting the directory with auto-saved files and backup
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq auto-save-list-file-prefix nil)


;; Always use "y" for "yes"
(fset 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;; my own config


;; initial buffer choice
;; (setq initial-buffer-choice "~/Nextcloud/org/gtd.org")

;; set font size
(set-face-attribute 'default nil :height 120)
;; (set-face-attribute 'default nil :height 80)

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; hide menu-bar and tool-bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(show-paren-mode 1)
(auto-insert-mode 1)
(display-time-mode 1)
(tooltip-mode -1)
(blink-cursor-mode -1)
(scroll-bar-mode -1)
(pixel-scroll-mode 1)


;; configure modeline
(display-time-mode -1)


;; Stop polluting the directory with auto-saved files and backup
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq auto-save-list-file-prefix nil)

;; do not show line numbers
(global-linum-mode -1)

;; no bell
(setq ring-bell-function 'ignore)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

(setq org-return-follows-link t)

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t
      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t
      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t
      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t
      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)


;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)


;; function for toggle comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; Trigger completion on Shift-Space
(global-set-key (kbd "S-SPC") #'company-complete)

;; keymaps
;; Unset C-z which is bound to `suspend-frame' by default
(global-unset-key (kbd "C-z"))
(global-set-key "\M-k" (lambda () (interactive) (kill-line 0)) ) ;M-k kills to the left
(global-set-key (kbd "C-c r") 'query-replace-regexp)
(global-set-key (kbd "C-c e") 'eww)
(global-set-key (kbd "C-c l") 'org-cliplink)
(global-set-key (kbd "C-c d") 'dictcc)
(global-set-key (kbd "C-x p") 'dictcc-at-point)
(global-set-key (kbd "C-.") 'toggle-comment-on-line)
(global-set-key (kbd "M-o") 'mode-line-other-buffer)
(global-set-key (kbd "C-ö") 'ido-next-match)
(global-set-key (kbd "C-ä") 'ido-prev-match)
(global-set-key (kbd "M-t") 'transpose-lines)
(global-set-key (kbd "C-#") 'fixup-whitespace)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
(global-set-key (kbd "C-c v") 'visual-line-mode)
(global-set-key (kbd "C-c u") 'unipoint-insert)
(global-set-key (kbd "C-c C-r") 'fill-region)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c c") 'org-capture)
;; (global-set-key (kbd "C-h") 'delete-backward-char)
;; (global-set-key (kbd "M-h") 'backward-kill-word)
;; (global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "C-c s") 'ispell)
(global-set-key (kbd "<f12>") 'terminal-here)
(global-set-key (kbd "<f5>") 'recompile)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c p") 'projectile-command-map)
(setq scroll-preserve-screen-position 1)
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x c") 'helm-command-prefix-key)
(define-key global-map (kbd "C-c SPC") 'ace-jump-char-mode)
(global-set-key (kbd "M-q") 'ace-window)
;; (global-set-key (kbd "<f11>") 'shell)
(global-set-key (kbd "<f11>") 'deft)
(global-set-key (kbd "<f7>") 'calendar)
(global-set-key (kbd "<f9>") 'delete-trailing-whitespace)
(global-set-key (kbd "<f10>") 'org-agenda)
(global-set-key (kbd "<f8>") 'deft)
(global-set-key (kbd "C-c b") 'open-org)
(global-set-key (kbd "C-c z") 'open-zathura)
(global-set-key (kbd "C-c g") 'open-gimp)
(global-set-key (kbd "M-j")
            (lambda ()
                  (interactive)
                  (join-line -1)))



;; dired mode key bindings https://stackoverflow.com/questions/24567313/remap-key-bindings-for-dired-mode
(global-set-key (kbd "<dead-circumflex>") 'dired-up-directory)

;; Move more quickly
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

(defun open-org ()
  (interactive)
  (find-file "/home/pt/Nextcloud/org/gtd.org"))

;;;; package
(use-package smex)
(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))
(use-package ido-completing-read+)
(use-package flx-ido)
;; (global-set-key (kbd "M-o") 'mode-line-other-buffer)
(use-package bind-key
  :config
  (bind-key* "M-q" 'ace-window))
(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode 1))
(use-package projectile)
(use-package org-cliplink)
(use-package terminal-here)
;; (use-package org-roam
  ;; :config
  ;; (setq org-roam-directory "~/org/org-roam")
  ;; (add-hook 'after-init-hook 'org-roam-mode)
  ;; )

(use-package magit)
(use-package yasnippet)
(use-package yasnippet-classic-snippets)
(use-package yasnippet-snippets)
(use-package helm)
(use-package tramp)
(use-package ein
  :config
  (setq ein:output-area-inlined-images t))
(use-package memento-mori)
(use-package haskell-mode)
(use-package rust-mode)
(use-package yaml-mode)
(use-package julia-mode)
(use-package markdown-mode)
(use-package spray)
(use-package dictcc)
(use-package ess)
(use-package deft
  :config
  (setq deft-extensions '("org" "txt"))
  (setq deft-use-filter-string-for-filename t)
  (setq deft-directory "~/Nextcloud/org/zettelkasten"))
(use-package doom-themes)
(use-package company)
(use-package ace-window)
(use-package ace-jump-mode)
(use-package which-key)
(use-package volatile-highlights)
(use-package plantuml-mode)
(use-package writeroom-mode)
(use-package ido-completing-read+)

(use-package eglot :ensure t)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd-10"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)

;; pdf-tools
(pdf-loader-install)

;; hooks
;; (add-hook 'after-init-hook 'org-roam-mode)
(add-hook 'after-init-hook 'yas-global-mode)
(add-hook 'after-init-hook 'visual-line-mode)
(add-hook 'after-init-hook 'column-number-mode)
(add-hook 'after-init-hook 'winner-mode)
(add-hook 'after-init-hook 'whole-line-or-region-global-mode)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)


;; moving files with dired
(setq dired-dwim-target t)


;; stop projectile from slowing down tramp
(defadvice projectile-on (around exlude-tramp activate)
  "This should disable projectile when visiting a remote file"
  (unless  (--any? (and it (file-remote-p it))
                   (list
                    (buffer-file-name)
                    list-buffers-directory
                    default-directory
                    dired-directory))
    ad-do-it))

(setq projectile-mode-line "Projectile")


(defun insert-date()
  (interactive)
  (insert
   (concat "# " (shell-command-to-string "date +\"%d.%m.%Y %k:%M\""))))


;; Avoid stale dired buffers
;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; README via pandoc + github css style
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(setq markdown-command "pandoc -c file:///home/philipp/.emacs.d/github-pandoc.css --from gfm -t html5 --mathjax --highlight-style pygments --standalone")


;; (setq memento-mori-birth-date "1998-03-01")
;; (memento-mori-mode)

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)


;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
(ido-ubiquitous-mode t)
(ido-everywhere t)

;;;;;;;;;;;;;;;; org mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org mode use speed commands when on star
(setq org-use-speed-commands
      (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**"))))


;; set location for org capture
(setq org-directory "~/Nextcloud/org")
(setq org-default-notes-file (concat org-directory "/gtd.org"))
(setq org-startup-folded t)

;; enable languages for org babel
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (R . t)
    (shell . t)
    (haskell . t)        
    (python . t)))

(setq org-babel-python-command "python3")

(setq org-src-tab-acts-natively t)

;; macro for opening zathura
(fset 'open-zathura
   [?& ?z ?a ?t ?h tab return ?\C-x ?1])

(fset 'open-gimp
   [?& ?g ?i ?m ?p return ?\C-x ?1])

(setq frame-title-format "%f")

;; set org capture template
(setq org-capture-templates
      '(("i" "Inbox" entry (file+headline "~/Nextcloud/org/gtd.org" "Inbox")
         "* %?")))

(setq org-refile-targets '(
   (nil :maxlevel . 2)             ; refile to headings in the current buffer
   (org-agenda-files :maxlevel . 2) ; refile to any of these files
    ))

;; irc client
;;erc package


;; Use IPython for REPL
(setq python-shell-interpreter "jupyter"

      python-shell-interpreter-args "console --simple-prompt"

      python-shell-prompt-detect-failure-warning nil)

(add-to-list 'python-shell-completion-native-disabled-interpreters

             "jupyter")


;; weird indentation when editing java files
;; from https://www.emacswiki.org/emacs/IndentingJava
(add-hook 'java-mode-hook (lambda ()
                                (setq c-basic-offset 4
                                      tab-width 4
                                      indent-tabs-mode t)))

;; Auto refresh buffers
(global-auto-revert-mode 1)

(setq c-basic-offset 4)
;; use org-mode for txt files
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.dcr\\'" . text-mode))

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)


;; org-roam config
(setq org-roam-directory (file-truename "~/Nextcloud/org/zettelkasten"))
(setq find-file-visit-truename t)
;; display inline images always!
(setq org-startup-with-inline-images t)
;; set default width of images
(setq org-image-actual-width (list 450))
(org-roam-db-autosync-mode)
(global-set-key (kbd "C-c f") 'org-roam-node-find)
(global-set-key (kbd "C-c i") 'org-roam-node-insert)


;; elfeed
;; Somewhere in your .emacs file
;; https://osf.io/preprints/
(setq elfeed-feeds
      '("https://lobste.rs/rss"
        "http://arxiv.org/rss/cs"
        "https://www.youtube.com/simonsinstitute"
        "http://arxiv.org/rss/physics"
        "http://arxiv.org/rss/q-bio"
        "https://www.lesswrong.com/feed.xml"
        "http://arxiv.org/rss/q-fin"
        "http://arxiv.org/rss/stat"
        "http://arxiv.org/rss/eess"
        "http://arxiv.org/rss/econ"
        "https://blog.fefe.de/rss.xml"
        "https://chemrxiv.org/engage/rss/chemrxiv?categoryId=605c72ef153207001f6470d0"
        "https://chemrxiv.org/engage/rss/chemrxiv?categoryId=605c72ef153207001f6470d5"
        "https://chemrxiv.org/engage/rss/chemrxiv?categoryId=605c72ef153207001f6470dd"
        "https://chemrxiv.org/engage/rss/chemrxiv?categoryId=605c72ef153207001f6470da"
        "https://chemrxiv.org/engage/rss/chemrxiv?categoryId=605c72ef153207001f6470d9"
        "https://chemrxiv.org/engage/rss/chemrxiv?categoryId=605c72ef153207001f6470db"
        "https://chemrxiv.org/engage/rss/chemrxiv?categoryId=60b63c9f57d3ab002262a6f7"
        "https://chemrxiv.org/engage/rss/chemrxiv?categoryId=605c72ef153207001f6470d2"
        "https://chemrxiv.org/engage/rss/chemrxiv?categoryId=605c72ef153207001f6470d8"
        "https://chemrxiv.org/engage/rss/chemrxiv?categoryId=605c72ef153207001f6470d1"
        "https://chemrxiv.org/engage/rss/chemrxiv?categoryId=605c72ef153207001f6470d1"
        "http://connect.biorxiv.org/biorxiv_xml.php?subject=all"
        "https://pubmed.ncbi.nlm.nih.gov/rss/search/1XsgQGan4PZ5SVUmkCmlRCQWI7bRcS27BU6X9zs4mgvF7h_ol6/?limit=15&utm_campaign=pubmed-2&fc=20230716065139"
        "https://news.ycombinator.com/rss"
        ))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("~/Nextcloud/org/gtd.org" "~/Nextcloud/Notes/agenda.txt")))
 '(package-selected-packages
   (quote
    (elfeed google-translate emacsql ereader lua-mode darkroom fsharp-mode scala-mode csharp-mode cuda-mode adoc-mode futhark-mode json-mode eglot company-lean lean-mode unipoint erlang-mode pdf-tools multiple-cursors deft flutter all-the-icons-completion erlang helm yasnippet-snippets yasnippet-classic-snippets yaml-mode writeroom-mode wiki-summary whole-line-or-region which-key volatile-highlights use-package typescript-mode tramp terminal-here tagedit spray smex slime-repl slim-mode skewer-mode rust-mode rainbow-delimiters projectile pomidor plantuml-mode org-roam org-ref org-gcal org-cliplink nasm-mode memento-mori magit-todos lsp-mode ledger-mode jupyter julia-mode ido-completing-read+ helm-wikipedia helm-unicode haskell-mode graphviz-dot-mode go-mode geiser folding flycheck flx-ido exec-path-from-shell evil-numbers ess emms elpher elfeed-org ein doom-themes dictcc dart-mode csv-mode company clojure-snippets clojure-mode-extra-font-locking clj-refactor auto-dictionary auctex anki-editor ace-window ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)

;; (load-file (let ((coding-system-for-read 'utf-8))
                ;; (shell-command-to-string "agda-mode locate")))
;; (load-file "/home/pt/.cabal/store/ghc-9.2.4/Agda-2.6.2.2-e222571990d1912053f6135a206a8e1650b0a85840ab390825a19dd97a77f70b/share/emacs-mode/agda2.el")
