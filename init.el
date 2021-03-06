;;; init.el

;;; Bootstrap
;; Speed up startup
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)


;; initial buffer choice
(setq initial-buffer-choice "~/org/gtd.org")

;;; general config
;; move with SHIFT-arrow
(windmove-default-keybindings)

;; theme
(load-theme 'leuven t)

;; set font size
(set-face-attribute 'default nil :height 120)

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)


;; highlight todos
;; "TODO", "FIXME", "DEBUG"
;(global-hl-todo-mode 1)

;; hide menu-bar and tool-bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; do not show line numbers
(global-linum-mode -1)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; no bell
(setq ring-bell-function 'ignore)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; No need for ~ files when editing
(setq create-lockfiles nil)

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

;; org mode use speed commands when on star
(setq org-use-speed-commands
      (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**"))))

;; set location for org capture
(setq org-directory "~/org")
(setq org-default-notes-file (concat org-directory "/gtd.org"))

;; enable languages for org babel
(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (R . t)
    (python . t)))

;; function for toggle comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; keymaps
(global-set-key "\M-k" '(lambda () (interactive) (kill-line 0)) ) ;M-k kills to the left
(global-set-key (kbd "C-x C-n") 'writeroom-increase-width)
(global-set-key (kbd "C-c r") 'query-replace-regexp)
(global-set-key (kbd "C-c l") 'org-cliplink)
(global-set-key (kbd "C-.") 'toggle-comment-on-line)
(global-set-key (kbd "C-c d") 'dictcc)
(global-set-key (kbd "C-x p") 'dictcc-at-point)
(global-set-key (kbd "C-c f") 'org-roam-find-file)
(global-set-key (kbd "C-c i") 'org-roam-insert)
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
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "C-c s") 'ispell)
;; increment and decrement numbers
(global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
(global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt)
;; set key bindings
(global-set-key (kbd "<f12>") 'terminal-here)
(global-set-key (kbd "<f9>") 'dictcc)
(global-set-key (kbd "<f6>") 'writeroom-mode)
;; recompile command
(global-set-key (kbd "<f5>") 'recompile)
;; agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;; projectile keybindings
(global-set-key (kbd "C-c p") 'projectile-command-map)


;(define-key pdf-view-mode-map (kbd "k") 'nil)
;(define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page) ; remove killing buffer
;(define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page) ; remove killing buffer

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)
;;scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))



;;--------------------------------------------------------------------
;; fix downloading from gnu archive
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Initialize package.el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; packages
;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))


(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    use-package

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
    ido-completing-read+
    
    ;; flex matching for ido
    flx-ido
    
    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; use C-w to kill current line
    whole-line-or-region

    ;; project navigation
    projectile

    ;; latex editing
    auctex

    ;; note taking ui
    deft

    ;; pretty links in org files
    org-cliplink

    ;; open terminal at current location
    terminal-here

    ;; zettelkasten
    org-roam

    ;; git integration
    magit
    magit-todos
    hl-todo

    ;; code snippet completion
    yasnippet
    yasnippet-classic-snippets
    yasnippet-snippets

    ;; filter-as-you-type completion
    helm

    ;; for focused writing
    writeroom-mode

    ;; highlight changes
    volatile-highlights

    ;; remote access
    tramp

    ;; feed reader
    elfeed
    elfeed-org

    ;; jupyter notebooks in emacs
    ein
    
    ;; editing modes
    haskell-mode
    rust-mode
    yaml-mode
    julia-mode
    markdown-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; hooks
(add-hook 'after-init-hook 'org-roam-mode)
(add-hook 'after-init-hook 'yas-global-mode)
(add-hook 'after-init-hook 'visual-line-mode)
(add-hook 'after-init-hook 'column-number-mode)
(add-hook 'after-init-hook 'winner-mode)
(add-hook 'after-init-hook 'whole-line-or-region-global-mode)
;; pdf-tools
(pdf-loader-install)

;; config for deft
(setq deft-default-extension "org")

;; Load elfeed-org
(require 'elfeed-org)

;; Initialize elfeed-org
;; This hooks up elfeed-org to read the configuration when elfeed
;; is started with =M-x elfeed=
(elfeed-org)

;; Optionally specify a number of files containing elfeed
;; configuration. If not set then the location below is used.
;; Note: The customize interface is also supported.
(setq rmh-elfeed-org-files (list "~/org/elfeed.org"))

;;--------------------------------------------------------------------
;; package config
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

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'helm-config)


;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; projectile everywhere!
(projectile-global-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-mode t)
 '(TeX-view-program-list nil)
 '(TeX-view-program-selection
   (quote
    ((output-pdf "Zathura")
     ((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Evince")
     (output-html "xdg-open"))))
 '(custom-enabled-themes (quote (leuven)))
 '(org-agenda-files (quote ("~/org/gtd.org")))
 '(org-modules
   (quote
    (org-habit org-w3m org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail)))
 '(package-selected-packages
   (quote
    (dart-mode csv-mode emms go-mode typescript-mode magit-todos flx-ido helm-unicode tramp anki-editor use-package dictcc zetteldeft yasnippet-snippets yasnippet-classic-snippets yaml-mode which-key terminal-here tagedit smex slime-repl slim-mode skewer-mode rust-mode rainbow-delimiters projectile plantuml-mode paredit org-roam org-ref org-gcal org-cliplink magit lsp-mode ledger-mode julia-mode ido-completing-read+ haskell-mode graphviz-dot-mode geiser folding flycheck ein company clojure-mode-extra-font-locking cider auto-dictionary auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; set org capture template
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Inbox")
         "* %?")))

;; set org capture template
(add-to-list 'org-capture-templates
             '("m" "media inbox"
               entry
               (file+headline "~/org/gtd.org" "Media Inbox")
               "* %?"))

(add-to-list 'org-capture-templates
             '("a" "anki inbox"
               entry
               (file+headline "~/org/gtd.org" "Anki inbox")
               "* %?"))
(add-to-list 'org-capture-templates
             '("f" "food tracking"
               entry
               (file+headline "~/org/quantified_self.org" "Daily Food")
               "* %t %?"))
(add-to-list 'org-capture-templates
             '("w" "body Weight tracking"
               entry
               (file+headline "~/org/quantified_self.org" "Body weight")
               "* %t %?"))
(add-to-list 'org-capture-templates
             '("h" "Happiness + General well-being"
               entry
               (file+headline "~/org/quantified_self.org" "Happiness + General well-being")
               "* %t \n %?"))
(add-to-list 'org-capture-templates
             '("g" "Gratitude"
               entry
               (file "~/org/gratitude.org")
               "* %t \n %?"))

;; fix org-mode
(org-reload)

(require 'emms-setup)
     (emms-all)
(emms-default-players)
(setq emms-player-list '(emms-player-mpv))
(setq emms-source-file-default-directory "/home/philipp/media/music/")


(defun yank-buffer ()
 "Nonce function"
 (interactive)
 (kill-new buffer-file-name))

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
  (insert (shell-command-to-string "date +\"%d.%m.%Y %k:%M\"")))
