(defun not-nil-p (x)
  "Return `t` if X is not nil."
  (not (not x)))

;; [[file:config.org::*Basic][Basic:1]]
;; Customize user interface.
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(setq inhibit-startup-screen t)
(column-number-mode)

;; Don't show trailing whitespace _always_. It's annoying.
(setq-default show-trailing-whitespace nil)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Consider a period followed by a single space to be end of sentence.
(setq sentence-end-double-space nil)

;; Use spaces, not tabs, for indentation.
(setq-default indent-tabs-mode nil)

;; Display the distance between two tab stops as 4 characters wide.
(setq-default tab-width 4)

;; Indentation setting for various languages.
(setq c-basic-offset 4)
(setq js-indent-level 2)
(setq css-indent-offset 2)

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

;; Move the point to bottom/top when using `C-v' and `M-v', respectively,
;; rather than just trying to scroll.
(setq scroll-error-top-bottom t)

;; Disable blinking cursor.
(setq-default visible-cursor nil)
(blink-cursor-mode 0) ;; Should be unnecessary on Emacs >24?

;; Make `replace-regexp-in-string' case-sensitive by default.
;; NOTE: This is mainly useful for things like `latex' src-blocks where,
;; for some reason, variable replacement is done case-insensitively, which I don't like.
;; But uncertain if this has any unwanted side-effects.
;; TODO: Maybe make PR to `org-mode' to make this configurable?
(defun with-case-fold-search-off (orig-fun &rest args)
  (let ((case-fold-search nil))
    (apply orig-fun args)))

(advice-add 'replace-regexp-in-string :around #'with-case-fold-search-off)

;; Basic:1 ends here

;; [[file:config.org::*Make =format-time-string= consistent across OS time settings][Make =format-time-string= consistent across OS time settings:1]]
;; Ensures that we're always going to format the string according to EN locale.
;; Setting `system-time-locale' to `"C"' or something doesn't work for daemon-mode.
;; This is copy-paste from https://kisaragi-hiu.com/blog/2019-10-09-format-time-string-today.html.
(require 'calendar)
(defun kisaragi/english-dow (&optional time zone abbreviated)
  "Return ABBREVIATED name of the day of week at TIME and ZONE.

If TIME or ZONE is nil, use `current-time' or `current-time-zone'."
  (unless time (setq time (current-time)))
  (unless zone (setq zone (current-time-zone)))
  (calendar-day-name
   (pcase-let ((`(,_ ,_ ,_ ,d ,m ,y . ,_)
                (decode-time time zone)))
     (list m d y))
   abbreviated))

(defun kisaragi/advice-format-time-string (func format &optional time zone)
  "Pass FORMAT, TIME, and ZONE to FUNC.

Replace \"%A\" in FORMAT with English day of week of today,
\"%a\" with the abbreviated version."
  (let* ((format (replace-regexp-in-string "%a" (kisaragi/english-dow time zone t)
                                           format))
         (format (replace-regexp-in-string "%A" (kisaragi/english-dow time zone nil)
                                           format)))
    (funcall func format time zone)))


(advice-add 'format-time-string :around #'kisaragi/advice-format-time-string)
;; Make =format-time-string= consistent across OS time settings:1 ends here

;; [[file:config.org::*File backup][File backup:1]]
;; Write auto-saves and backups to separate directory.
(make-directory "~/.tmp/emacs/auto-save/" t)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))

;; Do not move the current file while creating backup.
(setq backup-by-copying t)

;; Disable lockfiles.
(setq create-lockfiles nil)
;; File backup:1 ends here

;; [[file:config.org::*Custom settings in seperate file][Custom settings in seperate file:1]]
;; Write customizations to a separate file instead of this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)
;; Custom settings in seperate file:1 ends here

;; [[file:config.org::*Package management: =straight.el= and =use-package=][Package management: =straight.el= and =use-package=:1]]
;; straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun my/straight-installed-p (package)
  "Check if PACKAGE is installed (according to `straight.el')."
  (straight--installed-and-buildable-p
   ;; `format' allows us to handle both strings and symbols.
   (gethash (format "%s" package) straight--recipe-cache)))

;; Deactivate native compilation.
;; (setq straight-disable-native-compile nil)

;; use-package.el: Makes configuring the packages much easier.
(straight-use-package 'use-package)

;; Use `straight.el` by default when calling `use-package`.
(setq straight-use-package-by-default t)
;; Package management: =straight.el= and =use-package=:1 ends here

;; [[file:config.org::*Customizing =use-package=][Customizing =use-package=:1]]
;; Allow us to "require" system packages to be present using `:ensure-system-package'
;; in `use-package' blocks.
(use-package use-package-ensure-system-package)
;; Customizing =use-package=:1 ends here

;; [[file:config.org::*OS specific setup][OS specific setup:1]]
;; Linux
(defun tor/is-wsl-p ()
  ;; WSL: WSL1 has "-Microsoft", WSL2 has "-microsoft-standard"
  (not-nil-p (string-match "-[Mm]icrosoft" operating-system-release)))

(when (tor/is-wsl-p)
  ;; Source: https://www.emacswiki.org/emacs/Emacs_and_the_Windows_Subsystem_for_Linux
  (defun wsl-copy-region-to-clipboard (start end)
    "Copy region to Windows clipboard."
    (interactive "r")
    (call-process-region start end "clip.exe" nil 0))

  (defun wsl-clipboard-to-string ()
    "Return Windows clipboard as string."
    (let ((coding-system-for-read 'dos))
      (substring				; remove added trailing \n
       (shell-command-to-string
        "powershell.exe -Command Get-Clipboard") 0 -1)))

  (defun wsl-paste-from-clipboard (arg)
    "Insert Windows clipboard at point. With prefix ARG, also add to kill-ring"
    (interactive "P")
    (let ((clip (wsl-clipboard-to-string)))
      (insert clip)
      (if arg (kill-new clip)))))

(when (string-equal system-type "gnu/linux") ; linux
  (message "Performing Linux-specific setup...")
  (use-package exec-path-from-shell)
  (exec-path-from-shell-initialize)

  ;; use xclip to yank, allowing you to yank in terminal to the GLOBAL clipboard
  (when (not (tor/is-wsl-p))
      ((use-package xclip
         :init (xclip-mode))))
  )
;; OS specific setup:1 ends here

;; [[file:config.org::*Themes & styling][Themes & styling:1]]
(use-package solarized-theme
  :defer t
  ;; :init
  ;; (progn
  ;;   ;; Sets it to similar colors as the theme-colors; if `t' we use `dark' else we use `light'.
  ;;   ;; `solarized-dark' will have the "correct" midnight mode, so only do it if using `light'.
  ;;   ;; TODO: Implement `my/loaded-theme-is-solarized-light-p' or something similar
  ;;   (when (and (my/straight-installed-p 'pdf-tools) (my/loaded-theme-is-solarized-light-p))
  ;;     ;; (setq pdf-view-midnight-colors '("#556065" . "#fdf6e3"))
  ;;     ;; (setq pdf-view-midnight-colors '("#3f4446" . "#fdf6e3")) ;; slightly blacker font
  ;;     (setq pdf-view-midnight-colors '("#556065" . "#fff8e5")) ;; slightly brighter background
  ;;     ))
  )

(use-package darktooth-theme)
(use-package atom-one-dark-theme)

;; Load `solarized-dark` by default.
;; Themes & styling:1 ends here

;; [[file:config.org::*Themes & styling][Themes & styling:2]]
(use-package doom-themes)
;; (use-package spacegray-theme)
(load-theme 'doom-nord t)

;; Adds red "alert" to modeline upon hitting `C-g` to interrupt a command, etc.
(doom-themes-visual-bell-config)
;; Themes & styling:2 ends here

;; [[file:config.org::*Themes & styling][Themes & styling:3]]
;; You must run (all-the-icons-install-fonts) one time after
;; installing this package!
(use-package all-the-icons
  :if (display-graphic-p))
;; Themes & styling:3 ends here

;; [[file:config.org::*Modeline][Modeline:1]]
;; diminish.el: Allows us to hide certain mini-modes, if we so desire.
(use-package diminish)
;; Modeline:1 ends here

;; [[file:config.org::*Modeline][Modeline:2]]
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package doom-modeline
  ;; :after eshell     ;; Make sure it gets hooked after eshell
  :hook (after-init . doom-modeline-init)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 6)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc t)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))
;; Modeline:2 ends here

;; [[file:config.org::*=M-x=][=M-x=:1]]
;; helm.el: Provides a much more pleasant `M-x` experience. Alternative to `ido`.
(use-package helm
  :diminish helm-mode  ;; removes the helm-mode from the mode-line
  :init (progn
          (require 'helm-config)
          (helm-mode))
  :bind (("M-x" . helm-M-x)))

;; helm-descbinds.el: Adds mode
(use-package helm-descbinds
  :bind (("C-h b" . helm-descbinds)))
;; =M-x=:1 ends here

;; [[file:config.org::*=M-x=][=M-x=:2]]
;; which-key.el: Provides suggestions/completions for keybindings upon use.
(use-package which-key
  :diminish which-key-mode ;; hide form mode-line
  :config (which-key-mode))
;; =M-x=:2 ends here

;; [[file:config.org::*Scaling][Scaling:1]]
;; default-text-scale.el: Allows decreasing/increasing text size globally
;; rather than on a per-buffer basis.
(use-package default-text-scale
  :bind (("C-M-=" . default-text-scale-increase)
         ("C-M--" . default-text-scale-decrease)))
;; Scaling:1 ends here

;; [[file:config.org::*Special][Special:1]]
;; fic-mode.el: Provides highlighting for TODO, FIXME and BUG in comments.
(use-package fic-mode
  :hook (prog-mode . fic-mode)
  :config
  ;; Change the font
  (set-face-attribute 'fic-face nil
                      :foreground "magenta"
                      :background "transparent"
                      :weight 'bold
                      :slant 'normal)
  (set-face-attribute 'fic-author-face nil
                      :foreground "magenta"
                      :background "transparent"
                      :weight 'normal
                      :slant 'italic
                      :underline t)
  (add-to-list 'fic-highlighted-words "HACK")
  (add-to-list 'fic-highlighted-words "NOTE"))
;; Special:1 ends here

;; [[file:config.org::*Pretty][Pretty:1]]
(use-package prettify-symbols-mode
  :straight nil
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  ;; Fontification is deactivated upon marker-enter.
  (setq prettify-symbols-unprettify-at-point 'right-edge))
;; Pretty:1 ends here

;; [[file:config.org::*File management & browsing][File management & browsing:1]]
(use-package all-the-icons-dired
  :after (all-the-icons))

(use-package dired
  :ensure nil
  :straight nil
  :defer t
  :config
  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              ;; (dired-omit-mode 1) ;; TODO: Where is this from?
              ;; (dired-hide-details-mode 1)
              (when (display-graphic-p)
                ;; Activate the `all-the-icons-mode` if present.
                (all-the-icons-dired-mode 1))
              (hl-line-mode 1)))
  )
;; File management & browsing:1 ends here

;; [[file:config.org::*Window navigation][Window navigation:1]]
;; ace-window.el: Allows you to jump between windows. Super-useful when you're using more than 2 windows.
;; HACK: Only load if we're using a GUI. For some reason `ace-window' making it so that
;; switching between windows inserts 'I's and 'O's.
(when (display-graphic-p)
  (use-package ace-window
    ;; We might have multiple Emacs frames open, all using the same server.
    ;; In these cases it is usually undesired to have `ace-window' suggest
    ;; opening Emacs windows we can't even see. In addition, we usually then
    ;; end up with a huge number of candidates.
    ;; This limits the candidates that we can jump to to the current frame.
    :custom (aw-scope 'frame)
    ;; Feel free to change the binding.
    :bind ("M-[" . ace-window)))
;; Window navigation:1 ends here

;; [[file:config.org::*Within-buffer navigation][Within-buffer navigation:1]]
;; avy.el: Allows you to jump to words by specifying the first character.
(use-package avy
  ;; Feel free to change the binding.
  :bind ("M-j" . avy-goto-word-or-subword-1))
;; Within-buffer navigation:1 ends here

;; [[file:config.org::*Snippets][Snippets:1]]
;; yasnippet.el: Snippet engine.
(use-package yasnippet
  ;; Enable globally.
  :init (yas-global-mode)
  :config
  ;; Enable nested triggering of snippets.
  (setq yas-triggers-in-field t)
  ;; Ensures that the indentation is done after my choosing.
  (setq yas-indent-line 'fixed)
  )

;; yasnippet-snippets.el: A huge collection of useful snippets.
(use-package yasnippet-snippets)
;; Snippets:1 ends here

;; [[file:config.org::*PDF viewing][PDF viewing:1]]
;; pdf-tools.el: Best. PDF viewer. Ever.
;; NOTE: might need to run `(pdf-tools-install)' to install dependencies.
(use-package pdf-tools
  :mode ("\\.vpdf\\.?$" . pdf-virtual-edit-mode)
  :config (pdf-tools-install)
  ;; Force it to load so we trigger `pdf-tools-install'.
  :demand t)
;; PDF viewing:1 ends here

;; [[file:config.org::*Project management][Project management:1]]
;; projectile.el: A _bunch_ of utility functionality for working with projects, e.g. rename everywhere
;; in a projet.
;; It'll automatically detect if something is a project using different heuristics, e.g.
;; if you have a `.git` file in a parent directory.
(use-package projectile
  :diminish projectile-mode ;; hide from mode-line since it'll be activated everywhere
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (progn
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)
    (add-to-list 'projectile-globally-ignored-files "node-modules")
    (projectile-global-mode)))

;; helm-projectile.el: Improves interaction between `helm.el` and `projetile.el`.
(use-package helm-projectile)
(use-package helm-rg)
;; Project management:1 ends here

;; [[file:config.org::*Project management][Project management:2]]
(use-package treemacs)
(use-package treemacs-projectile)
(use-package treemacs-all-the-icons)
;; Project management:2 ends here

;; [[file:config.org::*Git][Git:1]]
;; magit.el: Objectively the best interface for working with Git-related stuff ever.
(use-package magit)
;; forge.el: Magit's interface to different repo hosts, e.g. Github, Gitlab.
(use-package forge)
;; Git:1 ends here

;; [[file:config.org::*Git][Git:2]]
(use-package blamer
  :ensure t
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  (blamer-commit-formatter "• %s") ;; default symbol is too large
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 0.8 ;; make it slightly smaller than default font-size
                   :italic t))))
;; Git:2 ends here

;; [[file:config.org::*General][General:1]]
;; smartparens.el: Automatic insertion of pairs of characters.
(use-package smartparens
  :config (progn
            (require 'smartparens-config)
            (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
            (add-hook 'prog-mode-hook 'show-paren-mode t)))
;; General:1 ends here

;; [[file:config.org::*Undo & redo][Undo & redo:1]]
;; undo-tree.el: Tree-based undo-mechanism.
;; NOTE: To install, see https://github.com/zachcurry/emacs-anywhere.
(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode))
;; Undo & redo:1 ends here

;; [[file:config.org::*Auto completion][Auto completion:1]]
;; company.el: Autocomplete backend. Other packages implement frontends for this,
;; e.g. auto-completer for Python.
(use-package company
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  )

;; Just to make the `company` UI work nicely with the Co-pilot suggestions.
(use-package company-box)

(use-package copilot
  :straight (:host github
             :repo "zerolfx/copilot.el"
             :files ("dist" "*.el"))
  :ensure t
  :config
  (progn
    ;; Unbind deprecated navigation bindings from `company'.
    (require 'bind-key)
    (unbind-key "M-p" company-active-map)
    (unbind-key "M-n" company-active-map)

    ;; Add bindings for `copilot'
    (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "M-p") 'copilot-previous-completion)
    (define-key copilot-completion-map (kbd "M-n") 'copilot-next-completion)
    (define-key copilot-completion-map (kbd "M-RET") 'copilot-accept-completion-by-word)
    (define-key copilot-completion-map (kbd "C-M-<return>") 'copilot-accept-completion-by-line)
  :init
  (progn
    (add-hook 'prog-mode-hook 'copilot-mode)

    (with-eval-after-load 'company
      ;; disable inline previews
      (delq 'company-preview-if-just-one-frontend company-frontends)))))
;; Auto completion:1 ends here

;; [[file:config.org::*Terminal emulation][Terminal emulation:1]]
(use-package vterm
  :config (setq vterm-buffer-name-string "*vterm [%s]*"))
;; Terminal emulation:1 ends here

;; [[file:config.org::*Flycheck][Flycheck:1]]
(use-package flycheck
  :config
  ;; Make `flycheck' recognize the packages available in Emacs' `load-path'.
  ;; Otherwise we get complaints on every `(require ...)'.
  ;; https://github.com/flycheck/flycheck/issues/1559#issuecomment-478569550
  (setq flycheck-emacs-lisp-load-path 'inherit))
;; Flycheck:1 ends here

;; [[file:config.org::*LaTeX][LaTeX:1]]
;; Don't clone but instead use `elpa' for this one.
(use-package tex
  :straight auctex
  ;; NOTE: You might have to build Auctex manually. Checkout the `INSTALL`
  ;; file in the cloned repo.
  :straight (auctex
             :type git
             :host github
             :repo "emacs-straight/auctex")
  
  :custom
  (TeX-source-correlate-start-server t)
  (TeX-macro-private nil "???")
  (TeX-parse-self t "Ensures that completion, etc. works properly.")
  (TeX-command-extra-options "-shell-escape")
  (TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
     (output-html "xdg-open"))
   "Specify the programs to use. In particular, use PDF tools for PDF viewing.")
  :config
  ;; Revert the document after compilation completes.
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  )

;; company-auctex.el: `company.el` frontend for `auctex.el`.
(use-package company-auctex
  :after (company)
  :hook (LaTeX-mode . company-mode)
  :init (company-auctex-init))

;; company-reftex.el: Completion of citations and labels within LaTeX commands, e.g. `\cite{}'.
(use-package company-reftex
  :after (company)
  :config (setq
           company-reftex-labels-regexp
           (rx "\\"
               ;; List taken from `reftex-ref-style-alist'
               (or "autoref"
                   "autopageref"
                   "Cpageref"
                   "cpageref"
                   "Cref"
                   "cref"
                   "eqref"
                   "Fref"
                   "fref"
                   "pageref"
                   "Ref"
                   "ref"
                   "vpageref"
                   "Vref"
                   "vref"
                   ;; custom stuff:
                   "propref"
                   "thmref"
                   "lemref"
                   "lemmaref"
                   "appref"
                   "assumptref"
                   "secref")
               "{"
               (group (* (not (any "}"))))
               (regexp "\\=")))
  (add-to-list 'company-backends 'company-reftex-labels)
  (add-to-list 'company-backends 'company-reftex-citations))
;; LaTeX:1 ends here

;; [[file:config.org::*Markdown][Markdown:1]]
;; markdown-mode.el: Standard mode for markdown.
(use-package markdown-mode
  :hook
  (
   ;; `visual-line-mode` adds word-wrap, etc.
   (markdown-mode . visual-line-mode)
   ;; Makes it so that we get automatic closing of **, etc.
   (markdown-mode . turn-on-smartparens-mode)
   )
  )
;; Markdown:1 ends here

(use-package yaml-mode)

;; [[file:config.org::*Polymode][Polymode:1]]
;; polymode: Allows you to use multiple modes within a single buffer, e.g.
;; use `julia-mode` for highlighting, etc. in a code-block within a markdown file.
(use-package polymode)

;; poly-markdown.el: Implementation of `polymode` for markdown, allowing other modes
;; to be used within buffers with `markdown-mode` enabled.
(use-package poly-markdown
  :mode ("\\.[jJ]md" . poly-markdown-mode) ;; Also enable for .jmd files.
  :bind (:map poly-markdown-mode-map
              ("C-c '" . markdown-edit-code-block)))

;; edit-indirect.el: Allows one to parts/subsections of buffers in a separate editable buffer,
;; whose changes are reflected in the main document. This is used by `poly-markdown` to allow
;; opening code-blocks in a separate editable buffer (see the `markdown-edit-code-block` from
;; the above `poly-markdown` block).
(use-package edit-indirect
  :config (progn
            (define-key edit-indirect-mode-map (kbd "C-c C-c") nil)))
;; Polymode:1 ends here

;; [[file:config.org::*Julia][Julia:1]]
;; Julia
(defvar prettify-symbols-alist--julia
  '(
    ("lambda" . ?λ)
    ("->" . ?↦)
    ("=>" . ?⟹)
    ))

(defun my/set-julia-prettify-symbols-alist ()
  (setq prettify-symbols-alist prettify-symbols-alist--julia)
  ;; HACK: It seems like we need to "re-enable" the mode to load the updated `prettify-symbols-alist'.
  (prettify-symbols-mode 1))

(use-package julia-mode
  :config
  (add-hook 'julia-mode-hook 'my/set-julia-prettify-symbols-alist)
  )
;; Julia:1 ends here

;; [[file:config.org::*Python][Python:1]]
;; Python
(use-package python
  ;; :hook
  ;; (
  ;;  ;; Make it so that `elpy-mode` is also enabled whenever `python-mode` is.
  ;;  (python-mode . elpy-mode)
  ;;  )
  )

;; TODO: can we remove this?
(use-package elpy
  ;; :defer t
  ;; `advice-add` effecftively allows you insert code before/after the execution of
  ;; some other functions. In this case we insert `(elpy-enable)` "before" `python-mode`,
  ;; i.e. whenever `python-mode` is called, `elpy-enable` will be called just before it.
  ;; :init (advice-add 'python-mode :before 'elpy-enable)
  )              

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  ((lsp-mode . lsp-enable-which-key-integration))
  )

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lps-treemacs-errors-list)

(use-package lsp-julia)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred
;; Python:1 ends here

;; [[file:config.org::*Emacs-lisp][Emacs-lisp:1]]
(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (ielm-mode . rainbow-delimiters-mode)
         (lisp-interaction-mode . rainbow-delimiters-mode)
         (list-mode . rainbow-delimiters-mode))
  ;; :config
  ;; ;; Custom faces.
  ;; (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
  ;; (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
  ;; (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
  ;; (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
  ;; (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
  ;; (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
  ;; (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
  ;; (set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
  ;; (set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray
  )
;; Emacs-lisp:1 ends here

;; [[file:config.org::*Jupyter][Jupyter:1]]
;; Jupyter
;; This is awesome _but_ requires an Emacs version built with dynamic modules.
;; See https://github.com/nnicandro/emacs-zmq for more information on this.
;; But if this has been done, then you cna uncomment the line below.
(use-package jupyter
  :config
  (setq org-babel-default-header-args:jupyter-julia '((:async . "yes")
                                                      (:session . "jl")
                                                      (:kernel . "julia-1.6")))
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:session . "py")
                                                       (:kernel . "python3"))))
;; Jupyter:1 ends here

;; [[file:config.org::*Org][Org:1]]
(defvar prettify-symbols-alist--org
  '(
    ("#+name:" . ?✎)
    ("#+begin_src" . ?↪)
    ("#+end_src" . ?□)
    ("#+begin_definition" . ?𝒟)
    ("#+end_definition" . ?□)
    ("#+begin_theorem" . ?𝒯)
    ("#+end_theorem" . ?□)
    ("#+begin_proof" . ?𝒫)
    ("#+end_proof" . ?■)
    ))

(defun my/prettify-symbols-alist-set--org ()
  (setq prettify-symbols-alist prettify-symbols-alist--org)
  ;; HACK: It seems like we need to "re-enable" the mode to load the updated `prettify-symbols-alist'.
  (prettify-symbols-mode 1))

(message "hi 2")
(use-package org
  ;; Ensures that we're using the version of `org` which comes with Emacs.
  :straight (org :type built-in)
  :hook (org-mode . my/prettify-symbols-alist-set--org)
  ;; Bind `C-c l' to `org-store-link'.
  :bind (("C-c l" . org-store-link))
  :config
  ;; customization for latex-preview in org-mode
  (setq org-format-latex-options '(:foreground default
                                               :background default
                                               :scale 1.5
                                               :html-foreground "steelblue"
                                               :html-background "Transparent"
                                               :html-scale 1.0
                                               :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

  ;; During LaTeX export, try to preserve the labels defined by the user.
  (setq org-latex-prefer-user-labels t)
  ;; Hide emphasis markup.
  (setq org-hide-emphasis-markers nil)
  ;; Use bullets for lists.
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  ;; Don't query us every time we trying to evaluate code in buffers.
  (setq org-confirm-babel-evaluate nil)
  ;; Don't indent text in a section to align with section-level.
  (setq org-adapt-indentation nil)
  ;; Don't indent body of code-blocks at all.
  (setq org-edit-src-content-indentation 0)
  ;; Allow setting variables in setup-files.
  (setq org-export-allow-bind-keywords t)
  ;; Where to store the generated images from `org-latex-preview'. This '/' at the end is VERY important.
  (setq org-preview-latex-image-directory "~/.ltximg/")
  ;; Make it so that the src block is opened in the current window when we open to edit.
  (setq org-src-window-setup 'current-window)
  ;; Necessary for header-arguments in src-blocks to take effect during export.
  (setq org-export-use-babel t)
  ;; Disable execution of code-blocks on export by default.
  (add-to-list 'org-babel-default-header-args '(:eval . "never-export"))
  ;; Allow us to specify width of images with `#+ATTR_ORG: :width ...'
  (setq org-image-actual-width nil)
  ;; Just generally a better default in our case.
  (setq org-directory "~/org-blog")
  ;; Format dates a bit nicer in latex exports.
  (setq org-latex-active-timestamp-format "\\texttt{%s}")
  (setq org-latex-inactive-timestamp-format "\\texttt{%s}")
  ;; Use `minted' for syntax highlighting in latex exports.
  (setq  org-latex-listings 'minted)
  ;; Don't export `_', etc. as subscript/supscript.
  (setq org-export-with-sub-superscripts nil)

  ;; Make `org-goto' nice to work with.
  ;; Source: https://emacs.stackexchange.com/a/32625
  ;; Complete on outlines/headings.
  ;; This uses `completing-read' behind the scenes, hence if you have something like
  ;; `helm' or `ivy' activated, this will be used for the completion.
  (setq org-goto-interface 'outline-path-completion)
  ;; Don't try to complete headings in steps.
  (setq org-outline-path-complete-in-steps nil)

  ;; Org-agenda / Org-capture related
  (setq org-agenda-files '("~/Dropbox/org/gtd.org"
                           "~/Dropbox/org/school.org"
                           "~/Dropbox/org/reading.org"
                           "~/Dropbox/org/implement.org"))
  (setq org-agenda-files nil)
  (setq org-default-notes-file "~/Dropbox/org/gtd.org")
  (setq org-refile-targets '(("~/Dropbox/org/gtd.org" :maxlevel . 2)))

  (setq org-capture-templates
        '(("t"        ;; shortcut
           "Todo"     ;; title
           entry      ;; type of template
           (file+headline "~/Dropbox/org/gtd.org" "Tasks")  ;; what and where to add
           "* TODO %^{Brief Description} %^g\nEntered on %U\n%?\n%i\n%a"  ;; template
           :empty-lines 1 ;; property
           )

          ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
           "* %^{Description}\nEntered on %U\n%a\n%?" :empty-lines 1)

          ("i" "Idea" item (file "~/Dropbox/org/ideas.org"))

          ("s" "School" entry
           (file "~/Dropbox/org/school.org")
           "* TODO %^{Brief Description} %^{COURSE}p %^g\n%?" :empty-lines 1)

          ("r" "Reading" entry (file "~/Dropbox/org/reading.org")
           "* TODO %(tor/reading-list-next-idx). %?\nEntered on %U\n%a\n%i")

          ("R" "Research" entry (file "~/org-blog/notes/research.org")
           "* %^{Title} %^g\n:PROPERTIES:\n:DATE: %U\n:SOURCE: %a\n:END:\n%i\n%?")

          ("I" "Implement" entry (file "~/Dropbox/org/implement.org")
           "* TODO %(tor/impl-list-next-idx). %?\nEntered on %U\n%a\n%i")

          ;; NOTE: the `ANKI_DECK' property will use auto-completion from `anki-editor.el'
          ;; and thanks to the use of `anki-editor-mode' in `~/Dropbox/org/anki.org'
          ;; we also get autocomplete for the tags.
          ("a" "Anki basic"
           entry
           (file+headline org-my-anki-file "Dispatch Shelf")
           "* %U   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:END:%^{ANKI_DECK}p\n** Front\n%?\n** Back\n%x\n")

          ("A" "Anki cloze"
           entry
           (file+headline org-my-anki-file "Dispatch Shelf")
           "* %U   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:END:%^{ANKI_DECK}p\n** Text\n%x\n** Extra\n")
          ))

  ;; Hooks.
  ;; If `flycheck` is installed, disable `flycheck` in src-blocks.
  ;; NOTE: This is maybe a bit "harsh". Could potentially just disable certain
  ;; features of `flycheck`.
  (when (my/straight-installed-p 'flycheck)
    (require 'flycheck)
    (defun disable-flycheck-in-org-src-block ()
      (flycheck-mode -1))
    (add-hook 'org-src-mode-hook 'disable-flycheck-in-org-src-block))

  ;; Use `visual-line-mode' as it gives word-wrapping, etc.
  (add-hook 'org-mode-hook 'visual-line-mode)

  ;; https://emacs.stackexchange.com/a/18146
  ;; I want this bindings for references, etc. + don't add files to agenda
  ;; often enough to warrant having a binding for it.
  (require 'bind-key)
  (unbind-key "C-c [" org-mode-map)
  (unbind-key "C-c ]" org-mode-map)
  (unbind-key "C-c ," org-mode-map)
  (bind-key "C-c ," 'org-time-stamp-inactive org-mode-map)

  ;; Make `org-capture' globally available.
  (global-set-key (kbd "C-c c") 'org-capture)

  ;;;; Org-Babel ;;;;
  ;; HACK: Need to load this here to ensure that we don't end up installing `org' (which is likely
  ;; to be a dependency of `ob-*' babel) using the wrong recipe.
  ;; (use-package ob-julia
  ;;   :config (setq org-babel-julia-command "julia"))

  ;; Specify which programming languages to support in code-blocks.
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp t)
     (shell . t)
     (C . t)
     (latex . t)
     (python . t)
     (jupyter . t)
     (latex . t)
     ;; (julia-vterm . t)
     ;; (julia . t)
     ))

  (require 'ob-jupyter)
  (org-babel-jupyter-override-src-block "julia")
  (org-babel-jupyter-override-src-block "python")

  ;; TODO: Figure out what is causing errors when using the terminal.
  ;; Faces.
  ;; (custom-theme-set-faces
  ;;  'user
  ;;  '(variable-pitch ((t (:family "ETBembo" :height 1.0))))
  ;;  '(fixed-pitch ((t ( :family "Source Sans Prop" :height 1.0))))
  ;;  )

  ;; (custom-theme-set-faces
  ;;  'user
  ;;  '(org-block ((t (:inherit fixed-pitch))))
  ;;  '(org-code ((t (:inherit fixed-pitch))))
  ;;  '(org-document-info ((t (:foreground "dark orange"))))
  ;;  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  ;;  '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
  ;;  '(org-link ((t (:foreground "royal blue" :underline t))))
  ;;  '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch) :weight normal))))
  ;;  '(org-property-value ((t (:inherit fixed-pitch))) t)
  ;;  '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  ;;  '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
  ;;  '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  ;;  '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

  ;; (let* ((variable-tuple
  ;;         (cond
  ;;          ;; ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
  ;;          ;; ((x-list-fonts "Lobster")         '(:font "Lobster")) ;; NOTE: This one is fun.
  ;;          ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
  ;;          ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
  ;;          ((x-list-fonts "Verdana")         '(:font "Verdana"))
  ;;          ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
  ;;          (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))
  ;;          ))
  ;;        (base-font-color     (face-foreground 'default nil 'default))
  ;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  ;;   (custom-theme-set-faces
  ;;    'user
  ;;    `(org-level-8 ((t (,@headline ,@variable-tuple))))
  ;;    `(org-level-7 ((t (,@headline ,@variable-tuple))))
  ;;    `(org-level-6 ((t (,@headline ,@variable-tuple))))
  ;;    `(org-level-5 ((t (,@headline ,@variable-tuple))))
  ;;    `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
  ;;    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
  ;;    `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
  ;;    `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
  ;;    `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))
  :custom
  (org-format-latex-header
   "\\documentclass{article}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")

  (org-latex-default-packages-alist
   '(("AUTO" "inputenc" t
      ("pdflatex"))
     ("T1" "fontenc" t
      ("pdflatex"))
     ("" "graphicx" t nil)
     ("" "grffile" t nil)
     ("" "longtable" nil nil)
     ("" "wrapfig" nil nil)
     ("" "rotating" nil nil)
     ("normalem" "ulem" t nil)
     ("" "amsmath" t nil)
     ("" "textcomp" t nil)
     ("" "amssymb" t nil)
     ("" "capt-of" nil nil)
     ("" "mathpazo" t nil)
     ("" "eulervm" t nil)))

  (org-preview-latex-process-alist
   '((dvipng :programs
             ("latex" "dvipng")
             :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
             (1.0 . 1.0)
             :latex-compiler
             ("latex -interaction nonstopmode -output-directory %o %f")
             :image-converter
             ("dvipng -D %D -T tight -bg 'Transparent' -o %O %f"))
     (dvisvgm :programs
              ("latex" "dvisvgm")
              :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
              (1.7 . 1.5)
              :latex-compiler
              ("latex -interaction nonstopmode -output-directory %o %f")
              :image-converter
              ("dvisvgm %f -n -b min -c %S -o %O"))
     (imagemagick :programs
                  ("latex" "convert")
                  :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                  (1.0 . 1.0)
                  :latex-compiler
                  ("pdflatex -interaction nonstopmode -output-directory %o %f")
                  :image-converter
                  ("convert -density %D -trim -antialias %f -quality 100 %O")))))

;; Override `org-babel-expand-body:latex' to support case-sensitive matching.
(defun my/org-babel-expand-body:latex (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (mapc (lambda (pair) ;; replace variables
          (setq body
                (replace-regexp-in-string
                 (regexp-quote (format "%S" (car pair)))
                 (if (stringp (cdr pair))
                     (cdr pair) (format "%S" (cdr pair)))
                 body
                 t)))
	(org-babel--get-vars params))
  (org-trim body))

;; TODO: Move this into `org' :config?
(advice-add 'org-babel-expand-body:latex :override #'my/org-babel-expand-body:latex)

;; Generate ID for headline if none exist.
(require 'org-id)
(defun org-id-new (&optional prefix)
  "Create a new globally unique ID.

An ID consists of two parts separated by a colon:
- a prefix
- a unique part that will be created according to `org-id-method'.

PREFIX can specify the prefix, the default is given by the variable
`org-id-prefix'.  However, if PREFIX is the symbol `none', don't use any
prefix even if `org-id-prefix' specifies one.

So a typical ID could look like \"Org-4nd91V40HI\"."
  (let* ((prefix (if (eq prefix 'none)
                     ""
                   (concat (or prefix org-id-prefix) "-")))
         unique)
    (if (equal prefix "-") (setq prefix ""))
    (cond
     ((memq org-id-method '(uuidgen uuid))
      (setq unique (org-trim (shell-command-to-string org-id-uuid-program)))
      (unless (org-uuidgen-p unique)
        (setq unique (org-id-uuid))))
     ((eq org-id-method 'org)
      (let* ((etime (org-reverse-string (org-id-time-to-b36)))
             (postfix (if org-id-include-domain
                          (progn
                            (require 'message)
                            (concat "@" (message-make-fqdn))))))
        (setq unique (concat etime postfix))))
     (t (error "Invalid `org-id-method'")))
    (concat prefix unique)))


(defun my/generate-sanitized-alnum-dash-string(str)
  "Returns a string which contains only a-zA-Z0-9 with single dashes
 replacing all other characters in-between them.

 Some parts were copied and adapted from org-hugo-slug
 from https://github.com/kaushalmodi/ox-hugo (GPLv3)."
  (let* (;; Remove "<FOO>..</FOO>" HTML tags if present.
         (str (replace-regexp-in-string "<\\(?1:[a-z]+\\)[^>]*>.*</\\1>" "" str))
         ;; Remove URLs if present in the string.  The ")" in the
         ;; below regexp is the closing parenthesis of a Markdown
         ;; link: [Desc](Link).
         (str (replace-regexp-in-string (concat "\\](" ffap-url-regexp "[^)]+)") "]" str))
         ;; Replace "&" with " and ", "." with " dot ", "+" with
         ;; " plus ".
         (str (replace-regexp-in-string
               "&" " and "
               (replace-regexp-in-string
                "\\." " dot "
                (replace-regexp-in-string
                 "\\+" " plus " str))))
         ;; Replace German Umlauts with 7-bit ASCII.
         (str (replace-regexp-in-string "[Ä]" "Ae" str t))
         (str (replace-regexp-in-string "[Ü]" "Ue" str t))
         (str (replace-regexp-in-string "[Ö]" "Oe" str t))
         (str (replace-regexp-in-string "[ä]" "ae" str t))
         (str (replace-regexp-in-string "[ü]" "ue" str t))
         (str (replace-regexp-in-string "[ö]" "oe" str t))
         (str (replace-regexp-in-string "[ß]" "ss" str t))
         ;; Replace all characters except alphabets, numbers and
         ;; parentheses with spaces.
         (str (replace-regexp-in-string "[^[:alnum:]()]" " " str))
         ;; On emacs 24.5, multibyte punctuation characters like "："
         ;; are considered as alphanumeric characters! Below evals to
         ;; non-nil on emacs 24.5:
         ;;   (string-match-p "[[:alnum:]]+" "：")
         ;; So replace them with space manually..
         (str (if (version< emacs-version "25.0")
                  (let ((multibyte-punctuations-str "：")) ;String of multibyte punctuation chars
                    (replace-regexp-in-string (format "[%s]" multibyte-punctuations-str) " " str))
                str))
         ;; Remove leading and trailing whitespace.
         (str (replace-regexp-in-string "\\(^[[:space:]]*\\|[[:space:]]*$\\)" "" str))
         ;; Replace 2 or more spaces with a single space.
         (str (replace-regexp-in-string "[[:space:]]\\{2,\\}" " " str))
         ;; Replace parentheses with double-hyphens.
         (str (replace-regexp-in-string "\\s-*([[:space:]]*\\([^)]+?\\)[[:space:]]*)\\s-*" " -\\1- " str))
         ;; Remove any remaining parentheses character.
         (str (replace-regexp-in-string "[()]" "" str))
         ;; Replace spaces with hyphens.
         (str (replace-regexp-in-string " " "-" str))
         ;; Remove leading and trailing hyphens.
         (str (replace-regexp-in-string "\\(^[-]*\\|[-]*$\\)" "" str)))
    str)
  )	

(defun my/id-get-or-generate(&optional pom)
  "Returns the ID property if set or generates and returns a new one if not set.
 The generated ID is stripped off potential progress indicator cookies and
 sanitized to get a slug. Furthermore, it is prepended with an ISO date-stamp
 if none was found before."
  (interactive)
  (org-with-point-at pom
    (when (not (org-id-get))
      (let* (
             ;; TODO: Only get the raw text, i.e. ignore stuff like a link [[...]].
	         ;; retrieve heading string
    	     (my-heading-text (nth 4 (org-heading-components)))
             (my-heading-text (progn
                                (message "%s" my-heading-text)
                                (message "%s" (org-element-interpret-data (org-element-at-point)))
                                (org-element-interpret-data my-heading-text)))
		     ;; remove progress indicators like "[2/7]" or "[25%]"
		     (my-heading-text (replace-regexp-in-string "[[][0-9%/]+[]] " "" my-heading-text))
		     ;; get slug from heading text
             (new-id (my/generate-sanitized-alnum-dash-string my-heading-text))
    	     )
        (when (not (string-match "[12][0-9][0-9][0-9]-[01][0-9]-[0123][0-9]-.+" new-id))
          ;; only if no ISO date-stamp is found at the beginning of the new id:
          (setq new-id (concat (format-time-string "%Y-%m-%d-%H-%M-%S-") new-id)))
        (org-set-property "ID" new-id)
        )
      ))

  (org-id-get);; retrieve the current ID in any case as return value
  )

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

(defun my/org-add-ids-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the current
   file which do not already have one. Only adds ids if the
   `auto-id' option is set to `t' in the file somewhere. ie,
   #+OPTIONS: auto-id:t"
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (org-map-entries (lambda () (my/id-get-or-generate (point))))))

;; automatically add ids to saved org-mode headlines
(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (and (eq major-mode 'org-mode)
                                   (eq buffer-read-only nil))
                          (my/org-add-ids-to-headlines-in-file))))))

;; Make it so that errors are also just outputted rather than opened in a different buffer, etc.
;; NOTE: This does NOT work in combination with `ob-async' (not sure `ob-async' actually runs the advice).
(defun my/advice--org-babel-execute:shell (args)
  (let ((body (car args)))
    (cons (concat "exec 2>&1\n" body "\n:\n") (cdr args))))

(advice-add 'org-babel-execute:shell :filter-args #'my/advice--org-babel-execute:shell)

;; NOTE: Attempt at fixing issue with `ob-async'.
;; Source: https://github.com/astahlman/ob-async/issues/75#issuecomment-766783255
(use-package ob-async
  ;; :straight (:type git :host github :repo "astahlman/ob-async")
  ;; Handling of errors: https://github.com/astahlman/ob-async/issues/75#issuecomment-766783255
  ;; + some of my changes.
  :straight (:type git :host github :repo "torfjelde/ob-async" :branch "tor/develop"))

;; TODO: Does this actually work? Check it.
;; Source: https://github.com/astahlman/ob-async/issues/75#issuecomment-1133145526
(defun no-hide-overlays (orig-fun &rest args)
  (setq org-babel-hide-result-overlays nil))

(advice-add 'ob-async-org-babel-execute-src-block :before #'no-hide-overlays)

;; Source: https://emacs.stackexchange.com/a/63562
(defun ek/babel-ansi ()
  "Properly handle ANSI color codes in the result for a SRC block."
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))))
(add-hook 'org-babel-after-execute-hook 'ek/babel-ansi)

(defmacro my/by-backend (&rest body)
  "Allow specification of different behavior for different backends.
The BODY is evaluated with the variable `org-export-current-backend' bound
to the current backend.  If the backend is not specified in the BODY, the
first form is used.

Example:

#+header: :results (my/backend (latex \"latex\") (t \"output\"))
#+begin_src emacs-lisp
...
#+end_src

results in `latex' used if the current backend is latex, and `output' otherwise.
"
  `(cl-case (if (not (not org-export-current-backend)) (org-export-backend-name (org-export-get-backend org-export-current-backend)) nil) ,@body))

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Very useful package for navigating Org buffers.
(use-package helm-org-ql
  :after org
  :init (progn
          (setq org-ql-search-directories-files-recursive t)))


;; Org:1 ends here

;; [[file:config.org::*Jupyter notebook][Jupyter notebook:1]]
;; (use-package ox-ipynb
;;   :straight (:type git :host github :repo "jkitchen/ox-ipynb")
;;   :init
;;   ;; It doesn't respect the `(org-babel-jupyter-override-src-block "python")' statements
;;   ;; and so it won't recognize `python' blocks as `jupyter-python', and thus not export properly.
;;   (setq ox-ipynb-kernelspecs '((R . (kernelspec . ((display_name . "R")
;;                                                    (language . "R")
;;                                                    (name . "ir"))))
;;                                (julia . (kernelspec . ((display_name . "Julia 1.6.2")
;;                                                        (language . "julia")
;;                                                        (name . "julia-1.6"))))
;;                                (jupyter-python . (kernelspec . ((display_name . "Python 3")
;;                                                                 (language . "python")
;;                                                                 (name . "python3"))))
;;                                (python . (kernelspec . ((display_name . "Python 3")
;;                                                         (language . "python")
;;                                                         (name . "python3"))))))


;;   (setq ox-ipynb-language-infos
;;         '((jupyter-python . (language_info . ((codemirror_mode . ((name . ipython)
;;                                                                   (version . 3)))
;;                                               (file_extension . ".py")
;;                                               (mimetype . "text/x-python")
;;                                               (name . "python")
;;                                               (nbconvert_exporter . "python")
;;                                               (pygments_lexer . "ipython3")
;;                                               (version . "3.8.10"))))
;;           (python . (language_info . ((codemirror_mode . ((name . ipython)
;;                                                           (version . 3)))
;;                                       (file_extension . ".py")
;;                                       (mimetype . "text/x-python")
;;                                       (name . "python")
;;                                       (nbconvert_exporter . "python")
;;                                       (pygments_lexer . "ipython3")
;;                                       (version . "3.8.10"))))

;;           (julia . (language_info . ((codemirror_mode . "julia")
;;                                      (file_extension . ".jl")
;;                                      (mimetype . "text/x-julia")
;;                                      (name . "julia")
;;                                      (pygments_lexer . "julia")
;;                                      (version . "1.6.2"))))

;;           (R . (language_info . ((codemirror_mode . "r")
;;                                  (file_extension . ".r")
;;                                  (mimetype . "text/x-r-source")
;;                                  (name . "R")
;;                                  (pygments_lexer . "r")
;;                                  (version . "3.3.2")))))))
;; Jupyter notebook:1 ends here

;; [[file:config.org::*Org-roam][Org-roam:1]]
(message "hi 1")
;; (use-package org-roam
;;   :straight (:type git :host github :repo "org-roam/org-roam-v1")
;;   ;; :hook
;;   ;; (after-init . org-roam-mode)
;;   :custom
;;   (org-roam-directory (file-truename "~/org-roam/"))
;;   (org-roam-completion-everywhere t)
;;   (org-roam-include-type-in-ref-path-completions t)
;;   :bind (:map org-roam-mode-map
;;          (("C-c n l" . org-roam)
;;           ("C-c n f" . org-roam-find-file)
;;           ("C-c n g" . org-roam-graph))
;;          :map org-mode-map
;;          (("C-c n i" . org-roam-insert))
;;          (("C-c n I" . org-roam-insert-immediate)))
;;   )
;; Org-roam:1 ends here

;; [[file:config.org::*Org-ref][Org-ref:1]]
;; helm-bibtex.el: Provides completion with `helm` for bibliographies.
(use-package helm-bibtex
  :bind ("C-c ]" . helm-bibtex)
  :config
  (setq bibtex-completion-library-path '("~/Dropbox/bibliography/pdfs/"))
  (setq bibtex-completion-bibliography '("~/Dropbox/bibliography/references.bib"))
  (setq bibtex-completion-additional-search-fields '(keywords))
  (setq bibtex-completion-pdf-field "file")

  ;; Same as old `org-ref'
  (setq bibtex-completion-display-formats 
        '((t . "${author:36} ${title:*} ${year:4} ${=has-pdf=:1}${=has-note=:1} ${=type=:7} ${keywords:40}")))

  ;; Use `org-ref-insert-cite-keys' for citation formatting when in `org-mode'.
  (setq bibtex-completion-format-citation-functions
        ;; TODO: Actually, functions used here are expected to _return_ the formatted string, _not_ insert
        ;; it directly into the buffer (which is what `org-ref-insert-cite-keys' does).
        '((org-mode . org-ref-insert-cite-keys)
          (latex-mode . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (python-mode . bibtex-completion-format-citation-sphinxcontrib-bibtex)
          (rst-mode . bibtex-completion-format-citation-sphinxcontrib-bibtex)
          (default . bibtex-completion-format-citation-default)))
  )

;; org-ref.el: Provides citation management and handling for Org-mode.
(use-package org-ref
  :config
  ;; Use `helm-bibtex' for completion etc.
  (require 'org-ref-helm)
  (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
        org-ref-insert-cite-function 'org-ref-cite-insert-helm
        org-ref-insert-label-function 'org-ref-insert-label-link
        org-ref-insert-ref-function 'org-ref-insert-ref-link
        org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))))
;; Org-ref:1 ends here

;; [[file:config.org::*helm-org-named][helm-org-named:1]]


(message "hi 3")

(use-package helm-org-named
  :straight (:type git :host github :repo "torfjelde/helm-org-named")
  :bind ("C-c [" . helm-org-named)
  :config
  (setq helm-org-named-directories '("/home/tor/org-blog/notes")))
;; helm-org-named:1 ends here

;; [[file:config.org::*Org-pomodoro][Org-pomodoro:1]]
;; (use-package org-pomodoro
;;   :ensure-system-package mplayer
;;   :config
;;   ;; Add notification.
;;   ;; NOTE: This might only work on linux.
;;   (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil))))
;;   ;; Keep the time entered even if we killed the timer midway.
;;   (setq org-pomodoro-keep-killed-pomodoro-time t)
;;   ;; Use 45min sessions instead.
;;   (setq org-pomodoro-length 45)
;;   ;; Force us to enter break manually, thus we're just making `org-pomodoro` a "notifier" that
;;   ;; we've spent so and so much time on a particular task, rather than religiously following "The Pomodoro Way".
;;   (setq org-pomodoro-manual-break t)
;;   ;; ;; Disable sound.
;;   ;; (setq org-pomodoro-play-sounds nil)

;;   ;; Use `mplayer' since this allows us specify the volume.
;;   (setq org-pomodoro-audio-player "mplayer")
;;   (setq org-pomodoro-finished-sound-args "-volume 50")
;;   (setq org-pomodoro-long-break-sound-args "-volume 50")
;;   (setq org-pomodoro-short-break-sound-args "-volume 50")

;;   ;; Play a sound when we start. It's useful feedback.
;;   (setq org-pomodoro-start-sound-p t)
;;   (setq org-pomodoro-start-sound-args "-volume 50")

;;   ;; Play a sound when we run into overtime too.
;;   (setq org-pomodoro-overtime-sound-p t)
;;   (setq org-pomodoro-overtime-sound-args "-volume")
;;   )
;; Org-pomodoro:1 ends here

;; [[file:config.org::*Org-ql][Org-ql:1]]
(use-package org-ql)
;; Org-ql:1 ends here

;; [[file:config.org::*Org-noter][Org-noter:1]]
;; (use-package org-noter
;;   :config
;;   ;; Don't hide the other sections when we scroll to a new page.
;;   (setq org-noter-hide-other nil)

;;   (setq org-noter-doc-property-in-notes t)

;;   (setq org-noter-notes-search-path '("~/org-blog/papers/"))
;;   (setq org-noter-default-notes-file-names '("notes.org"))
;;   ;; (require 'org-noter-pdftools)
;;   )

;; (use-package org-noter-pdftools
;;   :after org-noter
;;   :config
;;   ;; Add a function to ensure precise note is inserted
;;   (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
;;     (interactive "P")
;;     (org-noter--with-valid-session
;;      (let ((org-noter-insert-note-no-questions (if toggle-no-questions
;;                                                    (not org-noter-insert-note-no-questions)
;;                                                  org-noter-insert-note-no-questions))
;;            (org-pdftools-use-isearch-link t)
;;            (org-pdftools-use-freestyle-annot t))
;;        (org-noter-insert-note (org-noter--get-precise-info)))))

;; ;;   ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
;; ;;   (defun org-noter-set-start-location (&optional arg)
;; ;;     "When opening a session with this document, go to the current location.
;; ;; With a prefix ARG, remove start location."
;; ;;     (interactive "P")
;; ;;     (org-noter--with-valid-session
;; ;;      (let ((inhibit-read-only t)

;; ;;            (ast (org-noter--parse-root))
;; ;;            (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
;; ;;        (with-current-buffer (org-noter--session-notes-buffer session)
;; ;;          (org-with-wide-buffer
;; ;;           (goto-char (org-element-property :begin ast))
;; ;;           (if arg
;; ;;               (org-entry-delete nil org-noter-property-note-location)
;; ;;             (org-entry-put nil org-noter-property-note-location
;; ;;                            (org-noter--pretty-print-location location))))))))

;;   (with-eval-after-load 'pdf-annot
;;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
;; Org-noter:1 ends here

;; [[file:config.org::*Emacs anywhere][Emacs anywhere:1]]
;; emacs-anywhere: https://github.com/zachcurry/emacs-anywhere
(defun github-conversation-p (app-name window-title)
  (and
   (string-match-p "google-chrome" (downcase app-name))
   (or (string-match-p "Pull Request" window-title)
       (string-match-p "Issue" window-title))))

(defun plutojl-p (app-name window-title)
  (and
   (string-match-p "google-chrome" (downcase app-name))
   ;; Last part of the window name should be `Pluto.jl'
   (string-match-p "Pluto\\.jl$" window-title)))

(defun popup-handler (app-name window-title x y w h)
  ;; Resize
  (set-frame-width (selected-frame) 250)
  (set-frame-height (selected-frame) 50)
  ;; set major mode
  (cond
   ((github-conversation-p app-name window-title) (poly-markdown-mode))
   ((plutojl-p app-name window-title) (julia-mode))
   ;; ...
   (t (poly-markdown-mode)) ; default major mode
   ))

;; NOTE: `ea-popup-hook' is used by `emacs-anywhere'.
(add-hook 'ea-popup-hook 'popup-handler)
;; Emacs anywhere:1 ends here

;; [[file:config.org::*Frame name][Frame name:1]]
;; Format the application/window name as "USER [PROJECT NAME]: FILE"
(setq-default frame-title-format
              '(:eval
                (format "%s [%s]: %s"
                        (or (file-remote-p default-directory 'user)
                            user-real-login-name)
                        ;; (or (file-remote-p default-directory 'host)
                        ;;     system-name)
                        (projectile-project-name)
                        (buffer-name)
                        )))
;; Frame name:1 ends here

(message "hi 4")

;; Emoji support
;; Source: https://ianyepan.github.io/posts/emacs-emojis/
(use-package emojify)

;; Copied from `%systemroot%/Fonts'.
(when (member "Segoe UI Emoji" (font-family-list))
  (set-fontset-font t 'symbol (font-spec :family "Segoe UI Emoji") nil 'prepend))

;; Just use the `:' specification for emojis.
(setq emojify-emoji-styles '(github))
;; Use unicode instead of images.
(setq emojify-display-style 'unicode)

;; Replicate `org-previous-src-block' with including matching inline calls.
(defvar my/org-babel-inline-call-regexp "call_\\S-\\|^[ \t]*#\\+CALL:"
  "Regexp matching inline calls.")

(defun my/join-regexps-or (regexp1 regexp2)
  "Join two regexps in an OR match."
  (concat "\\(" regexp1 "\\)\\|\\(" regexp2 "\\)"))

(defun org-next-src-block-or-inline-call (&optional arg)
  "Jump to the next source block or inline call."
  (interactive "p")
  (org-next-block arg nil (my/join-regexps-or org-babel-src-block-regexp my/org-babel-inline-call-regexp)))

(defun org-previous-src-block-or-inline-call (&optional arg)
  "Jump to the previous source block or inline call."
  (interactive "p")
  (org-previous-block arg (my/join-regexps-or org-babel-src-block-regexp my/org-babel-inline-call-regexp)))

;; Override.
(defun org-next-block (arg &optional backward block-regexp)
  "Jump to the next block.

With a prefix argument ARG, jump forward ARG many blocks.

When BACKWARD is non-nil, jump to the previous block.

When BLOCK-REGEXP is non-nil, use this regexp to find blocks.
Match data is set according to this regexp when the function
returns.

Return point at beginning of the opening line of found block.
Throw an error if no block is found."
  (interactive "p")
  (let ((re (or block-regexp "^[ \t]*#\\+BEGIN"))
	    (case-fold-search t)
	    (search-fn (if backward #'re-search-backward #'re-search-forward))
	    (count (or arg 1))
	    (origin (point))
	    last-element)
    (if backward (beginning-of-line) (end-of-line))
    (while (and (> count 0) (funcall search-fn re nil t))
      (let ((element (save-excursion
		               (goto-char (match-beginning 0))
		               (save-match-data (org-element-at-point)))))
	    (when (and (memq (org-element-type element)
			             '(center-block comment-block dynamic-block
					                    example-block export-block quote-block
					                    special-block src-block verse-block
                                        ;; NOTE: This is the only change. We've just added `babel-call' and `inline-babel-call'.
                                        babel-call inline-babel-call))
		           (<= (match-beginning 0)
		               (org-element-property :post-affiliated element)))
	      (setq last-element element)
	      (cl-decf count))))
    (if (= count 0)
	    (prog1 (goto-char (org-element-property :post-affiliated last-element))
	      (save-match-data (org-show-context)))
      (goto-char origin)
      (user-error "No %s code blocks" (if backward "previous" "further")))))

;; Bind `C-c C-v C-p' to `org-previous-src-block-or-inline-call'.
(define-key org-mode-map (kbd "C-c C-v C-p") 'org-previous-src-block-or-inline-call)
;; Bind `C-c C-v C-n' to `org-next-src-block-or-inline-call'.
(define-key org-mode-map (kbd "C-c C-v C-n") 'org-next-src-block-or-inline-call)


;; Call `org-display-inline-images' on the current subtree.
(defun org-display-inline-images-in-subtree ()
  "Display inline images in the current subtree."
  (interactive)
  (let ((current-num-inline-images (length org-inline-image-overlays))
        (start (save-excursion (org-back-to-heading) (point)))
        (end (save-excursion (org-end-of-subtree) (point))))
    (org-display-inline-images nil t start end)
    (when (called-interactively-p 'interactive)
      (message "Displayed %d inline images"
               (- (length org-inline-image-overlays) current-num-inline-images)))))


;; Very useful when an org file grows too large.
;; Nicely lists the TODOs in the current buffer while providing ease of navigation.
(use-package org-sidebar)

;; Useful for searching in org files.
;; Allows one to jump to a heading whose contents or itself has a match
;; with the search string.
(use-package helm-org-rifle
  :config
  (setq helm-org-rifle-show-path nil))


;; Source: https://marcohassan.github.io/bits-of-experience/pages/emacs/
(fset 'yes-or-no-p 'y-or-n-p)

(defun my/org-add-export-file-name-property ()
  "Add the property `EXPORT_FILE_NAME' to the current heading, if it doesn't already exist, using the title of the heading as the default name."
  (interactive)
  (let ((export-file-name (org-entry-get (point) "EXPORT_FILE_NAME")))
    (if export-file-name
        (message "The property `EXPORT_FILE_NAME' already exists.")
      (let* ((default-name (org-get-heading t t))
             ;; Santiize the default name.
             (default-name (replace-regexp-in-string "[^a-zA-Z0-9_]+" "-" default-name))
             (name (read-string (format "Export file name (default: %s): " default-name) nil nil default-name)))
        (org-entry-put (point) "EXPORT_FILE_NAME" name)))))
