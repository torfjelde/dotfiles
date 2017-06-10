(setq
 inhibit-startup-screen t
 create-lockfiles nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil 
 global-prettify-symbols-mode t
 default-tab-width 4

 org-export-babel-evaluate nil
 
 omnisharp-server-executable-path "/home/tor/omnisharp-server/Omnisharp/bin/Debug/OmniSharp.exe"

 ;; slime
 inferior-lisp-program "/usr/local/bin/sbcl"
 slime-lisp-implementations '((sbcl ("sbcl")))
 ;; lisp-indent-function 'common-lisp-indent-function
 ;; slime-complete-symbol-function 'slime-fuzzy-complete-symbol
 )

;; PEP-8 tells me not to use tabs..so by defalt we disable this
(setq-default indent-tabs-mode nil)


;;; Custom functions for note-taking ;;;
(defun notes:code-directory ()
    (let* ((filepath (buffer-file-name))
           (directory (file-name-directory filepath))
           (filebase (file-name-base filepath)))
      (concat
       directory
       (file-name-as-directory (concat "." filebase))
       (file-name-as-directory "code"))))

(defun notes:code-file-path (filename)
  (let ((code-dir (notes:code-directory)))
    (concat code-dir filename)))


;;; OS specific variables ;;;
(cond
 ;; Windows
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (message "Microsoft Windows")))
 
 ;; Mac OS X
 ;; We want to disable left-cmd and bind left-option to Meta
 ;; due to terminal apps using left-cmd for stuff, and I
 ;; want uniform bindings independent of the environment.
 ;; These variables are for this version of Emacs for Mac OS X:
 ;; https://bitbucket.org/mituharu/emacs-mac
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (message "Mac OS X")
    (setq mac-command-modifier nil  ;; disables bindings to left-cmd on Mac
		  mac-option-modifier (quote (:ordinary meta :function alt :mouse alt))  ;; binds left-option to Meta
		  mac-right-option-modifier nil)  ;; disables it as a modifier so we can type properly, e.g. "[]|∞≈"

    (setenv "PATH"
	(concat
	 "/usr/local/bin:"
	 (getenv "PATH"))
	(setq racer-rust-src-path "/Users/tef/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src/"))))

 ;; Linux
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (message "Linux")
    (setenv "PATH"
	(concat
	 "/usr/local/bin:"
	 (getenv "PATH"))
	;; need to do `rustup component add rust-src' for `racer' to work
	(setq racer-rust-src-path "/home/tor/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/")))))

;; (add-to-list 'exec-path "/usr/local/bin/:")

;; fancy letters
(defun pretty-greek ()
  (let ((greek '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "rho" "sigma_final" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega")))
    (loop for word in greek
          for code = 97 then (+ 1 code)
          do  (let ((greek-char (make-char 'greek-iso8859-7 code))) 
                (font-lock-add-keywords nil
                                        `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[a-zA-Z]")
                                           (0 (progn (decompose-region (match-beginning 2) (match-end 2))
                                                     nil)))))
                (font-lock-add-keywords nil 
                                        `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[^a-zA-Z]")
                                           (0 (progn (compose-region (match-beginning 2) (match-end 2)
                                                                     ,greek-char)
                                                     nil)))))))))

;; prettify-list
(defun my-prettiest-symbols () 
  (setq prettify-symbols-alist
		'(
		  ("lambda" . 955) ; λ
		  ("->" . 10140)    ; →
		  ("=>" . 10233)    ; ⇒
		  )))

;; convience functions
(defun my/open-block-c-mode (id action context)
  (when (eq action 'insert)
	(newline)
	(indent-according-to-mode)
	(previous-line)
	(indent-according-to-mode)))

(defun my/duplicate-line ()
  "Copies current line to next line. Like C-d in Pycharm"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line)
  (yank))

(defun org-babel-python-strip-session-chars ()
  "Remove >>> and ... from a Python session output."
  (when (and (string=
              "python"
              (org-element-property :language (org-element-at-point)))
             (string-match
              ":session"
              (org-element-property :parameters (org-element-at-point))))
    (save-excursion
      (when (org-babel-where-is-src-block-result)
        (goto-char (org-babel-where-is-src-block-result))
        (end-of-line 1)
        ;(while (looking-at "[\n\r\t\f ]") (forward-char 1))
        (while (re-search-forward
                "\\(>>> \\|\\.\\.\\. \\|: $\\|: >>>$\\)"
                (org-element-property :end (org-element-at-point))
                t)
          (replace-match "")
          ;; this enables us to get rid of blank lines and blank : >>>
          (beginning-of-line)
          (when (looking-at "^$")
            (kill-line)))))))

;; global keys
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c C-d") 'my/duplicate-line)
(global-set-key (kbd "C-;") 'iedit-mode)

;; the package manager
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/"))
 package-archive-priorities '(("melpa-stable" . 1)))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; notes about use-package ;;
;; :init - executes BEFORE loading package
;; :config - executes AFTER loading package

;;; general ;;;
;; org-mode
(use-package ob-http)
(use-package ob-ipython)
(use-package org
  ;; :pin org
  :bind (("C-c l" . org-store-link))
  :init
  (progn
    (setq org-confirm-babel-evaluate nil
		  org-export-headline-levels 5
		  org-export-with-toc 2
		  org-export-babel-evaluate nil))
  :config
  (progn
    (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
    (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

	;; Custom hooks
	(add-hook 'org-mode-hook 'pretty-greek)
	(add-hook 'org-mode-hook 'my-prettiest-symbols)

	(font-lock-add-keywords 'org-mode
							'(("^ +\\([-*]\\) "
							   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    ;; org-agenda / org-capture
    (setq org-default-notes-file "~/Dropbox/org/gtd.org")
    (setq org-refile-targets '(("~/Dropbox/org/gtd.org" :maxlevel . 2) 
			       ("~/Dropbox/org/someday.org" :level . 2)))
    (setq org-capture-templates
	  '(("t"        ;; shortcut
	     "Todo"     ;; title
	     entry      ;; type of template
	     (file+headline "~/Dropbox/org/gtd.org" "Tasks")  ;; what and where to add
	     "* TODO %^{Brief Description} %^g\n  Entered on %U\n  %?\n  %i\n  %a"  ;; template
	     :empty-lines 1 ;; property
	     )
	    
	    ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
	     "* %?\nEntered on %U\n  %i\n  %a")
	    
	    ("i" "Idea" item (file "~/Dropbox/org/ideas.org"))))
    (setq org-agenda-custom-commands
	  '(("p" . "PROJECT+Name tags searches")
	    ("pi" tags "+PROJECT+My")
	    ("po" tags "+PROJECT+Octochain")
	    ("pm" tags "+PROJECT+Masterloop")))

    ;; babel
    (require 'ob-clojure)
    (setq org-babel-clojure-backend 'cider)
    (add-hook 'org-babel-after-execute-hook 'org-babel-python-strip-session-chars)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((shell . t)
	   (C . t)
       (dot . t)
       (sql . t)
       (clojure . t)
       (python . t)
	   (ein . t)
       (ipython . t)
       (scala . t)
       ;; (rust . t)
	   (haskell . t)
       ;; (csharp. t)
       (ditaa . t)))))

(use-package org-bullets
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

;; flycheck
(use-package flycheck
  :pin melpa-stable
  :init
  (progn
	;; uncomment below if you're having issues with flycheck performance
	;; (setq 'flycheck-highlighting-mode 'lines) 
	(add-hook 'after-init-hook #'global-flycheck-mode)))

;; company
(use-package company
  :config
  (progn
	(add-hook 'prog-mode-hook 'company-mode)
    (add-to-list 'company-backends '(company-jedi :with company-capf))
    (add-to-list 'company-backends 'ein:company-backend)
	(add-to-list 'company-backends '(company-irony-c-headers
									 company-irony))))
(use-package company-quickhelp)

;; yasnippet
(use-package yasnippet
  :pin melpa-stable
  :init (yas-global-mode))

;; undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode))

;; smartparens
(use-package smartparens
  :init
  (progn
    (require 'smartparens-config)
    (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
    (add-hook 'prog-mode-hook 'show-paren-mode t)))

;; edit server
(use-package edit-server
  :init (edit-server-start))

;; helm
(use-package helm
  :diminish helm-mode  ;; removes the helm-mode from the mode-line
  :init
  (progn
    (require 'helm-config)
    (helm-mode))
  :bind (("M-x" . helm-M-x)))

(use-package helm-descbinds
  :bind (("C-h b" . helm-descbinds)
	 ("C-h w" . helm-descbinds)))

(use-package projectile
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)
    (add-to-list 'projectile-globally-ignored-files "node-modules")
	(projectile-global-mode)))
(use-package helm-projectile)

;; dirtree
(use-package dirtree)

;; multiple cursors
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)))

(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode))
(use-package ace-window
  :config (global-set-key (kbd "M-p") 'ace-window))
(use-package iedit)
(use-package rainbow-delimiters)
(use-package centered-cursor-mode)
(use-package htmlize)
(use-package magit)










;;; programming languages ;;;
(add-hook 'prog-mode-hook 'my-prettiest-symbols)

;; provides highlighting for TODO, FIXME and BUG in comments
(use-package fic-mode
  :config
  (progn
	(add-hook 'prog-mode-hook 'fic-mode)
	(set-face-attribute 'fic-author-face nil :foreground "dark violet" :underline t)
	(set-face-attribute 'fic-face nil :foreground "magenta" :weight 'bold)))

;; c/c++
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's asynchronous function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
	'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
	'irony-completion-at-point-async))

(use-package irony
  :config
  (progn
	;; Windows performance tweaks
    (when (boundp 'w32-pipe-read-delay)
      (setq w32-pipe-read-delay 0))
    ;; Set the buffer size to 64K on Windows (from the original 4K)
    (when (boundp 'w32-pipe-buffer-size)
      (setq irony-server-w32-pipe-buffer-size (* 64 1024)))))

(use-package company-irony-c-headers)
(use-package company-irony)
(use-package cc-mode
  :bind (("C-c o" . ff-find-other-file)
	 ("C-c C-d" . my/duplicate-line))
  :config
  (progn
	(add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'objc-mode-hook 'irony-mode)

    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (c-set-offset 'case-label '+)
    (sp-local-pair 'c-mode "{" nil :post-handlers '((my/open-block-c-mode "RET")))
    (sp-local-pair 'c++-mode "{" nil :post-handlers '((my/open-block-c-mode "RET")))))

(use-package arduino-mode
  :mode "\.\\(pde\\|ino\\).?$"
  :config (sp-local-pair 'arduino-mode
			 "{" nil :post-handlers '((my/open-block-c-mode "RET"))))

;; lisp
;; (add-to-list 'load-path "/Users/tef/quicklisp/dists/quicklisp/software/slime-2.14")
;; (require 'slime)
(defvar slime-repl-font-lock-keywords lisp-font-lock-keywords-2)
(defun slime-repl-font-lock-setup ()
  (setq font-lock-defaults
		'(slime-repl-font-lock-keywords
		  ;; From lisp-mode.el
		  nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
		  (font-lock-syntactic-face-function
		   . lisp-font-lock-syntactic-face-function))))

(defadvice slime-repl-insert-prompt (after font-lock-face activate)
	(let ((inhibit-read-only t))
	  (add-text-properties
	   slime-repl-prompt-start-mark (point)
	   '(font-lock-face
		 slime-repl-prompt-face
		 rear-nonsticky
		 (slime-repl-prompt read-only font-lock-face intangible)))))

;; (add-to-list 'load-path "/Users/tef/.emacs.d/elpa/slime-2.19/contrib/")
(use-package "slime-company")
(use-package "slime"
  ;; :mode "\\.lisp\\.?$"
  
  :init
  (progn
	;; (require 'slime-repl)
	;; (add-hook 'slime-repl-mode-hook 'slime-repl-font-lock-setup)
	(setq slime-net-coding-system 'utf-8-unix)
	(slime-setup '(slime-fancy slime-company))
	(slime-setup '(slime-fancy slime-company))
	(setq slime-enable-evaluate-in-emacs t)
	))

(use-package "eldoc"
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (progn
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

;; clojure
(use-package clojure-mode)
(use-package cider
  :init
  (progn
    (setq cider-cljs-lein-repl
	  "(do (require 'figwheel-sidecar.repl-api)
		   (figwheel-sidecar.repl-api/start-figwheel!)
		   (figwheel-sidecar.repl-api/cljs-repl))")))

;; rust
(use-package rust-mode
  :init
  (progn
	(add-hook 'rust-mode-hook 'pretty-greek)
	(add-hook 'rust-mode-hook 'my-prettiest-symbols)))
(use-package racer
  :bind (("C-c TAB" . company-indent-or-complete-common))
  :init
  (progn
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'rust-mode-hook #'eldoc-mode)
    (setq company-tooltip-align-annotations t)))
(use-package company-racer)

;; java


;; scala
(use-package scala-mode
  :pin melpa-stable
  :mode "\\.scala\\.?$")
(use-package ensime
  :pin melpa-stable)

;; groovy
(use-package groovy-mode
  :config
  (progn
    (sp-local-pair 'c-mode "{" nil :post-handlers '((my/open-block-c-mode "RET")))))

;; c# / c-sharp
(use-package csharp-mode
  :mode "\\.cs\\.?$"
  :pin melpa-stable
  :config (sp-local-pair 'csharp-mode "{" nil :post-handlers '((my/open-block-c-mode "RET"))))

;; (use-package omnisharp
;;   :init (setq omnisharp-server-executable-path "/Users/tef/omnisharp-server/Omnisharp/bin/Debug/OmniSharp.exe")
;;   :config (add-to-list 'company-backends 'company-omnisharp))

;; web development
;; from FAQ at http://web-mode.org/ for smartparens
(defun my/web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil))

(defun my/sp-web-mode-is-code-context (id action context)
  (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
                (get-text-property (point) 'block-side)))))

(use-package web-mode
  :mode "\\.\\(html?\\|jinja\\).$"
  :config
  (progn
    (add-hook 'web-mode-hook  'my/web-mode-hook)
    (sp-local-pair 'web-mode "<" nil :when '(my/sp-web-mode-is-code-context))
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-ac-sources-alist
	  '(("css" . (ac-source-css-property))
	    ("html" . (ac-source-words-in-buffer ac-source-abbrev)))
	  )))

;; Allows for 'div.className' + C-j => "<div class='className'></div>"
(use-package emmet-mode
     :init
     (progn
       (add-hook 'web-mode-hook 'emmet-mode)))
(use-package helm-emmet)

;; javascript
(use-package js2-mode
  :mode "\\.js\\.?$")
(use-package skewer-mode
  :init
  (progn
    (add-hook 'js2-mode-hook 'skewer-mode)
    (add-hook 'css-mode-hook 'skewer-css-mode)
    (add-hook 'html-mode-hook 'skewer-html-mode))
  :config (skewer-setup))

;; js autocomplete server. Requires "npm install -g tern" too.
(use-package tern
  :config
  (progn
    (bind-key "C-c C-c" 'compile tern-mode-keymap)
    (when (eq system-type 'windows-nt) (setq tern-command '("cmd" "/c" "tern")))
    (add-hook 'js2-mode-hook 'tern-mode)
    (setq company-tern-property-marker nil)))

(use-package company-tern
  :init (add-to-list 'company-backends 'company-tern))

;; typescript
(use-package typescript-mode
  :init (sp-local-pair 'csharp-mode "{" nil :post-handlers '((my/open-block-c-mode "RET"))))

;; python
(use-package jedi
  :pin melpa-stable
  :config
  (progn
    (jedi:setup)))

(use-package company-jedi
  :pin melpa-stable)

;; (use-package elpy
;;   :pin melpa
;;   :config
;;   (progn
;;     (when (require 'flycheck nil t)
;; 	  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;; 	  (add-hook 'elpy-mode-hook 'flycheck-mode)))
;;   )

(use-package python
  :mode ("\\.py\\.?$" . python-mode)
  :pin melpa-stable
  :config
  (progn
	(add-hook 'python-mode-hook 'pretty-greek)
    ;; (add-hook 'python-mode-hook 'jedi-mode)
	(add-hook 'python-mode-hook 'elpy-mode)
    (add-hook 'python-mode-hook
	  (lambda ()
	    (progn
	      (setq electric-indent-chars (delq ?: electric-indent-chars)))))
    ))

(use-package ein
  :init (progn
          (setq ein:use-smartrep t)
          (add-hook 'ein:notebook-mode-hook 'company-mode)))

;; haskell
(use-package haskell-mode
  :mode "\\.hs\\.?$"
  :init (progn
		  ;; buffer-local variable so to use `hlint'
		  ;; had performance issues with using `stack-ghc-lint'
		  (add-hook 'haskell-mode-hook
					(lambda ()
					  (setq flycheck-checker 'haskell-ghc)))))


;; XAML stuff
(add-hook 'nxml-mode-hook 'turn-on-smartparens-mode)
(add-hook 'nxml-mode-hook 'show-paren-mode)

;; YAML
(use-package yaml-mode
  :pin melpa-stable
  :mode "\\.yaml\\'")

;; cassandra CQL
(use-package cql-mode
  :mode "\\.cql?$")


(add-hook 'lisp-mode-hook 'pretty-greek)
(add-hook 'emacs-lisp-mode-hook 'pretty-greek)

;;; themes ;;;
(use-package solarized-theme)
(use-package darktooth-theme)
(use-package atom-one-dark-theme)
  ;; :init
  ;; (add-hook 'after-make-frame-functions
  ;;         '(lambda (frame)
  ;;           (select-frame frame)
  ;;           (if window-system
  ;;               nil
  ;; 	      (set-frame-parameter nil 'background-color "#2B2B2B")
  ;; 	      ))))

(enable-theme 'atom-one-dark)

(use-package smart-mode-line)
(use-package spaceline
  :init
  (progn
	(require 'spaceline-config)
	(spaceline-emacs-theme)))

(let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                             ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                             ((x-list-fonts "Verdana")         '(:font "Verdana"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

	  ;; some settings for makin headings and bullets nicer
	  (custom-theme-set-faces 'user
							  `(org-level-8 ((t (,@headline ,@variable-tuple))))
							  `(org-level-7 ((t (,@headline ,@variable-tuple))))
							  `(org-level-6 ((t (,@headline ,@variable-tuple))))
							  `(org-level-5 ((t (,@headline ,@variable-tuple))))
							  `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
							  `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
							  `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
							  `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
							  `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(asana-my-tasks-project-id nil t)
 '(asana-selected-workspace (quote ((id . 31298705178401) (name . "octochain"))) t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (atom-one-dark)))
 '(custom-safe-themes
   (quote
    ("a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "36d92f830c21797ce34896a4cf074ce25dbe0dabe77603876d1b42316530c99d" "b04425cc726711a6c91e8ebc20cf5a3927160681941e06bc7900a5a5bfe1a77f" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "5ecdec97d6697c14c9cbbd634ac93979d199e3f65b7d60d2c2c357bcb40e2821" "59171e7f5270c0f8c28721bb96ae56d35f38a0d86da35eab4001aebbd99271a8" "47aa6e82734866b2915781c6e1d9517bd897d45fe8aec360dd4b6294fec73068" "6254372d3ffe543979f21c4a4179cd819b808e5dd0f1787e2a2a647f5759c1d1" default)))
 '(ein:console-args (quote ("--simple-prompt")))
 '(ein:org-execute-timeout 999999)
 '(elpy-rpc-python-command "python3")
 '(fci-rule-color "#3E4451")
 '(flycheck-clang-language-standard "c++11")
 '(flycheck-disabled-checkers nil)
 '(flycheck-gcc-language-standard "c++11")
 '(flycheck-highlighting-mode (quote symbols))
 '(flycheck-scss-executable "/usr/local/bin/sass")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(js-indent-level 2)
 '(magit-diff-use-overlays nil)
 '(nyan-mode t)
 '(org-agenda-files
   (quote
    ("~/Dropbox/notes/projects/octochain/tasks.org" "~/Dropbox/notes/projects/octochain/octo.org" "~/Dropbox/org/gtd.org")))
 '(org-babel-python-command "python3")
 '(org-edit-src-content-indentation 0)
 '(org-emphasis-alist
   (quote
    (("*" bold)
     ("/" italic)
     ("`" underline)
     ("=" org-verbatim verbatim)
     ("~" org-code verbatim)
     ("+"
      (:strike-through t)))))
 '(org-export-babel-evaluate nil)
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-journal-dir "~/Dropbox/org/journal")
 '(org-reveal-single-file t)
 '(org-src-block-faces nil)
 '(org-src-preserve-indentation nil)
 '(org-src-tab-acts-natively nil)
 '(package-selected-packages
   (quote
    (fic-mode company-web company-lua lua-mode elpy helm-gitlab exec-path-from-shell pug-mode ein company-irony-c-headers company-irony ace-jump-mode irony spotify helm-spotify-plus company-go go-mode ox-asciidoc ox-rst sparql-mode slime-company slime ess nyan-mode spaceline smart-mode-line smart-mode-line-powerline-theme haskell-mode org-journal calfw json-mode pytest realgud cmake-ide rtags org-tree-slide ox-reveal org-bullets graphviz-dot-mode dot-mode toml-mode cargo dockerfile-mode org-ref ob-ipython markdown-preview-mode gitlab yaml-mode web-mode use-package undo-tree typescript-mode solarized-theme smartparens skewer-mode rainbow-delimiters racer org omnisharp ob-http multiple-cursors magit iedit htmlize helm-projectile helm-emmet helm-descbinds groovy-mode ensime edit-server dirtree darktooth-theme cql-mode company-tern company-racer company-quickhelp company-jedi cider centered-cursor-mode atom-one-dark-theme arduino-mode ace-window)))
 '(python-indent-guess-indent-offset nil)
 '(python-shell-completion-native-disabled-interpreters (quote ("pypy" "ipython")))
 '(python-shell-interpreter "ipython3")
 '(python-shell-interpreter-args "--simple-prompt -i")
 '(python-shell-interpreter-interactive-arg "")
 '(racer-rust-src-path
   "/Users/tef/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src/" t)
 '(recentf-keep (quote (recentf-keep-default-predicate)))
 '(rtags-path "/Users/tef/rtags/bin/")
 '(safe-local-variable-values (quote ((org-export-with-toc))))
 '(scroll-bar-mode nil)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(sml/mode-width
   (if
       (eq
        (powerline-current-separator)
        (quote arrow))
       (quote right)
     (quote full)))
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active1)
                   nil)))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   nil
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active2)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes)))
 '(sml/theme (quote dark))
 '(spaceline-helm-mode t)
 '(tab-width 4)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(tool-bar-position (quote top))
 '(tramp-mode t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fic-author-face ((t (:foreground "dark violet" :underline t))))
 '(fic-face ((t (:foreground "magenta" :weight bold))))
 '(org-block ((t (:inherit shadow :background "gray20"))))
 '(org-block-begin-line ((t (:inherit org-meta-line :background "grey15" :height 0.9))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif"))))
 '(spaceline-evil-visual ((t (:background "gray" :foreground "#3E3D31" :inherit (quote mode-line)))))
 '(spaceline-unmodified ((t (:background "DarkGoldenrod2" :foreground "#3E3D31" :inherit (quote mode-line))))))
(put 'narrow-to-region 'disabled nil)
