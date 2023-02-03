(setq
 desktop-restore-forces-onscreen nil ;; fixes an error occurring when using restoring `desktop'

 inhibit-startup-screen t
 inhibit-splash-screen t
 create-lockfiles nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.5
 use-package-always-ensure t
 sentence-end-double-space nil 
 global-prettify-symbols-mode t
 default-tab-width 4

 system-time-locale "C" ;; ensures that week-days follows English convention, e.g. Thu and Wed

 org-adapt-indentation nil  ;; don't indent text in a section to align with section-level
 org-export-allow-bind-keywords t  ;; allows us to set variables in setup-files for project
 org-preview-latex-image-directory "~/.ltximg/"  ;; this '/' at the end is VERY important..

 org-edit-src-content-indentation 0
 org-babel-inline-result-wrap "%s"
 org-emphasis-alist '(("*" bold)
		      ("/" italic)
		      ("_" default)
		      ("=" org-verbatim verbatim)
		      ("~" org-code verbatim)
		      ("+"
		       (:strike-through t)))

 bibtex-completion-pdf-field "file"

 ;; TeX stuff
 TeX-source-correlate-start-server t  ;; clicking in document takes you to source

 ;; set the backup folder to be the temp-folder
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))

 ;; omnisharp-server-executable-path "/home/tor/omnisharp-server/Omnisharp/bin/Debug/OmniSharp.exe"

 ;; slime
 inferior-lisp-program "/usr/local/bin/sbcl"
 slime-lisp-implementations '((sbcl ("sbcl")))
 ;; lisp-indent-function 'common-lisp-indent-function
 ;; slime-complete-symbol-function 'slime-fuzzy-complete-symbol

 safe-local-variable-values '((visual-line-mode . 1)
			      (visual-line-mode . t)
			      (org-export-with-toc)
			      (org-after-todo-state-change-hook . tor/reading-list-done-hook)
			      (org-after-todo-state-change-hook . tor/impl-list-done-hook))
 )

(setq-default fill-column 100)

;; change the font-size a bit
(set-face-attribute 'default nil :height 64)

;; TODO: make of these sweeties
(defun eval-region-as-sympy-simplify ()
  "Evaluate selection as a python expression, replacing it with the result"
  (interactive)
  (shell-command-on-region-and-select
   (region-beginning)
   (region-end)
   "python -c 'import sys; from sympy import latex; from sympy.parsing.latex import parse_latex; sys.stdout.write(latex(parse_latex(str(sys.stdin.read())).simplify()))'" 0 t))

(defun eval-region-as-sympy-expand ()
  "Evaluate selection as a python expression, replacing it with the result"
  (interactive)
  (shell-command-on-region-and-select
   (region-beginning)
   (region-end)
   "python -c 'import sys; from sympy import latex; from sympy.parsing.latex import parse_latex; sys.stdout.write(latex(parse_latex(str(sys.stdin.read())).expand()))'" 0 t))

(defun eval-region-as-sympy-sum ()
  "Evaluate selection as a python expression, replacing it with the result"
  (interactive)
  (shell-command-on-region-and-select
   (region-beginning)
   (region-end)
   "python -c 'import sys; from sympy import latex; from sympy.parsing.latex import parse_latex; sys.stdout.write(latex(parse_latex(str(sys.stdin.read())).doit()))'" 0 t))

(defun eval-region-as-sympy-integrate ()
  "Evaluate selection as a python expression, replacing it with the result"
  (interactive)
  (shell-command-on-region-and-select
   (region-beginning)
   (region-end)
   "python -c 'import sys; from sympy import latex; from sympy.parsing.latex import parse_latex; sys.stdout.write(latex(parse_latex(str(sys.stdin.read())).integrate()))'" 0 t))

(defun clone-if-not-exists (remoteurl targetdir)
  (let* ((parentdir (file-name-directory (directory-file-name targetdir))))
    (when (not (file-directory-p targetdir))
      (message "%s not present; cloning from %s..." targetdir remoteurl)

      ;; check if the parent-directory exists, and create if it doesn't
      (if (not (file-directory-p parentdir))
          (make-directory parentdir))

      ;; clone and stuff
      (if (eq (shell-command (format "git -C %s clone %s" parentdir remoteurl)) 0)
          (progn
            (message "Cloning of %s successful!" remoteurl)
            t)
        (progn
          (warn "Cloning of %s FAILED!" remoteurl)
          (warn (format "git -C %s clone %s" parentdir remoteurl))
          nil)))))


(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))
;; (global-set-key (kbd "C-c e") 'eval-and-replace)


;; https://emacs.stackexchange.com/a/34900
(defun shell-command-on-region-and-select
    (start end command
           &optional output-buffer replace
           error-buffer display-error-buffer
           region-noncontiguous-p)
  "Wrapper for 'shell-command-on-region', re-selecting the output.

Useful when called with a selection, so it can be modified in-place"
  (interactive)
  (let ((buffer-size-init (buffer-size)))
    (shell-command-on-region
     start end command output-buffer replace
     error-buffer display-error-buffer
     region-noncontiguous-p)
    (setq deactivate-mark t)
    (setq end (+ end (- (buffer-size) buffer-size-init)))
    ;; (set-mark start)
    (goto-char end)
    (activate-mark)
    ))

;; https://stackoverflow.com/a/25212377/4956107
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

;; (global-set-key (kbd "C-c e p") 'eval-and-replace)

;; Allows you to fold everything on a indentation-level greater than the current.
;; Source: https://stackoverflow.com/a/4459159
(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))
(global-set-key [(M C i)] 'aj-toggle-fold)

;; Tor's keybindings
(defun tor/duplicate-downward (begin end)
  "https://emacs.stackexchange.com/a/32515"
  (interactive "r")
  (let (deactivate-mark (point (point)))
    (insert (buffer-substring begin end))
    (push-mark point)))

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
;; (global-set-key (kbd "C-;") 'iedit-mode)

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

;; notes about use-package ;;
;; :init - executes BEFORE loading package
;; :config - executes AFTER loading package
(require 'use-package)

;; required by some OS specific stuff
(use-package exec-path-from-shell)

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
    (exec-path-from-shell-initialize)
	(setq racer-rust-src-path "/Users/tef/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src/")))

 ;; Linux
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (message "Linux")
    (exec-path-from-shell-initialize)            
	;; need to do `rustup component add rust-src' for `racer' to work
    (setq racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/")

    ;; use xclip to yank, allowing you to yank in terminal to the GLOBAL clipboard
    (use-package xclip
      :init (xclip-mode))
    )))

;;; xclip

;; pdf-tools - much improved way to view pdfs
;; IMPORTANT: need to run `(pdf-tools-install)' to install dependencies
(use-package pdf-tools
  :pin melpa
  :mode ("\\.vpdf\\.?$" . pdf-virtual-edit-mode)
  :init (progn
          (if (string-equal system-type "gnu/linux") (pdf-tools-install))
          ;; copied from the source-code, but uses `org-mode' as default major-mode for text-annotations
          (setq pdf-annot-edit-contents-setup-function
                (lambda (a)
                   (let ((mode (if (funcall pdf-annot-latex-string-predicate
                                            (pdf-annot-get a 'contents))
                                   'latex-mode
                                 'org-mode)))
                     (unless (derived-mode-p mode)
                       (funcall mode)))))))

;; PACKAGES
;; AucTeX
;; (require 'auctex)
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %S%(PDFout)")))
;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)
(add-hook 'LaTeX-mode-hook
          (lambda()
            (local-unset-key (kbd "C-c ]"))))

(use-package company-reftex
  :init (setq company-reftex-labels-regexp (rx "\\"
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
                                                   "propref"
                                                   "thmref"
                                                   "lemref"
                                                   "lemmaref"
                                                   "appref"
                                                   "assumptref"
                                                   "secref")
                                               "{"
                                               (group (* (not (any "}"))))
                                               (regexp "\\="))))
(use-package company-auctex
  :init (progn
          (company-auctex-init)
          (add-hook 'LaTeX-mode-hook 'company-mode)
          (add-hook 'latex-mode-hook 'company-mode)
          (add-hook 'LaTeX-mode-hook 'reftex-mode)

          ;; Means that we get
          (add-to-list 'company-backends 'company-reftex-labels)
          (add-to-list 'company-backends 'company-reftex-citations)
          ))

;; anki-editor
(use-package anki-editor
  :pin melpa
  :init (progn
          (setq anki-editor-break-consecutive-braces-in-latex t)))

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
                                     company-irony))
    ))
;; Additional stuff
(use-package company-quickhelp)

;; yasnippet
(use-package yasnippet
  :pin melpa-stable
  :init (progn
          (setq yas-triggers-in-field t ;; Enable nested triggering of snippets
                yas-indent-line 'fixed ;; Ensures that the indentation is done after my choosing
                )
          (yas-global-mode)))

(use-package undo-tree
  :diminish undo-tree-mode
  :init (global-undo-tree-mode))

(use-package smartparens
  :init
  (progn
    (require 'smartparens-config)
    (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
    (add-hook 'prog-mode-hook 'show-paren-mode t)))

(use-package edit-server
  :pin melpa
  :init (progn
          ;; Starts the edit server
          (edit-server-start)

          ;; We can set major-modes for different domains!!!
          (setq edit-server-url-major-mode-alist
                '(("github\\.com" . poly-markdown-mode)))
          )
  )

(use-package visual-fill-column
  :init (progn
          (setq-default visual-fill-column-center-text t)
          (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
          (add-hook 'prog-mode-hook #'visual-fill-column-mode)))

(use-package which-key
  :pin melpa
  :config (which-key-mode))

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

;; Project stuff
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

(use-package magit
  :pin melpa)

;; (use-package forge
;;   :pin melpa)


;; Navigation

;; dirtree
(use-package dirtree)

(use-package avy
  :bind ("M-j" . avy-goto-word-or-subword-1))

(use-package ace-window
  :config (global-set-key (kbd "M-[") 'ace-window))

;; Editing
(use-package multiple-cursors
  :bind (("C->" . mc/mark-next-like-this)))

(use-package iedit
  :bind ("C-c ,"))

;; Visual
(use-package rainbow-delimiters)
(use-package centered-cursor-mode)
(use-package htmlize)

(use-package default-text-scale
  :pin melpa)


(message "Parsing programming setup")


;;; programming languages ;;;
(add-hook 'prog-mode-hook #'my-prettiest-symbols)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook
 'prog-mode-hook
 (lambda ()
   ;; Fixes the width of the line-numbers; if I end up having to edit a file with more than 99 999 lines of code, I'll throw my computer through the wall anyways.
   (setq display-line-numbers-width 5)
   ;; Don't use word-wrap in programming mode; I want to see if it wraps
   (setq word-wrap nil)
   ;; Default to 100 linewidth in programming languages, because I like it.
   (set-fill-column 100)))


;; provides highlighting for TODO, FIXME and BUG in comments
(use-package fic-mode
  :config
  (progn
	(add-hook 'prog-mode-hook 'fic-mode)
	(set-face-attribute 'fic-author-face nil :foreground "dark violet" :underline t)
	(set-face-attribute 'fic-face nil :foreground "magenta" :weight 'bold)
    (add-to-list 'fic-highlighted-words "HACK")))

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
      (setq irony-server-w32-pipe-buffer-size (* 64 1024))))
  :init (progn
          ;; default to C++11
          (setq irony-additional-clang-options '("-std=c++11"))))

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

    ;; used to be set globally but this messed up when opening C files
    (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
    (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))

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

;; COMMENTED SLIME for a faster startup => uncomment if you want to use it
;; (add-to-list 'load-path "/Users/tef/.emacs.d/elpa/slime-2.19/contrib/")
;; (use-package "slime-company")
;; (use-package "slime"
;;   ;; :mode "\\.lisp\\.?$"
  
;;   :init
;;   (progn
;; 	;; (require 'slime-repl)
;; 	;; (add-hook 'slime-repl-mode-hook 'slime-repl-font-lock-setup)
;; 	(setq slime-net-coding-system 'utf-8-unix)
;; 	(slime-setup '(slime-fancy slime-company))
;; 	(slime-setup '(slime-fancy slime-company))
;; 	(setq slime-enable-evaluate-in-emacs t)
;; 	))

(use-package "eldoc"
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (progn
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

;; https://github.com/magnars/dash.el
(use-package dash
  :pin melpa)

(use-package helpful
  :pin melpa-stable)

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
    ;; (add-hook 'rust-mode-hook 'flycheck-rust-setup)  ;; newly added
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

;; scala
(use-package scala-mode
  :pin melpa-stable
  :mode "\\.scala\\.?$")
;; FIXME: apparently `ensime' is done for, and it's replaced by something called `metals'. Probably never going to do Scala again though, so whatever
;; (use-package ensime
;;   :pin melpa-stable)

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

;; Golang
(use-package go-mode
  :mode "\\.go\\.?$"
  :pin melpa-stable
  :config (add-to-list 'company-backends 'company-go))
(use-package company-go)

;; Julia
(use-package julia-mode
  :config
  (progn
    (add-hook
     'julia-mode-hook
     (lambda ()
       ;; shift such that it wraps only if it goes beyond 91
       (set-fill-column (+ 91 display-line-numbers-width 2)))))
  ;; :config
  ;; (progn
  ;;   ;; (load "ess-site")
  ;;   ;; (add-hook 'julia-mode #'ess-julia-mode)
  ;;   ;; overwrite this rebinding from `ess-julia-mode'
  ;;   ;; (bind-key "TAB" 'julia-latexsub-or-indent ess-julia-mode-map))
    )

;; web development
;; from FAQ at http://web-mode.org/ for smartparens
(defun my/web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil))

(defun my/sp-web-mode-is-code-context (id action context)
  (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
                (get-text-property (point) 'block-side)))))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide)

(use-package web-mode
  :mode "\\.\\(html?\\|jinja||tsx\\).$"
  :config
  (progn
    (add-hook 'web-mode-hook  'my/web-mode-hook)
    ;; setup Tide with web-mode
    (add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
    
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
(use-package js2-mode)
(use-package rjsx-mode
  :mode "\\.js\\.?$"
  :config (setq js-indent-level 2))
(use-package skewer-mode
  :init
  (progn
    ;; disable warning on missing semi-colons
    (setq js2-strict-missing-semi-warning nil
          js2-missing-semi-one-line-override nil)
    
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
    (add-hook 'rjsx-mode-hook 'tern-mode)
    (setq company-tern-property-marker nil)))

;; Can't find this thing anymore...
;; (use-package company-tern
;;   :init (add-to-list 'company-backends 'company-tern))

;; typescript
(use-package typescript-mode
  :init (sp-local-pair 'csharp-mode "{" nil :post-handlers '((my/open-block-c-mode "RET"))))

;; R
(use-package ess
  :pin melpa-stable)

;; python
(use-package jedi
  :pin melpa-stable
  :config
  (progn
    (setq jedi:environment-virtualenv (list "virtualenv" "--system-site-packages"))
    (jedi:setup)))

(use-package company-jedi
  :pin melpa-stable)

(use-package elpy
  :pin melpa
  :config
  (progn
    (when (require 'flycheck nil t)
	  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
	  (add-hook 'elpy-mode-hook 'flycheck-mode)))
  )

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
          ;; BUG: this does not currently work for some reason; also I think I need it
          ;; (setq ein:use-smartrep t)
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

;; Lua
(use-package lua-mode
  :pin melpa
  :mode "\\.lua?$")


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

;; END programming

;; org-mode
(defmacro tor/with-local (var val &rest body)
  "Utility temporarily setting setting VAR to VAL and exectuting BODY in this context, then restoring the value of the variable."
  `(let ((prev ,var)
	 (res nil))
     (setq ,var ,val)
     (setq res (progn ,@body))
     (setq ,var prev)
     res))

(defvar tor/latex-publish-directory "./.latex/")

(defun tor/blog-dir-as-relative (dir filename)
  (file-relative-name dir (file-name-directory filename)))

(defun tor/blog-get-latex-directory (plist filename pub-dir)
  (cond
   ((plist-member plist :latex-directory) (file-relative-name (plist-get plist :latex-directory) (file-name-directory filename)))
   ;; ((plist-member plist :assets-directory) (file-relative-name (concat (plist-get plist :assets-directory) "latex/") (file-name-directory filename)))
   ((plist-member plist :project-directory) (file-relative-name (concat (plist-get plist :project-directory) "assets/latex/") (file-name-directory filename)))))

;; TODO: create a customized publishing function
(defun tor/org-html-publish-to-html (plist filename pub-dir)
  "My customized HTML publishing function. Publish an org file to HTML.

PLIST is the property list of the given object.
FILENAME is the filename of the Org file to be published. 
PUB-DIR is the publishing directory.

Return output file name."
  ;; TODO: need to update/republish "local" index if it exists
  (tor/with-local org-preview-latex-image-directory
		  (or (tor/blog-get-latex-directory plist filename pub-dir)
		      tor/latex-publish-directory)
		  (org-html-publish-to-html plist filename pub-dir)))

(defun tor/publish-html (plist filename pub-dir)
  (message "%s" plist)
  (message "%s" filename)
  (message "%s" pub-dir)
  (copy-file filename (concat pub-dir (file-name-nondirectory filename)) t)
  (concat pub-dir (file-name-nondirectory filename)))

;; TODO: format paths properly to avoid recursion and so on.
(defun tor/org-publish-attachment (plist filename pub-dir)
  "Publish a file with no transformation of any kind.

PLIST is the property list for the given project.
FILENAME is the filename of the Org file to be published.  
PUB-DIR is the publishing directory.

Return output file name."
  (org-publish-attachment plist filename pub-dir))

(defun tor/org-publish-attachment-local (plist)
  "Use PLIST to copy the entire base-directory to publishing-directory."
  (shell-command (concat "cp -r " (plist-get plist :base-directory) "/* " (plist-get plist :publishing-directory) "/")))

(defun tor/filename-to-title (filename)
  "Transform FILENAME into title by splitting on _ and concatenating."
  (string-join
   (mapcar #'capitalize
	   (split-string (string-remove-suffix ".org" filename) "\[-_ \]" t))
   " "))

(use-package mustache
  :config (require 'ht))

(defun tor/directory-p (d)
  (string-match-p "\\." d))

(defun tor/org-file-p (p)
  (string-match-p "\\.org" p))

(defun tor/not-org-file-p (p)
  (not (tor/org-file-p p)))

(defun tor/posts-render-front-page (files)
  (let ((mustache-partial-paths '("~/org-blog/templates/"))
	(base-dir (file-truename (plist-get export-options :publishing-directory))))
    (mustache-render "{{> posts }}"
		     (ht ("posts"
			  (-map
			   (lambda (c) (ht ("title" (tor/filename-to-title c))
				      ("link" (concat base-dir c))))
			   (remove-if #'tor/not-org-file-p (directory-files "~/org-blog/posts/"))))))))

(defun tor/prepare-blog-post-publish (export-options)
  (let ((files (remove-if #'tor/not-org-file-p (directory-files "~/org-blog/posts/")))
	(mustache-partial-paths '("~/org-blog/templates/"))
	(base-dir (file-truename (plist-get export-options :publishing-directory))))
    (with-temp-buffer
      (insert (mustache-render "{{> posts }}"
			       (ht ("posts"
				    (-map
				     (lambda (c) (ht ("title" (tor/filename-to-title c))
						("link" (replace-regexp-in-string "\\.org" ".html" c))))
				     files)))))
      (write-region nil nil "~/org-blog/posts/index.html"))))

(defun tor/render-html-preamble (export-options)
  "Renders the HTML preamble. EXPORT-OPTIONS refers to the export options passed by org."
  ;; FIXME: figure out a better way to load this on demand
  (require 'mustache)
  (require 'ht)

  (let ((mustache-partial-paths '("~/org-blog/templates/"))
	(base-dir (file-truename (plist-get export-options :base-directory)))
	(input-file (file-truename (plist-get export-options :input-file))))
    (message base-dir)
    (mustache-render "{{> base }}"
		     (ht ("categories"
			  (-map
			   (lambda (c) (ht ("category" (tor/filename-to-title c))
				      ("link" (concat (file-relative-name
						       (concat base-dir "/" c)
						       (file-name-directory input-file))
						      "/index.html"))))
			   (remove-if #'tor/directory-p (directory-files "~/org-blog/notes/"))))))))

(defun tor/render-html-postamble (export-options)
  "Renders the HTML post-amble. EXPORT-OPTIONS refers to the export options passed by org."
  (require 'mustache)
  ;; (require 'ht)

  (let ((mustache-partial-paths '("~/org-blog/templates/")))
    (mustache-render "{{> footer}}" (ht ("" nil)))))

(defun tor/render-html-preamble--posts (export-options)
  "Renders the HTML preamble for blog-posts. EXPORT-OPTIONS refers to the export options passed by org."
  ;; FIXME: figure out a better way to load this on demand
  (require 'mustache)
  (require 'ht)

  (let ((mustache-partial-paths '("~/org-blog/templates/"))
	(base-dir (file-truename (plist-get export-options :publishing-directory))))
    (message base-dir)
    (mustache-render "{{> base }}"
		     (ht ("categories"
			  `(,(ht ("category" "Posts") ("link" "index.html"))
			    ,(ht ("category" "Wiki") ("link" "../notes/index.html"))
			    ,(ht ("category" "Notes from papers") ("link" "../papers/index.html"))
			    ,(ht ("category" "About me") ("link" "../about.html"))))))))

(defun tor/element--sort-elements-by-raw-value (el1 el2)
  "Compare :raw-value of EL1 and EL2, returning true if EL2 > EL1."
  (string-greaterp (org-element-property :raw-value el2)
		   (org-element-property :raw-value el1)))

(defun tor/element--get-begin (el)
  "Get beginning of EL."
  (org-element-property :begin el))

(defun tor/element--get-end (el)
  "Get end of EL."
  (org-element-property :end el))

(defun tor/reading-list-sort (&optional level)
  "Sort reading list at LEVEL."
  (interactive)
  (let* ((i 0)
	 (headline-level (or level 1))
	 (parsed (org-element-parse-buffer))
	 (headlines (-filter (lambda (el) (= (org-element-property :level el) headline-level)) 
			    (org-element-map parsed 'headline 'identity)))
	 (start (-min (-map 'tor/element--get-begin headlines)))
	 (end (-max (-map 'tor/element--get-end headlines))))
    (delete-region start end)
    (goto-char start)
    (insert (string-join
	     ;; TODO: update indices
	     (-map
	      (lambda (el)
		(progn
		  (setq i (+ i 1))
		  (replace-regexp-in-string "* TODO [0-9]+\\."
					    (format "* TODO %03d." i)
					     el)))
	      (-map 'org-element-interpret-data
			 (sort headlines 'tor/element--sort-elements-by-raw-value)))
	     ""))))

(defun tor/reading-list--get-next-idx (&optional level category)
  "Get index for reading list at LEVEL and ."
  (let* ((headline-level (or level 1))
	 (parsed (org-element-parse-buffer))
	 (headlines (-filter (lambda (el) (and (= (org-element-property :level el) headline-level)
					  ;; FIXME: BROKEN. Grab this from the property-drawer
					  (if category
					      (org-element-property :category el)
					    t)))
			     (org-element-map parsed 'headline 'identity))))
    (+ 1 (-max
	  (or (-filter
	       (lambda (x) (not (= x 0)))
	       (-map (lambda (el)
		       (string-to-number
			(car (split-string
			      (org-element-property :raw-value el) "\\."))))
		     headlines))
	      '(0))))))

(defun tor/list-done-hook (filename)
  "Remove number of completed todo and re-sort reading list."
  (when (and (boundp 'org-state) (string-equal org-state "DONE"))
    (save-excursion
      (with-current-buffer (find-file-noselect filename)
	(goto-char (point-min))
	;; ONLY match one instead of going on a spree here
	(if (re-search-forward "* DONE \\([0-9]+\\)\\." nil t)
	    ;; replace the completed heading            
	    (let ((n (string-to-number (buffer-substring (match-beginning 1) (match-end 1)))))
	      (message (buffer-substring (match-beginning 0) (match-end 0)))
	      (replace-match "* DONE" nil nil nil 0)
	      ;; search for next headings which need to be updated; +1 to their number
	      (message (number-to-string (point)))
	      (message (buffer-name))
	      (goto-char (match-end 0))
	      (while (re-search-forward "* TODO [0-9]+\\." nil t)
		(message (number-to-string n))
		(replace-match (format "* TODO %03d." n))
		(setf n (+ n 1)))))
	;; sort reading-list
	(tor/reading-list-sort)
	))))

(defun tor/reading-list-next-idx ()
  (save-excursion
    (with-current-buffer (find-file-noselect "~/Dropbox/org/reading.org")
      (format "%03d" (tor/reading-list--get-next-idx)))))

;; used to have this `-*- org-after-todo-state-change-hook: tor/reading-list-done-hook; -*-'
;; at the top of `reading.org', but it doesn't quite work for some reason
;; ACTUALLY this is not what's causing the issue I believe, so I reactivated it.
(defun tor/reading-list-done-hook ()
  (tor/list-done-hook "~/Dropbox/org/reading.org"))

(defun tor/impl-list-next-idx ()
  (save-excursion
    (with-current-buffer (find-file-noselect "~/Dropbox/org/implement.org")
      (format "%03d" (tor/reading-list--get-next-idx)))))

(defun tor/impl-list-done-hook ()
  (tor/list-done-hook "~/Dropbox/org/implement.org"))

;; TODO: setup this to properly work
;; currently having issues with inactive timestamps used in the appointments
(defun tor/clocks-to-clocked-string (start end)
  (format "%s--%s"
	  (format-time-string "[%Y-%m-%d %H:%M]" start)
	  (format-time-string "[%Y-%m-%d %H:%M]" end)))

(defun tor/appt-fake-clock-hook ()
  "Create 'fake' clock-in and clock-out entry for appointment with time-range."
  (org-back-to-heading)
  (let* ((hl (org-element-headline-parser 1000))
	 (sch (org-element-property :scheduled hl))
	 (closed (org-element-property :closed hl)))
    (message "%s" hl)
    (when (and sch (or (string-equal (org-element-property :type sch) "active-range")
		       (and (string-equal (org-element-property :type closed) "inactive")
			    (org-element-property :year-end closed))))
      ;; instead of creating the entire entry, we create a small one and replace the values
      (message "clocking in and out")
      (org-clock-in)
      (org-clock-out)

      (if (re-search-forward "CLOCK: \\[.+\\]--\\[.+\\]" nil t 1)
	  (format-time-string "[%Y-%m-%d]" (current-time))
	(replace-match (concat
			"CLOCK: "
			(tor/clocks-to-clocked-string
			 (date-to-time (format "%s %02d:%02d"
					       (current-time)
					       ;; (org-element-property :year-start sch)
					       ;; (org-element-property :month-start sch)
					       ;; (org-element-property :day-start sch)
					       (org-element-property :hour-start sch)
					       (org-element-property :minute-start sch)))
			 (date-to-time (format "%s %02d:%02d"
					       (current-time)
					       ;; (org-element-property :year-end sch)
					       ;; (org-element-property :month-end sch)
					       ;; (org-element-property :day-end sch)
					       (org-element-property :hour-end sch)
					       (org-element-property :minute-end sch)))))))
      (org-clock-update-time-maybe)
      (message "%s" sch))))

(defun tor/latex-export-sqlite-blocks (text backend info)
  "Replaces `sqlite' src blocks by `sql' src blocks, as these are handled by minted."
  (when (org-export-derived-backend-p backend 'latex)
    (with-temp-buffer
      (insert text)
      ;; replace verbatim env by listings
      (goto-char (point-min))
      (replace-string "\\begin{minted}[]{sqlite}" "\\begin{minted}[]{sql}")
      (buffer-substring-no-properties (point-min) (point-max)))))

(use-package ob-http)
;; (use-package ob-ipython
;;   :config (progn
;;             (setq ob-ipython-resources-dir "/tmp/obipy-resources/")

;;             ;; HACK: the one below is an improvement
;;             ;; (advice-add 'ob-ipython--collect-json :before
;;             ;; (lambda (&rest args)
;;             ;;   (when (re-search-forward "{" nil t)
;;             ;;     (backward-char))))
;;             (advice-add 'ob-ipython--collect-json :before
;;                         (lambda (&rest args)
;;                           (let ((start (point)))
;;                             (set-mark (point))
;;                             (while (re-search-forward "{" nil t)
;;                               (backward-char)
;;                               (kill-region (region-beginning) (region-end))
;;                               (re-search-forward "}\n" nil t)
;;                               (set-mark (point)))
;;                             (end-of-buffer)
;;                             (kill-region (region-beginning) (region-end))
;;                             (goto-char start))))))
(use-package ob-sql-mode)

(defun poly-jupyter-eval-region (beg end msg)
  (jupyter-eval-region beg end))

(defun poly-jupyter-mode-setup ()
  (setq-local polymode-eval-region-function #'poly-jupyter-eval-region))

(use-package jupyter
  :pin melpa
  :after (polymode)
  :config (progn
	    (setq org-babel-default-header-args:jupyter-julia '((:async . "yes")
								(:session . "jl")
								(:kernel . "julia-1.5")))
	    (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
								 (:session . "py")
								 (:kernel . "python3")))

            ;; Set `polymode-eval-region-function' to `jupyter-eval-region'
            ;; so we can evaluate in REPL using `M-n v v'.
            (add-hook 'jupyter-repl-interaction-mode-hook #'poly-jupyter-mode-setup)
            ;; Make the keybinding `C-c '' work INSIDE of the code blocks too.
            ;; Combined with the above hook to `jupyter-repl-interaction', we can
            ;; run `jupyter-run-repl' with the cursor inside a code-block to associate a
            ;; buffer to all such code-blocks in this buffer. Then, to ensure that the indirect
            ;; buffer opened using `iedit-indirect-region', we set `jupyter-current-client'
            ;; to the value which it has in the innerchunk that we executed
            ;; `markdown-edit-code-block' in.
            (add-hook
             'jupyter-repl-interaction-mode-hook
             (lambda ()
               (setq-local edit-indirect-after-creation-hook
                           `(lambda () (setq jupyter-current-client ,jupyter-current-client)))))))

;; Set `polymode-eval-region-function' to `jupyter-eval-region'
;; so we can evaluate in REPL using `M-n v v'.
(add-hook 'jupyter-repl-interaction-mode-hook #'poly-jupyter-mode-setup)
;; Make the keybinding `C-c '' work INSIDE of the code blocks too.
;; Combined with the above hook to `jupyter-repl-interaction', we can
;; run `jupyter-run-repl' with the cursor inside a code-block to associate a
;; buffer to all such code-blocks in this buffer. Then, to ensure that the indirect
;; buffer opened using `iedit-indirect-region', we set `jupyter-current-client'
;; to the value which it has in the innerchunk that we executed
;; `markdown-edit-code-block' in.
(add-hook
 'jupyter-repl-interaction-mode-hook
 (lambda ()
   (setq-local edit-indirect-after-creation-hook
               `(lambda () (setq jupyter-current-client ,jupyter-current-client)))))

(use-package org
  :pin org
  :bind (("C-c l" . org-store-link)
         ("C-c C-x C-l" . org-latex-preview))
  :init
  (progn
    ;; `sqlite' not available using `minted', so we change those blocks to std `sql' blocks
    (require 'ox)
    (add-to-list 'org-export-filter-src-block-functions 'tor/latex-export-sqlite-blocks)
    (setq org-confirm-babel-evaluate nil
	  org-export-headline-levels 5
	  org-export-with-toc 2
	  org-export-use-babel t ;; necessary for parsing header-arguments of src-blocks

	  org-latex-listings 'minted ;; use `minted' instead of `listings' when exporting to latex

	  org-src-window-setup 'current-window ;; makes it so that the src block is opened in the current window

	  ;; customization for latex-preview in org-mode
	  org-format-latex-options '(:foreground default
						 :background default
						 :scale 1.5
						 :html-foreground "steelblue"
						 :html-background "Transparent"
						 :html-scale 1.0
						 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
	  )
    ;; disable execution on export UNLESS otherwise specified
    (add-to-list 'org-babel-default-header-args '(:eval . "never-export")))
  :config
  (progn
    (setq org-confirm-babel-evaluate nil
		  org-export-headline-levels 5
		  org-export-with-toc 2
		  org-export-use-babel t ;; necessary for parsing header-arguments of src-blocks 
	  )
    ;; disable execution on export UNLESS otherwise specified
    (add-to-list 'org-babel-default-header-args '(:eval . "never-export"))

    (global-set-key (kbd "C-c å") 'org-agenda)
    (global-set-key (kbd "C-c ¤") 'org-mark-ring-goto)

    ;; https://emacs.stackexchange.com/a/18146
    (require 'bind-key)
    (unbind-key "C-c [" org-mode-map)
    (unbind-key "C-c ," org-mode-map)
    (bind-key "C-c ," 'org-time-stamp-inactive org-mode-map)

    (setcar org-emphasis-regexp-components " \t('\"{[:alpha:]")
    (setcar (nthcdr 1 org-emphasis-regexp-components) "[:alpha:]- \t.,:!?;'\")}\\")
    (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

    ;; Custom hooks
    (add-hook 'org-mode-hook 'pretty-greek)
    (add-hook 'org-mode-hook 'my-prettiest-symbols)

    ;; Disables flycheck when opening src blocks!
    (defun disable-flycheck-in-org-src-block ()
      (flycheck-mode -1))

    (add-hook 'org-src-mode-hook 'disable-flycheck-in-org-src-block)

    (font-lock-add-keywords 'org-mode
			    '(("^ +\\([-*]\\) "
			       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

    ;; org-agenda / org-capture
    (setq org-agenda-files '("~/Dropbox/org/gtd.org"
			     "~/Dropbox/org/school.org"
			     "~/Dropbox/org/reading.org"
			     "~/Dropbox/org/implement.org"))
    (setq org-default-notes-file "~/Dropbox/org/gtd.org")
    (setq org-refile-targets '(("~/Dropbox/org/gtd.org" :maxlevel . 2) 
			       ("~/Dropbox/org/someday.org" :level . 2)))

    (setq org-my-anki-file "~/Dropbox/org/anki.org")

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
    (setq org-agenda-custom-commands
	  '(("s" alltodo "" ((org-agenda-files '("~/Dropbox/org/school.org"))))
	    ("r" alltodo "" ((org-agenda-files '("~/Dropbox/org/reading.org"))))
	    ("i" alltodo "" ((org-agenda-files '("~/Dropbox/org/implement.org"))))
	    ("p" . "PROJECT+Name tags searches")
	    ("pI" tags "+PROJECT+My")
	    ("po" tags "+PROJECT+Octochain")
	    ("pm" tags "+PROJECT+Masterloop")
	    ("pe" tags "+PROJECT+Easee")
	    ("pp" tags "+PROJECT+Public")))

    ;; babel
    (setq org-babel-clojure-backend 'cider)
    (add-hook 'org-babel-after-execute-hook 'org-babel-python-strip-session-chars)

    ;; Latex
    (require 'ox-latex)
    (add-to-list 'org-latex-packages-alist '("" "listingsutf8"))
    (add-to-list 'org-latex-packages-alist '("" "color"))
    ;; (add-to-list 'org-latex-packages-alist '("" "minted"))

    (add-hook 'org-mode-hook 'visual-line-mode)

    (let ((targetdir "~/.emacs.d/private/ox-jekyll-lite/"))
      (clone-if-not-exists "https://github.com/torfjelde/ox-jekyll-lite.git"
			   targetdir)
      (when (file-directory-p targetdir)
	(add-to-list 'load-path targetdir)))

    ;; HACK: I generally don't use
    (clone-if-not-exists "https://github.com/gjkerns/ob-julia.git"
			 "~/.emacs.d/private/ob-julia/")
    (let ((targetdir "~/.emacs.d/private/ob-julia/"))
      (when (file-directory-p targetdir)
	(add-to-list 'load-path targetdir)))

    ;; if you ever have issues with org-evaluate being disabled
    ;; => https://emacs.stackexchange.com/questions/28441/org-mode-9-unable-to-eval-code-blocks
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp t)
       (shell . t)
       (C . t)
       (dot . t)
       (latex . t)
       (sql . t)
       (sqlite . t)
       (clojure . t)
       (python . t)
       (matlab . t)
       ;; (R . t)
       ;; (ein . t)
       ;; (ipython . t)
       ;; (scala . t)
       ;; (rust . t)
       ;; (haskell . t)
       (jupyter . t)
       (julia . t)
       ;; (csharp. t)
       (ditaa . t)))

    (setq org-babel-default-header-args:jupyter-julia '((:async . "yes")
							(:session . "jl")
							(:kernel . "julia-1.5")))

    ;; ensure that we use Py3 to evaluate Python blocks
    (setq org-babel-python-command "python3")

    (org-babel-jupyter-override-src-block "julia")
    (org-babel-jupyter-override-src-block "python")

    ;; customization for HTML export using MathJax
    (setq org-html-mathjax-template "<script type=\"text/x-mathjax-config\">
 MathJax.Hub.Config({
   displayAlign: \"%ALIGN\",
   displayIndent: \"%INDENT\",

   \"HTML-CSS\": { scale: %SCALE,
		 linebreaks: { automatic: \"%LINEBREAKS\" },
		 webFont: \"%FONT\"
   },
   SVG: {scale: %SCALE,
	 linebreaks: { automatic: \"%LINEBREAKS\" },
	 font: \"%FONT\"},
   NativeMML: {scale: %SCALE},
   TeX: { equationNumbers: {autoNumber: \"%AUTONUMBER\"},
	  MultLineWidth: \"%MULTLINEWIDTH\",
	  TagSide: \"%TAGSIDE\",
	  TagIndent: \"%TAGINDENT\",
	  extensions: [\"color.js\", \"cancel.js\"]
   },
   extensions: [\"[Contrib]/physics/physics.js\"]
 });
</script>
<script type=\"text/javascript\"
	src=\"%PATH\"></script>

")

    ;; show agenda on startup
    (setq initial-buffer-choice (lambda ()
				  (org-agenda-list)
				  (get-buffer "*Org Agenda*")))

    ;; ox-publish
    (require 'ox-publish)
    (setq org-publish-project-alist
	  '(
	    ;; ... add all the components here (see below)...
	    ("blog-latex"
	     :base-directory "~/org-blog/assets/latex"
	     :publishing-directory "~/org-blog/public_html/assets/latex"
	     :recursive t
	     :publishing-function tor/org-publish-attachment
	     :base-extension "png\\|jpg\\|gif\\\\|ogg\\|swf")

	    ;; ;; TODO: somehow allow us to simply copy the files in one go instead of going through
	    ;; ;; all files to check if modified
	    ("blog-latex-local"
	     :base-directory "~/org-blog/assets/latex"
	     :publishing-directory "~/org-blog/public_html/assets/latex"
	     :recursive nil
	     :preparation-function tor/org-publish-attachment-local
	     :publishing-function identity
	     :base-extension "")

	    ("org-posts"
	     :project-directory "~/org-blog/"
	     :assets-directory "~/org-blog/assets/"
	     :base-directory "~/org-blog/posts/"
	     :base-extension "org"
	     :exclude ".*\\.org_archive|.*\\.org_old"  ;; HACK: this allows us to filter out posts
	     :publishing-directory "~/org-blog/public_html/posts/"
	     :recursive nil
	     :publishing-function tor/org-html-publish-to-html
	     ;; :preparation-function tor/prepare-blog-post-publish
	     :headline-levels 4
	     :auto-preamble t
	     :html-preamble tor/render-html-preamble--posts
	     :html-postamble nil
	     :html-html5-fancy t
	     :html-metadata-timestamp-format "%Y-%m-%d %a")

	    ("org-posts-index"
	     :base-directory "~/org-blog/posts"
	     :base-extension "html"
	     :publishing-directory "~/org-blog/public_html/posts/"
	     :publishing-function tor/publish-html
	     :preparation-function tor/prepare-blog-post-publish
	     :recursive nil
	     :auto-preamble nil
	     :html-postamble nil
	     :html-preamble nil)

	    ("org-posts-assets"
	     :base-directory "~/org-blog/posts/"
	     :base-extension "css\\|js\\|png\\|jpg\\|svg\\|gif\\|mp3\\|ogg\\|swf"
	     :publishing-directory "~/org-blog/public_html/posts/"
	     :recursive t
	     :publishing-function tor/org-publish-attachment)

	    ("org-blog" :components ("org-posts" "org-posts-index" "org-posts-assets"))

	    ("org-notes"
	     :project-directory "~/org-blog/"
	     :assets-directory "̃~/org-blog/assets/"
	     :base-directory "~/org-blog/notes/"
	     :base-extension "org"
	     :publishing-directory "~/org-blog/public_html/notes/"
	     :recursive t
	     :publishing-function tor/org-html-publish-to-html
	     :headline-levels 4             ; Just the default for this project.
	     :auto-preamble t
	     :html-preamble tor/render-html-preamble
	     :html-postamble nil
	     ;; :html-postamble tor/render-html-postamble
	     ;; :html-html5-fancy t
	     :html-metadata-timestamp-format "%Y-%m-%d %a"
	     )

	    ("org-notes-assets"
	     :base-directory "~/org-blog/notes/"
	     :base-extension "css\\|js\\|png\\|jpg\\|svg\\|gif\\|mp3\\|ogg\\|swf"
	     :publishing-directory "~/org-blog/public_html/notes/"
	     :recursive t
	     :publishing-function tor/org-publish-attachment)

	    ("org-static"
	     :project-directory "~/org-blog/"
	     :base-directory "~/org-blog/assets/"
	     ;; :base-extension "css\\|js\\|png\\|jpg\\|gif\\|mp3\\|ogg\\|swf"
	     :base-extension "css\\|js\\|gif\\|mp3\\|ogg\\|swf"
	     :publishing-directory "~/org-blog/public_html/assets/"
	     :recursive t
	     :publishing-function tor/org-publish-attachment)

	    ("org"
	     :components ("org-notes" "org-notes-assets" "org-static"))

	    ("org-papers"
	     ;; :base-directory "~/Dropbox/bibliography/notes/"
	     :project-directory "~/org-blog/"
	     :assets-directory "̃~/org-blog/assets/"
	     :base-directory "~/org-blog/papers/"
	     :base-extension "org"
	     :publishing-directory "~/org-blog/public_html/papers/"
	     :recursive nil
	     :publishing-function tor/org-html-publish-to-html
	     :headline-levels 4
	     :auto-premable t
	     :html-postamble nil)
	    ))
    ))

(use-package org-bullets
  :init (add-hook 'org-mode-hook 'org-bullets-mode))

;; This adds some autocomplete stuff specific for Org-mode, e.g. allowing you to
;; add multiple tags to a headline using autocompletion.
(use-package helm-org
  :pin melpa
  :init (progn
          ;; ensures that it works correctly with org-capture
          (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
          (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags))))

(defun tor/org-ref-open-bibtex-pdf ()
  "Attemt to open PDF from file-field in BibTeX entry if does not exist in default pdf-dir."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (let* ((bibtex-expand-strings t)
	   (entry (bibtex-parse-entry t))
	   (key (reftex-get-bib-field "=key=" entry))
	   (pdf (org-ref-get-mendeley-filename key)))
      (message "%s" pdf)
      (if (file-exists-p pdf)
	  (org-open-link-from-string (format "[[file:%s]]" pdf))
	(ding)))))

;; TODO: is this necessary?
(use-package bibtex-completion
  :pin melpa)

(use-package helm-bibtex
  :pin melpa
  :config (require 'bibtex-completion))

;; (use-package org-ref
;;   :pin melpa
;;   :config (progn
;; 	    (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib")
;; 		  org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
;; 		  org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
;; 		  org-ref-pdf-directory "~/Dropbox/bibliography/pdfs/"
;; 		  biblio-download-directory "~/Dropbox/bibliography/pdfs/"
;; 		  bibtex-completion-bibliography '("~/Dropbox/bibliography/references.bib")
;; 		  ;; bibtex-completion-notes-path "/home/tor/Dropbox/bibliography/notes/"
;; 		  bibtex-completion-notes-path "/home/tor/org-blog/papers/"
;; 		  bibtex-completion-notes-template-multiple-files "#+SETUPFILE: ../setup-level-1.org\n#+TITLE: Notes on: ${author-or-editor} (${year}): ${title}\n\n"

;; 		  bibtex-completion-library-path '("~/Dropbox/bibliography/pdfs")

;; 		  ;; ensures that the use of #+NAME: works properly when exporting
;; 		  org-latex-prefer-user-labels t

;; 		  ;; with this activated it's horrendously SLOW for large files
;; 		  org-ref-show-broken-links nil

;; 		  org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;; 					  "bibtex %b"
;; 					  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;; 					  "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
;; 		  ;; also attempts to open what's referenced in the "file = ..." field of the BibTeX entry
;; 		  org-ref-open-pdf-function 'tor/org-ref-open-bibtex-pdf

;; 		  ;; adds more entry-types, e.g. @misc and @online
;; 		  ;; bibtex-dialect 'biblatex
;; 		  )))
(use-package org-ref
  :pin melpa
  :config (progn
            (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib")
                  org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
                  org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
                  org-ref-pdf-directory "~/Dropbox/bibliography/pdfs/"
                  biblio-download-directory "~/Dropbox/bibliography/pdfs/"
                  bibtex-completion-bibliography '("~/Dropbox/bibliography/references.bib")
                  ;; bibtex-completion-notes-path "/home/tor/Dropbox/bibliography/notes/"
                  bibtex-completion-notes-path "/home/tor/org-blog/papers/"
                  bibtex-completion-notes-template-multiple-files "#+SETUPFILE: ../setup-level-1.org\n#+TITLE: Notes on: ${author-or-editor} (${year}): ${title}\n\n"

                  bibtex-completion-library-path '("~/Dropbox/bibliography/pdfs")

                  ;; ensures that the use of #+NAME: works properly when exporting
                  org-latex-prefer-user-labels t

                  ;; with this activated it's horrendously SLOW for large files
                  org-ref-show-broken-links nil

                  org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                          "bibtex %b"
                                          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                                          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
                  ;; also attempts to open what's referenced in the "file = ..." field of the BibTeX entry
                  org-ref-open-pdf-function 'tor/org-ref-open-bibtex-pdf

                  ;; adds more entry-types, e.g. @misc and @online
                  ;; bibtex-dialect 'biblatex
                  )

            ;; overwrites the 'inbook' BibTeX type defined by doi-utils
            ;; +FIXME+: getting an issue with "mandatory field is missing: chapter"
            ;; the above was due to the choice of dialect
            (doi-utils-def-bibtex-type book ("book")
                                       author title booktitle series publisher year pages doi url)
            (doi-utils-def-bibtex-type inbook ("book-chapter" "chapter" "reference-entry")
                                       author title booktitle series publisher year pages doi url)
            ;;

            ;; FIXME: for now we make `misc' a placeholder for `online'
            ;; since the dialect `BibTeX' does not support `online'
            ;; which causes issues when exporting Org-files

            ;; (doi-utils-def-bibtex-type online ("online")
            ;;                            author title url year)
            ;; (add-to-list 'org-ref-bibliography-entry-format '("online" . "%a, %t, <a href=\"%U\">link</a>. %N"))
            ;; and misc

            (add-to-list 'org-ref-bibliography-entry-format '("misc" . "%a, %t, <a href=\"%U\">link</a>.. %N"))

            ;; NOT WORKING
            ;; (defun my-pdf-proxy (orig-fun &rest args)
            ;;   (let* ((pdf-url (apply orig-fun args))
            ;;          (url-struct (url-generic-parse-url pdf-url)))
            ;;     (setf (url-host url-struct)
            ;;           (concat (url-host url-struct) ".ezproxy.is.ed.ac.uk"))
            ;;     (url-recreate-url url-struct)))

            ;; remove it like this.
            ;; (advice-remove 'doi-utils-get-pdf-url #'my-pdf-proxy)
            ;; (advice-add 'doi-utils-get-pdf-url :around #'my-pdf-proxy)
            (bind-key "C-c ]" 'org-ref-helm-insert-cite-link)
            )
  :init (progn
          (require 'org-ref-pdf)
          (bind-key "C-c [" 'org-ref-insert-ref-link)
          (bind-key "C-c ]" 'org-ref-helm-insert-cite-link)))

;; (use-package org-pdftools
;;   :hook (org-load . org-pdftools-setup-link))

;; (use-package org-noter-pdftools
;;   :after org-noter
;;   :config
;;   (with-eval-after-load 'pdf-annot
;;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package org-download
  :config (progn
          ;; don't want to use the sub-headings for the folder name
          ;; TODO: check if this actually work! Might be buffer-local, hence not do anything.
          (setq org-download-heading-lvl nil)
          
          ;; HACK: overload this method so we fall back to using "./.filename/assets/" for the downloaded stuff
          (defun org-download--dir-1 ()
            (or org-download-image-dir
                (concat (file-name-as-directory ".")
                        "."
                        (file-name-base)
                        "/attachments")))))

(use-package org-pdftools
  :after org
  :config (org-pdftools-setup-link))

(use-package org-pomodoro
  :pin melpa
  :ensure t
  :commands (org-pomodoro)
  :config
  (progn
    (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil))))
    (setq org-pomodoro-length 60) ;; make one session 1hr
    ))

;; END org-mode

(use-package markdown-mode
  :config (progn
            (setq-default markdown-spaces-after-code-fence 0)))
(use-package edit-indirect
  :config (progn
            ;; Interfers with Jupyter's `C-c C-c' to evaluate region, so we unbind
            ;; + also have `C-c '' to commit.
            (define-key edit-indirect-mode-map (kbd "C-c C-c") nil)))

(use-package polymode)
(use-package poly-markdown
  :mode ("\\.[jJ]md" . poly-markdown-mode)
  :bind (:map poly-markdown-mode-map
              ("C-c '" . markdown-edit-code-block)))

;;; themes ;;;
(message "Parsing themes")
(use-package solarized-theme
  :init (progn
          ;; Sets it to similar colors as the theme-colors; if `t' we use `dark' else we use `light'.
          ;; `solarized-dark' will have the "correct" midnight mode, so only do it if using `light'.
          (when nil
            ;; (setq pdf-view-midnight-colors '("#556065" . "#fdf6e3"))
            ;; (setq pdf-view-midnight-colors '("#3f4446" . "#fdf6e3")) ;; slightly blacker font
            (setq pdf-view-midnight-colors '("#556065" . "#fff8e5")) ;; slightly brighter background
            )))

;; (use-package darktooth-theme)
;; (use-package atom-one-dark-theme)
;; :init
;; (add-hook 'after-make-frame-functions
;;         '(lambda (frame)
;;           (select-frame frame)
;;           (if window-system
;;               nil
;; 	      (set-frame-parameter nil 'background-color "#2B2B2B")
;; 	      ))))

;; Essentially removing the background color in terminal since I often use transparent terminals
(defun on-frame-open (&optional frame)
  "If the FRAME created in terminal don't load background color."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))

(add-hook 'after-make-frame-functions 'on-frame-open)

;; (custom-set-variables '(custom-enabled-themes (quote solarized-dark))

(use-package smart-mode-line)
(use-package spaceline
  :init
  (progn
	(require 'spaceline-config)
	(spaceline-emacs-theme)
        ))

;; (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;                              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;                              ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;                              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;                              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;; 	  ;; some settings for makin headings and bullets nicer
;; 	  (custom-theme-set-faces 'user
;; 							  `(org-level-8 ((t (,@headline ,@variable-tuple))))
;; 							  `(org-level-7 ((t (,@headline ,@variable-tuple))))
;; 							  `(org-level-6 ((t (,@headline ,@variable-tuple))))
;; 							  `(org-level-5 ((t (,@headline ,@variable-tuple))))
;; 							  `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;; 							  `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;; 							  `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;; 							  `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;; 							  `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

;; Global keybindings
(bind-keys*
 ("C-x C-y" . tor/duplicate-downward)
 ("C-c C-x C-m" . mc/mark-all-in-region))

;; add the private files to `load-path'
(message "Loading private files")
(add-to-list 'load-path "~/.emacs.d/private/")
;; (load "utilities")
(require 'bookmark+)

;; TODO: make this automatically download and set it up
;; Requires downloading and loading https://github.com/emacsmirror/emacswiki.org/blob/master/header2.el
;; and then the following can be used to automatically insert headers!
;; (defsubst header-org-mode-latex-default ()
;;   (when (eq major-mode 'org-mode)
;;     (insert "#+SETUPFILE: ~/org-blog/setup.org\n")))

;; (setq make-header-hook '(header-org-mode-latex-default))

;; (add-hook 'org-mode-hook 'auto-make-header)

(blink-cursor-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(default-text-scale-mode)

(setq
 system-time-locale "C" ;; ensures that week-days follows English convention, e.g. Thu and Wed
 )

(message "Parsing custom-variables")

;; (custom-set-variables
;;  '(custom-enabled-themes (quote (solarized-dark)))
;;  '(custom-safe-themes
;;    (quote
;;     ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "ec5f761d75345d1cf96d744c50cf7c928959f075acf3f2631742d5c9fe2153ad" "59171e7f5270c0f8c28721bb96ae56d35f38a0d86da35eab4001aebbd99271a8" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
;;  '(default-text-scale-mode t nil (default-text-scale))
;;  )

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(TeX-view-program-selection
;;    (quote
;;     (((output-dvi has-no-display-manager)
;;       "dvi2tty")
;;      ((output-dvi style-pstricks)
;;       "dvips and gv")
;;      (output-dvi "xdvi")
;;      (output-pdf "PDF Tools")
;;      (output-html "xdg-open"))))
;;  '(bibtex-completion-pdf-field "file")
;;  '(blink-cursor-mode nil)
;;  '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
;;  '(compilation-message-face (quote default))
;;  '(custom-enabled-themes (quote (solarized-dark)))
;;  '(custom-safe-themes
;;    (quote
;;     ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d91ef4e714f05fff2070da7ca452980999f5361209e679ee988e3c432df24347" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" "ec5f761d75345d1cf96d744c50cf7c928959f075acf3f2631742d5c9fe2153ad" "59171e7f5270c0f8c28721bb96ae56d35f38a0d86da35eab4001aebbd99271a8" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
;;  '(default-text-scale-mode t nil (default-text-scale))
;;  '(elpy-rpc-python-command "python3")
;;  '(julia-max-block-lookback 100000)
;;  '(magit-diff-use-overlays nil)
;;  '(markdown-command "/usr/bin/pandoc")
;;  '(org-agenda-files
;;    (quote
;;     ("~/Dropbox/org/gtd.org" "~/Dropbox/org/school.org" "~/Dropbox/org/reading.org" "~/Dropbox/org/implement.org")))
;;  '(org-babel-inline-result-wrap "%s")
;;  '(org-edit-src-content-indentation 0)
;;  '(org-emphasis-alist
;;    (quote
;;     (("*" bold)
;;      ("/" italic)
;;      ("_" default)
;;      ("=" org-verbatim verbatim)
;;      ("~" org-code verbatim)
;;      ("+"
;;       (:strike-through t)))))
;;  '(org-format-latex-header
;;    "\\documentclass{article}
;; \\usepackage[usenames]{color}
;; [PACKAGES]
;; [DEFAULT-PACKAGES]
;; \\pagestyle{empty}             % do not remove
;; % The settings below are copied from fullpage.sty
;; \\setlength{\\textwidth}{\\paperwidth}
;; \\addtolength{\\textwidth}{-3cm}
;; \\setlength{\\oddsidemargin}{1.5cm}
;; \\addtolength{\\oddsidemargin}{-2.54cm}
;; \\setlength{\\evensidemargin}{\\oddsidemargin}
;; \\setlength{\\textheight}{\\paperheight}
;; \\addtolength{\\textheight}{-\\headheight}
;; \\addtolength{\\textheight}{-\\headsep}
;; \\addtolength{\\textheight}{-\\footskip}
;; \\addtolength{\\textheight}{-3cm}
;; \\setlength{\\topmargin}{1.5cm}
;; \\addtolength{\\topmargin}{-2.54cm}")
;;  '(org-format-latex-options
;;    (quote
;;     (:foreground default :background default :scale 1.5 :html-foreground "SteelBlue" :html-background "Transparent" :html-scale 1.0 :matchers
;;                  ("begin" "$1" "$" "$$" "\\(" "\\["))))
;;  '(org-html-htmlize-output-type (quote inline-css))
;;  '(org-html-mathjax-options
;;    (quote
;;     ((path "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML")
;;      (scale "100")
;;      (align "center")
;;      (font "Neo-Euler")
;;      (linebreaks "false")
;;      (autonumber "AMS")
;;      (indent "0em")
;;      (multlinewidth "85%")
;;      (tagindent ".8em")
;;      (tagside "right"))))
;;  '(org-latex-default-packages-alist
;;    (quote
;;     (("AUTO" "inputenc" t
;;       ("pdflatex"))
;;      ("T1" "fontenc" t
;;       ("pdflatex"))
;;      ("" "graphicx" t nil)
;;      ("" "grffile" t nil)
;;      ("" "longtable" nil nil)
;;      ("" "wrapfig" nil nil)
;;      ("" "rotating" nil nil)
;;      ("normalem" "ulem" t nil)
;;      ("" "amsmath" t nil)
;;      ("" "textcomp" t nil)
;;      ("" "amssymb" t nil)
;;      ("" "capt-of" nil nil))))
;;  '(org-latex-hyperref-template "
;; ")
;;  '(org-latex-pdf-process
;;    (quote
;;     ("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "bibtex %b" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))
;;  '(org-link-file-path-type (quote relative))
;;  '(org-preview-latex-image-directory "/home/tor/.ltximg/")
;;  '(org-preview-latex-process-alist
;;    (quote
;;     ((dvipng :programs
;;              ("latex" "dvipng")
;;              :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
;;              (1.0 . 1.0)
;;              :latex-compiler
;;              ("latex -interaction nonstopmode -output-directory %o %f")
;;              :image-converter
;;              ("dvipng -D %D -T tight -o %O %f"))
;;      (dvisvgm :programs
;;               ("latex" "dvisvgm")
;;               :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :use-xcolor t :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
;;               (1.7 . 1.5)
;;               :latex-compiler
;;               ("latex -interaction nonstopmode -output-directory %o %f")
;;               :image-converter
;;               ("dvisvgm %f -n -b min -c %S -o %O"))
;;      (imagemagick :programs
;;                   ("latex" "convert")
;;                   :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :use-xcolor t :image-input-type "pdf" :image-output-type "png" :image-size-adjust
;;                   (1.0 . 1.0)
;;                   :latex-compiler
;;                   ("pdflatex -interaction nonstopmode -output-directory %o %f")
;;                   :image-converter
;;                   ("convert -density %D -trim -antialias %f -quality 100 -transparent white %O")))))
;;  '(org-ref-bib-html "")
;;  '(org-ref-formatted-citation-formats
;;    (quote
;;     (("text"
;;       ("article" . "${author}, ${title}, ${journal}, ${archivePrefix}:${eprint} [${primaryClass}], ${volume}(${number}), ${pages} (${year}). ${doi}")
;;       ("inproceedings" . "${author}, ${title}, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
;;       ("book" . "${author}, ${title} (${year}), ${address}: ${publisher}.")
;;       ("phdthesis" . "${author}, ${title} (Doctoral dissertation) (${year}). ${school}, ${address}.")
;;       ("inbook" . "${author}, ${title}, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
;;       ("incollection" . "${author}, ${title}, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
;;       ("proceedings" . "${editor} (Eds.), ${booktitle} (${year}). ${address}: ${publisher}.")
;;       ("unpublished" . "${author}, ${title} (${year}). Unpublished manuscript.")
;;       (nil . "${author}, ${title} (${year})."))
;;      ("org"
;;       ("article" . "${author}, /${title}/, ${journal}, *${volume}(${number})*, ${pages} (${year}). ${doi}")
;;       ("inproceedings" . "${author}, /${title}/, In ${editor}, ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
;;       ("book" . "${author}, /${title}/ (${year}), ${address}: ${publisher}.")
;;       ("phdthesis" . "${author}, /${title}/ (Doctoral dissertation) (${year}). ${school}, ${address}.")
;;       ("inbook" . "${author}, /${title}/, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
;;       ("incollection" . "${author}, /${title}/, In ${editor} (Eds.), ${booktitle} (pp. ${pages}) (${year}). ${address}: ${publisher}.")
;;       ("proceedings" . "${editor} (Eds.), _${booktitle}_ (${year}). ${address}: ${publisher}.")
;;       ("unpublished" . "${author}, /${title}/ (${year}). Unpublished manuscript.")
;;       (nil . "${author}, /${title}/ (${year}).")))))
;;  '(org-reveal-mathjax-url "./MathJax-2.7.5/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
;;  '(package-selected-packages
;;    (quote
;;     (solarized-theme ox-jekyll-md stan-mode pdf-tools helm-bibtex gnuplot org-tree-slide gnu-elpa-keyring-update annotate jupyter lv sudo-edit ox-gfm graphviz-dot-mode ox-reveal projectile-ripgrep sublimity gif-screencast ox-rst interleave xah-lookup org-brain web-mode use-package string-inflection spotify spaceline smartparens smart-mode-line racer ox-hugo ox-clip owdriver org-ref org-clock-convenience org-bullets ob-sql-mode ob-rust ob-http ob-go mustache multiple-cursors matlab-mode irony-eldoc iedit helm-spotify helm-projectile helm-org-rifle helm-emmet helm-descbinds groovy-mode fic-mode exec-path-from-shell ess edit-server edit-indirect dirtree darktooth-theme csharp-mode cql-mode company-tern company-racer company-quickhelp company-jedi company-irony-c-headers company-irony company-go company-auctex centered-cursor-mode arduino-mode ace-window ace-jump-mode)))
;;  '(python-shell-interpreter "python3")
;;  '(vc-annotate-background nil)
;;  '(vc-annotate-background-mode nil)
;;  '(vc-annotate-very-old-color nil)
;;  '(warning-suppress-types (quote ((yasnippet backquote-change) (:warning))))
;;  '(yas-indent-line (quote fixed)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
;;  '(fic-author-face ((t (:foreground "dark violet" :underline t))))
;;  '(fic-face ((t (:foreground "magenta" :weight bold))))
;;  ;; '(org-block ((t (:background "#002d39")))) ;; HACK: this is in case we want to change the background-color; though should probably make a custom-theme instead
;;  '(org-block-begin-line ((t (:inherit org-meta-line :underline nil))))
;;  '(org-block-end-line ((t (:inherit org-meta-line :overline nil :slant normal :weight bold))))
;;  '(org-document-title ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif" :height 1.5 :underline nil))))
;;  '(org-level-1 ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif" :height 1.75))))
;;  '(org-level-2 ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif" :height 1.5))))
;;  '(org-level-3 ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif" :height 1.25))))
;;  '(org-level-4 ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif" :height 1.1))))
;;  '(org-level-5 ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif"))))
;;  '(org-level-6 ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif"))))
;;  '(org-level-7 ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif"))))
;;  '(org-level-8 ((t (:inherit default :weight bold :foreground "#ABB2BF" :family "Sans Serif"))))
;;  '(org-meta-line ((t (:foreground "#586e75" :slant normal :weight bold)))))
;; (put 'narrow-to-region 'disabled nil)



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
     (output-html "xdg-open")))
 '(aw-scope 'frame)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-enabled-themes '(solarized-dark))
 '(custom-safe-themes
   '("0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" default))
 '(elpy-rpc-python-command "python3")
 '(elpy-syntax-check-command "pylint")
 '(org-file-apps
   '((auto-mode . emacs)
     ("\\.mm\\'" . default)
     ("\\.x?html?\\'" . default)
     ("\\.pdf\\'" . emacs)))
 '(org-latex-default-packages-alist
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
     ("" "capt-of" nil nil)))
 '(org-latex-packages-alist '(("" "color" t) ("" "listingsutf8" t)))
 '(org-noter-default-notes-file-names '("notes.org"))
 '(org-noter-hide-other nil)
 '(org-noter-insert-note-no-questions t)
 '(org-noter-notes-search-path '("~/org-blog/papers"))
 '(package-selected-packages
   '(treemacs orgit neotree dired-subtree github-review forge ox-reveal s xenops company-reftex texfrag julia-repl poly-markdown matlab-mode vterm gist systemd edit-indirect company-tern yaml-mode xclip which-key web-mode visual-fill-column use-package undo-tree tide tern spaceline solarized-theme smartparens smart-mode-line scala-mode rjsx-mode rainbow-delimiters racer org-ref org-pomodoro org-download org-bullets ob-sql-mode ob-http mustache multiple-cursors magit lua-mode jupyter jedi iedit helpful helm-projectile helm-org helm-emmet helm-descbinds haskell-mode groovy-mode go-mode fic-mode exec-path-from-shell ess elpy ein edit-server dirtree default-text-scale csharp-mode cql-mode company-racer company-quickhelp company-jedi company-irony-c-headers company-irony company-go company-auctex cider centered-cursor-mode arduino-mode anki-editor ace-window))
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   '((org-download-image-dir . "~/Dropbox/org/anki-images")
     (eval setq-local org-tag-alist
           (-map
            (lambda
              (el)
              (cons el nil))
            (anki-editor-all-tags)))
     (visual-line-mode . 1)
     (visual-line-mode . t)
     (org-export-with-toc)
     (org-after-todo-state-change-hook . tor/reading-list-done-hook)
     (org-after-todo-state-change-hook . tor/impl-list-done-hook))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
