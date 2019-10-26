(setq package-check-signature nil)  ;; FIXME: hack because signatures fail

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
 tool-bar-mode nil
 scroll-bar-mode nil
 blink-cursor-mode nil

 org-adapt-indentation nil  ;; don't indent text in a section to align with section-level
 org-export-allow-bind-keywords t  ;; allows us to set variables in setup-files for project
 org-preview-latex-image-directory "~/.ltximg/"  ;; this '/' at the end is VERY important..

 ;; TeX stuff
 TeX-source-correlate-start-server t  ;; clicking in document takes you to source

 ;; set the backup folder to be the temp-folder
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 
 ;; omnisharp-server-executable-path "/home/tor/omnisharp-server/Omnisharp/bin/Debug/OmniSharp.exe"

 yas-triggers-in-field t; Enable nested triggering of snippets

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

;; PEP-8 tells me not to use tabs..so by defalt we disable this
(setq-default indent-tabs-mode nil)

;; bar-customization
(menu-bar-mode -1)


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

(require 'use-package)

;; notes about use-package ;;
;; :init - executes BEFORE loading package
;; :config - executes AFTER loading package

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

;;; general ;;;
;; pdf-tools - much improved way to view pdfs
;; IMPORTANT: need to run `(pdf-tools-install)' to install dependencies
(use-package pdf-tools
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
(use-package org
  :pin org
  :bind (("C-c l" . org-store-link))
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
    (setq org-agenda-files '("~/Dropbox/org/gtd.org"
                             "~/Dropbox/org/school.org"
                             "~/Dropbox/org/reading.org"
                             "~/Dropbox/org/implement.org"))
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
             "* %^{Description}\nEntered on %U\n%a\n%?" :empty-lines 1)
            
            ("i" "Idea" item (file "~/Dropbox/org/ideas.org"))

            ("s" "School" entry
             (file "~/Dropbox/org/school.org")
             "* TODO %^{Brief Description} %^{COURSE}p %^g\n%?" :empty-lines 1)

            ("r" "Reading" entry (file "~/Dropbox/org/reading.org")
             "* TODO %(tor/reading-list-next-idx). %?\nEntered on %U\n%a\n%i")

            ("I" "Implement" entry (file "~/Dropbox/org/implement.org")
             "* TODO %(tor/impl-list-next-idx). %?\nEntered on %U\n%a\n%i")
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
    (add-to-list 'org-latex-packages-alist '("" "minted"))

    (add-hook 'org-mode-hook 'visual-line-mode)

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
       ;; (R . t)
       ;; (ein . t)
       ;; (ipython . t)
       ;; (scala . t)
       ;; (rust . t)
       (haskell . t)
       (jupyter . t)
       ;; (csharp. t)
       (ditaa . t)))

    ;; ensure that we use Py3 to evaluate Python blocks
    (setq org-babel-python-command "python3")

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

(use-package org-ref
  :pin melpa
  :config (progn
            (setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib")
                  org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
                  org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
                  org-ref-pdf-directory "~/Dropbox/bibliography/pdfs/"
                  biblio-download-directory "~/Dropbox/bibliography/pdfs/"
                  bibtex-completion-bibliography '("~/Dropbox/bibliography/references.bib")
                  ;; bibtex-completion-notes-path "~/Dropbox/bibliography/notes/"
                  bibtex-completion-notes-path "~/org-blog/papers/"
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
            )
  :init (progn
          (require 'org-ref-pdf)
          (bind-key "C-c [" 'org-ref-insert-ref-link)
          (bind-key "C-c ]" 'org-ref-helm-insert-cite-link)))

;; AucTeX
;; (require 'auctex)
(setq LaTeX-command-style '(("" "%(PDF)%(latex) -shell-escape %S%(PDFout)")))
;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)

(use-package company-auctex
  :init (progn
          (company-auctex-init)
          (add-hook 'LaTeX-mode-hook 'company-mode)
          (add-hook 'latex-mode-hook 'company-mode)))

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
    ;; (add-to-list 'company-backends 'ein:company-backend)
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

;; (use-package ace-jump-mode
;;   :bind ("C-." . ace-jump-mode))
(use-package avy
  :bind ("M-æ" . avy-goto-word-or-subword-1))
(use-package ace-window
  :config (global-set-key (kbd "M-å") 'ace-window))
(use-package iedit
  :bind ("C-c ,"))
(use-package rainbow-delimiters)
(use-package centered-cursor-mode)
(use-package htmlize)

(use-package magit
  :pin melpa)
(use-package forge
  :pin melpa)

(use-package default-text-scale
  :pin melpa)


(message "Parsing programming setup")


;;; programming languages ;;;
(add-hook 'prog-mode-hook 'my-prettiest-symbols)

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
  :pin melpa-stable)

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

;; Golang
(use-package go-mode
  :mode "\\.go\\.?$"
  :pin melpa-stable
  :config (add-to-list 'company-backends 'company-go))
(use-package company-go)

;; Julia
(use-package julia-mode
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

(use-package company-tern
  :init (add-to-list 'company-backends 'company-tern))

;; typescript
(use-package typescript-mode
  :init (sp-local-pair 'csharp-mode "{" nil :post-handlers '((my/open-block-c-mode "RET"))))

;; R
(use-package ess
  :pin melpa-stable)

;; General stuff but also julia
(use-package jupyter
  :pin melpa)

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

;; (use-package ein
;;   :init (progn
;;           ;; BUG: this does not currently work for some reason; also I think I need it
;;           ;; (setq ein:use-smartrep t)
;;           (add-hook 'ein:notebook-mode-hook 'company-mode)))

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


;; Lua
(use-package lua-mode
  :pin melpa
  :mode "\\.lua?$")


(add-hook 'lisp-mode-hook 'pretty-greek)
(add-hook 'emacs-lisp-mode-hook 'pretty-greek)

;;; themes ;;;
(message "Parsing themes")
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

(defun on-frame-open (&optional frame)
  "If the FRAME created in terminal don't load background color."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))

(add-hook 'after-make-frame-functions 'on-frame-open)

(use-package smart-mode-line)
(use-package spaceline
  :init
  (progn
	(require 'spaceline-config)
	(spaceline-emacs-theme)))

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

;; Tor's keybindings
(defun tor/duplicate-downward (begin end)
  "https://emacs.stackexchange.com/a/32515"
  (interactive "r")
  (let (deactivate-mark (point (point)))
    (insert (buffer-substring begin end))
    (push-mark point)))

(bind-keys*
 ("C-x C-y" . tor/duplicate-downward)
 ("C-c C-x C-m" . mc/mark-all-in-region))

;; add the private files to `load-path'
(message "Loading private files")
(add-to-list 'load-path "~/.emacs.d/private/")
(load "utilities")
(require 'bookmark+)

;;; Screen specific stuff ;;;
;; (if (equal (x-display-pixel-height) 1440)
;;     (setq doc-view-resolution 192))

(message "Parsing custom-variables")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
     (output-html "xdg-open"))))
 '(ansi-color-names-vector
   ["#32302F" "#FB4934" "#B8BB26" "#FABD2F" "#83A598" "#D3869B" "#17CCD5" "#EBDBB2"])
 '(blink-cursor-mode nil)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-enabled-themes (quote (atom-one-dark)))
 '(custom-safe-themes
   (quote
    ("ec5f761d75345d1cf96d744c50cf7c928959f075acf3f2631742d5c9fe2153ad" "59171e7f5270c0f8c28721bb96ae56d35f38a0d86da35eab4001aebbd99271a8" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(default-text-scale-mode t nil (default-text-scale))
 '(elpy-rpc-python-command "python")
 '(helm-bibtex-pdf-field "file" t)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/gtd.org" "~/Dropbox/org/school.org" "~/Dropbox/org/reading.org" "~/Dropbox/org/implement.org")))
 '(org-edit-src-content-indentation 0)
 '(org-emphasis-alist
   (quote
    (("*" bold)
     ("/" italic)
     ("_" default)
     ("=" org-verbatim verbatim)
     ("~" org-code verbatim)
     ("+"
      (:strike-through t)))))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.5 :html-foreground "SteelBlue" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-html-mathjax-options
   (quote
    ((path "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_HTML")
     (scale "100")
     (align "center")
     (font "Neo-Euler")
     (linebreaks "false")
     (autonumber "AMS")
     (indent "0em")
     (multlinewidth "85%")
     (tagindent ".8em")
     (tagside "right"))))
 '(org-latex-pdf-process
   (quote
    ("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "bibtex %b" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))
 '(org-link-file-path-type (quote relative))
 '(org-preview-latex-process-alist
   (quote
    ((dvipng :programs
             ("latex" "dvipng")
             :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
             (1.0 . 1.0)
             :latex-compiler
             ("latex -interaction nonstopmode -output-directory %o %f")
             :image-converter
             ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f"))
     (dvisvgm :programs
              ("latex" "dvisvgm")
              :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :use-xcolor t :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
              (1.7 . 1.5)
              :latex-compiler
              ("latex -interaction nonstopmode -output-directory %o %f")
              :image-converter
              ("dvisvgm %f -n -b min -c %S -o %O"))
     (imagemagick :programs
                  ("latex" "convert")
                  :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :use-xcolor t :image-input-type "pdf" :image-output-type "png" :image-size-adjust
                  (1.0 . 1.0)
                  :latex-compiler
                  ("pdflatex -interaction nonstopmode -output-directory %o %f")
                  :image-converter
                  ("convert -density %D -trim -antialias %f -quality 100 -transparent white %O")))))
 '(org-ref-bib-html "")
 '(org-reveal-mathjax-url "./MathJax-2.7.5/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
 '(package-selected-packages
   (quote
    (poly-markdown polymode xclip forge dash default-text-scale annotate helm-bibtex jupyter lv sudo-edit ox-gfm auctex graphviz-dot-mode ox-reveal projectile-ripgrep sublimity gif-screencast ox-rst interleave xah-lookup org-brain yaml-mode web-mode use-package undo-tree tide string-inflection spotify spaceline solarized-theme smartparens smart-mode-line rainbow-delimiters racer ox-hugo ox-clip owdriver org-ref org-clock-convenience org-bullets ob-sql-mode ob-rust ob-http ob-go mustache multiple-cursors matlab-mode markdown-mode lua-mode jedi irony-eldoc iedit helpful helm-spotify-plus helm-spotify helm-projectile helm-org-rifle helm-emmet helm-descbinds haskell-mode groovy-mode fic-mode exec-path-from-shell ess ensime elpy edit-server edit-indirect dirtree darktooth-theme csharp-mode cql-mode company-tern company-racer company-quickhelp company-jedi company-irony-c-headers company-irony company-go company-auctex cider centered-cursor-mode atom-one-dark-theme arduino-mode anki-editor ace-window ace-jump-mode)))
 '(python-shell-interpreter "python")
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(warning-suppress-types (quote ((yasnippet backquote-change) (:warning))))
 '(yas-indent-line (quote fixed)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
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
