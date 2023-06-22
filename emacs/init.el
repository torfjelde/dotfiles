;; [[file:config.org::*Loading the config][Loading the config:1]]
(require 'org)

;; A copy-paste from https://github.com/bzg/org-mode/blob/bd468136dd1a2172302b3ec980c5e6b6e327d683/lisp/org.el#L249-L279
;; but with the option of specifying the tangled file `file-out'.
;; Note that if blocks have specified a `:tangle' header, only those matching `file-out'
;; will indeed be tangled.'
(defun my/org-babel-load-file (file &optional file-out compile)
  "Load Emacs Lisp source code blocks in the Org FILE to FILE-OUT.
This function exports the source code using `org-babel-tangle'
and then loads the resulting file using `load-file'.  With
optional prefix argument COMPILE, the tangled Emacs Lisp file is
byte-compiled before it is loaded."
  (interactive "fFile to load: \nP")
  (let ((tangled-file (if file-out file-out (concat (file-name-sans-extension file) ".el"))))
    ;; Tangle only if the Elisp file is older than the Org file.
    ;; Catch the case when the .el file exists while the .org file is missing.
    (unless (file-exists-p file)
      (error "File to tangle does not exist: %s" file))
    (when (file-newer-than-file-p file tangled-file)
      (org-babel-tangle-file file
                             tangled-file
                             (rx string-start
                                 (or "emacs-lisp" "elisp")
                                 string-end))
      ;; Make sure that tangled file modification time is
      ;; updated even when `org-babel-tangle-file' does not make changes.
      ;; This avoids re-tangling changed FILE where the changes did
      ;; not affect the tangled code.
      (when (file-exists-p tangled-file)
        (set-file-times tangled-file)))
    (if compile
	    (progn
	      (byte-compile-file tangled-file)
	      (load-file (byte-compile-dest-file tangled-file))
	      (message "Compiled and loaded %s" tangled-file))
      (load-file tangled-file)
      (message "Loaded %s" tangled-file))))

;; Actually load the file we're interested in.
(my/org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory)    ;; <= this file
 (expand-file-name "init-full.el" user-emacs-directory)) ;; <= the tangled file
;; Loading the config:1 ends here
