;;; init.el --- Emacs initialization module
;;
;; Copyright 2012 Google, Inc.
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;    http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; Author: yesudeep@google.com (Yesudeep Mangalapilly)
;; Keywords: emacs, initialization
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; Just clone the repo into ~/.emacs.d/
;;
;;; Code:
;;


;; ***************************************************************************
;; Automatically install default packages.
;; ---------------------------------------------------------------------------
;; This solves the problem of installing
;; these packages if you move between systems frequently and want them
;; to remain in a consistent state. You may have to close and restart
;; Emacs several times for the procedure to complete before it is
;; usable. I've had to do about 3 such restarts when doing a fresh clone.

(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Automatically install these packages if they aren't present.
(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar default-packages '(
                           ac-slime
                           gist
                           highlight-indentation
                           move-text
                           starter-kit
                           starter-kit-bindings
                           starter-kit-eshell
                           starter-kit-js
                           starter-kit-lisp
                           yasnippet
                           yasnippet-bundle
                           clojure-mode
                           clojurescript-mode
                           coffee-mode
                           go-mode
                           haskell-mode
                           less-css-mode
                           markdown-mode
                           js-comint
;;                           pysmell
                           ;;js2-mode
                           protobuf-mode
                           yaml-mode
                           find-things-fast
                           ))

(dolist (p default-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; ***************************************************************************
;; Configuration root and other system paths.
;; ---------------------------------------------------------------------------

;; Configuration root.
(setq config-dir (file-name-directory (or (buffer-file-name) load-file-name)))
(setq vendor-library-dir (expand-file-name (concat config-dir "vendor")))
(setq auto-complete-dict-dir (concat vendor-library-dir "auto-complete/dict"))
(setq auto-complete-dict-dir-local (concat config-dir "auto-complete/dict"))
(setq snippets-dir (concat config-dir "snippets"))


(add-to-list 'load-path config-dir)
(add-to-list 'load-path vendor-library-dir)
(let ((default-directory vendor-library-dir))
  (normal-top-level-add-subdirs-to-load-path))
;; (let ((default-directory vendor-library-dir))
;;   (normal-top-level-add-to-load-path '("your" "subdirectories" "here")))


;; Functions to determine the platform on which we're running.
(defun system-type-is-darwin-p ()
  (interactive)
  "Return true if system is darwin-based (Mac OS X)"
  (string-equal system-type "darwin"))
(defun system-type-is-linux-p ()
  (interactive)
  "Return true if system is GNU/Linux-based."
  (string-equal system-type "gnu/linux"))


;;; A quick & ugly PATH solution to Emacs on Mac OSX
;; (if (system-type-is-darwin-p)
;;     (setenv "PATH" (concat "/usr/local/bin:/usr/bin" (getenv "PATH"))))
;; (setenv "PATH" (concat (concat config-dir "bin") (getenv "PATH")))

;;; Load up environment configuration for Mac OS X
;; (if (system-type-is-darwin-p)
;;     (require 'config-osx-environment))

;; (if (system-type-is-darwin-p)
;;     (progn
;;       (add-to-list 'exec-path "/usr/bin")
;;       (add-to-list 'exec-path "/usr/sbin")
;;       (add-to-list 'exec-path "/usr/local/bin")
;;       (add-to-list 'exec-path "/usr/local/sbin")))

;; (add-to-list 'exec-path "~/bin")
;; (add-to-list 'exec-path "~/Applications/bin")

;; ;; read in PATH from .bashrc
;; (if (system-type-is-darwin-p)
;;     (if (not (getenv "TERM_PROGRAM"))
;;        (setenv "PATH"
;;                (shell-command-to-string "source $HOME/.profile && printf $PATH"))))


;; ***************************************************************************
;; Automatically recompile the emacs init file on buffer-save or exit
;; ---------------------------------------------------------------------------
;; TODO: Write a module that automatically generates .elc files.
;; - on emacs load
;; - on buffer saves
;; - when .elc files are stale

(defun reload-user-init-file ()
  "thisandthat."
  (interactive)
  (byte-compile-user-init-file)
  (load-file (concat config-dir "init.el")))

(defun byte-compile-dotfiles ()
  "Byte compile all Emacs dotfiles."
  (interactive)
  ;; Automatically recompile the entire .emacs.d directory.
  (byte-recompile-directory (expand-file-name config-dir) 0))

(defun byte-compile-user-init-file ()
  (let ((byte-compile-warnings '(unresolved)))
    ;; in case compilation fails, don't leave the old .elc around:
    (when (file-exists-p (concat user-init-file ".elc"))
      (delete-file (concat user-init-file ".elc")))
    (byte-compile-file user-init-file)
    ;;(byte-compile-dotfiles)
    ;; (message "%s compiled" user-init-file)
    ))

(defun onuserinitsave-auto-recompile ()
  (when (equal buffer-file-name user-init-file)
    (add-hook 'after-save-hook 'byte-compile-user-init-file t t)))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
;;(add-hook 'emacs-lisp-mode-hook 'onuserinitsave-auto-recompile)
(add-hook 'kill-emacs-hook 'byte-compile-user-init-file t t)

(setq save-abbrevs nil)

;; Automatic compilation of Emacs lisp modules.
;; (when (not (file-exists-p (concat user-init-file ".elc")))
;;   (byte-compile-file user-init-file)
;;   (byte-compile-dotfiles))


;; Automatically compile all modules on startup.
;; Don't enable this because it takes too much time
;; on Mac OS X.
;; (byte-compile-dotfiles)

;; Make the mouse wheel scroll progressive.
;;(setq mouse-wheel-progressive-speed nil)

;; Make the mouse wheel scroll faster.
(setq mouse-wheel-scroll-amount '(5 ((shift) . 1) ((control) . nil)))

;; Enable the menu bar.
(menu-bar-mode t)

;; Stop prompting to save abbrevs.
(setq save-abbrevs nil)

;; Automatically reload files changed on disk.
(global-auto-revert-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(global-linum-mode t)

;; Automatically remove trailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Advise find-file to transparently create necessary directories.
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))


;; To be able to find files in a project quickly.
;; Bindings are defined in config-bindings.
(require 'find-things-fast)


;; Simultaneously edit regions of buffers. Also enable narrowing
;; which is useful with iedit.
(require 'iedit)
(put 'narrow-to-region 'disabled nil)


;; Key bindings and editing.
(require 'move-text)
;;(move-text-default-bindings)
(require 'config-defuns)
(require 'config-bindings)

;; Enables mouse scrolling when using Emacs in the terminal.
(unless window-system
  (xterm-mouse-mode 1)
  (global-set-key [mouse-4] '(lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                               (interactive)
                               (scroll-up 1)))
  )


;; https://github.com/bbatsov/emacs-prelude/commit/d26924894b31d5dc3a8b2813719579baccc2b433
(when (system-type-is-darwin-p)
  (defun copy-from-osx ()
    (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx)
  )

;; Automatically pair pairable symbols like (), '', "", [], <>, etc.
(require 'autopair)
(autopair-global-mode)
;; Prevents: http://code.google.com/p/autopair/issues/detail?id=32
(add-hook 'sldb-mode-hook #'(lambda () (setq autopair-dont-activate t)))
(set-default 'autopair-dont-activate #'(lambda () (eq major-mode 'sldb-mode)))

;; Automatic completion and suggestions.
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories auto-complete-dict-dir)
(add-to-list 'ac-dictionary-directories auto-complete-dict-dir-local)
(ac-config-default)
(global-auto-complete-mode t)
;; (setq ac-auto-start 1)
;; (setq ac-dwim 1)
(setq ac-ignore-case 1)
;; Don't allow tab to cycle. It's irritating.
(define-key ac-completing-map "\t" 'ac-complete)
(setq-default ac-sources
              (add-to-list 'ac-sources
                           'ac-source-dictionary))

(add-to-list 'ac-modes 'shell-mode)
(defun set-up-shell-mode-ac ()
  (setq ac-sources '(
                     ac-source-symbols
                     ac-source-abbrev
                     ac-source-symbols
                     ac-source-words-in-buffer
                     ac-source-files-in-current-dir
                     ac-source-words-in-same-mode-buffers
                     ac-source-filename)))
(add-hook 'shell-mode-hook 'set-up-shell-mode-ac)

;; Snippet completion.
(require 'yasnippet)
(setq yas/snippet-dirs '(snippets-dir))
(yas/initialize)
(yas/load-directory snippets-dir)
(yas/global-mode 1)
;; Add yasnippets to autocompletion sources.
(add-to-list 'ac-sources 'ac-source-yasnippet)

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;; Programming language modes.
(require 'clojure-mode)
(require 'clojurescript-mode)
(require 'coffee-mode)
(require 'cljdoc)
(require 'markdown-mode)
(require 'gyp-mode)

;; Shell script mode.
(defun set-sh-mode-coding-style ()
  (setq sh-basic-offset 2
        sh-indentation 2))
(add-hook 'sh-mode-hook 'set-sh-mode-coding-style)

;; CSS and LESS.
(require 'less-css-mode)
(defun set-less-css-coding-style ()
  (setq less-css-indent-level 2
        indent-tabs-mode nil
        require-final-newline 't
        tab-width 2))
(add-hook 'less-css-mode-hook 'set-less-css-coding-style)
(defun set-css-coding-style ()
  (setq css-indent-offset 2
        indent-tabs-mode nil
        require-final-newline 't
        tab-width 2))
(add-hook 'css-mode-hook 'set-css-coding-style)
(defun set-up-less-css-ac ()
  (setq ac-sources '(
                      ac-source-words-in-buffer
                      ac-source-abbrev
                      ac-source-symbols
                      ac-source-variables
                      ac-source-words-in-same-mode-buffers
                      ac-source-yasnippet
                      )))
(add-hook 'css-mode-hook 'set-up-less-css-ac)
(add-hook 'less-css-mode-hook 'set-up-less-css-ac)
(add-to-list 'ac-modes 'less-css-mode)


;; Protobuf mode
(require 'protobuf-mode)
(defconst g-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))
(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "g-protobuf-style" g-protobuf-style t)))

;; JavaScript-specific stuff.
(defun set-up-javascript-ac ()
  (setq ac-sources '(
                      ac-source-words-in-buffer
;;                      ac-source-filename
;;                      ac-source-files-in-current-dir
                      ac-source-abbrev
                      ac-source-functions
                      ac-source-symbols
                      ac-source-variables
                      ac-source-words-in-same-mode-buffers
                      ac-source-yasnippet
                      )))
(add-hook 'js-mode-hook 'set-up-javascript-ac)
(setq js-indent-level 2)
(defun setup-hs-minor-mode ()
  (imenu-add-menubar-index)
  (hs-minor-mode t))
(add-hook 'js-mode-hook 'setup-hs-minor-mode)

;; Node JavaScript REPL using js-comint and nodejs.
(require 'js-comint)
;; Use node as our repl
(setq inferior-js-program-command "node")
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list 'comint-preoutput-filter-functions
                     (lambda (output)
                       (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
                     (replace-regexp-in-string ".*1G.*3G" "js> " output))))))


;; JS2 Mode is way better.
;; (require 'js2-mode)
;; (add-hook 'js-mode-hook (lambda () (yas-minor-mode t)))
;; (add-hook 'js2-mode-hook (lambda () (yas-minor-mode t)))
;; (eval-after-load 'js2-mode
;;   '(progn
;;      (define-key js2-mode-map (kbd "TAB") (lambda()
;;                                             (interactive)
;;                                             (let ((yas/fallback-behavior 'return-nil))
;;                                               (unless (yas/expand)
;;                                                 (indent-for-tab-command)
;;                                                 (if (looking-back "^\s*")
;;                                                     (back-to-indentation))))))))

;; Python-specific
;;(require 'python)  ;; Disabled because it breaks a lot of shit.
(require 'cython-mode)
;;(autoload 'python-mode "python-mode" "Python mode." t)
;;(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
;;(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Defines the python coding style.
(defun set-python-coding-style ()
  (setq indent-tabs-mode nil
        require-final-newline 't
        tab-width 2
        python-indent-offset 2
        python-indent 2
        py-indent-offset 2))
(setq auto-mode-alist
      (append '(
                ("\\wscript$" . python-mode)
                ("\\SConstruct" . python-mode)
                ("\\SConscript" . python-mode)
                ("\\BUILD$" . python-mode))
              auto-mode-alist))
(add-hook 'rst-adjust-hook 'rst-toc-update)
(add-hook 'python-mode-hook 'set-python-coding-style)

;; Python linting and code compliance.
;; Requires installing pep8 and pylint using pip:
;;
;;   sudo pip install pep8 pylint
;;
(require 'python-pep8)
(require 'python-pylint)
(autoload 'python-pep8 "python-pep8")
(autoload 'pep8 "python-pep8")
(autoload 'python-pylint "python-pylint")
(autoload 'pylint "python-pylint")

(defun set-up-python-ac ()
  "Sets up python-mode autocomplete"
  (set (make-local-variable 'ac-sources)
       (append ac-sources '(ac-source-yasnippet))))
(add-hook 'python-mode-hook 'set-up-python-ac)

;; (defun onsave-run-pylint ()
;;   "Runs pylint on the code as soon as the Python file is saved."
;;   (add-hook 'after-save-hook 'python-pylint))
;; (add-hook 'python-mode-hook 'onsave-run-pylint)

;; Autocompletion using pysmell.
;; (require 'pysmell)
;; (add-hook 'python-mode-hook (lambda () (pysmell-mode 1)))

;; (require 'pysmell)
;; (defvar ac-source-pysmell
;;   '((candidates
;;      . (lambda ()
;;          (require 'pysmell)
;;          (pysmell-mode 1)
;;          (pysmell-get-all-completions))))
;;   "Source for PySmell")

;; (add-hook 'python-mode-hook
;;           '(lambda ()
;;              (set (make-local-variable 'ac-sources) (append ac-sources '(ac-source-pysmell)))))


;; Autocompletion using ropemacs.
;; http://www.emacswiki.org/emacs/AutoCompleteSources#toc2
;; This is too slow at the moment on OS X.
;; (require 'pymacs)
;; (setq pymacs-load-path '("~/.emacs.d/vendor/rope"
;;                          "~/.emacs.d/vendor/ropemode"
;;                          "~/.emacs.d/vendor/ropemacs"))
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)
;; (ac-ropemacs-initialize)
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-ropemacs)))

;; (setq ipython-command "ipython")
;; (require 'ipython)

;; YAML mode.
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
;; YAML autocomplete configuration.
(defun set-up-yaml-mode-ac ()
  (setq ac-sources '(
                     ac-source-symbols
                     ac-source-abbrev
                     ac-source-symbols
                     ac-source-words-in-buffer
                     ac-source-files-in-current-dir
                     ac-source-words-in-same-mode-buffers
                     ac-source-filename)))
(add-to-list 'ac-modes 'yaml-mode)
(add-hook 'yaml-mode-hook 'set-up-yaml-mode-ac)

;; Restructured text.
(require 'rst)
(setq auto-mode-alist
      (append '(
                ("\\.txt$" . rst-mode)
                ("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode))
              auto-mode-alist))


;; HTML and markup specific
;; zencoding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)
(add-hook 'sgml-mode-hook 'orgtbl-mode)

;; Don't use tabs when indenting in HTML mode.
(add-hook
 'html-mode-hook
 '(lambda ()
    (set (make-local-variable 'sgml-basic-offset) 2)
    (setq indent-tabs-mode nil)))

;; Soy-mode for closure templates.
(require 'soy-mode)
(add-hook
 'soy-mode-hook
 '(lambda ()
    ;; Don't use tabs when indenting in HTML mode.
    (setq indent-tabs-mode nil
          tab-width 2)
    (set (make-local-variable 'sgml-basic-offset) 2)
    (yas-minor-mode t)))

;; RFC specific
(require 'irfc)
(setq irfc-directory "~/.emacs.d/rfc")
(setq irfc-assoc-mode t)

(require 'nav)
;;(nav)
;; (require 'sr-speedbar)
;; (sr-speedbar-open)

;; Pastebin (gist.github.com)
(require 'gist)

;; Highlights indentation levels.
(require 'highlight-indentation)
(add-hook 'python-mode-hook 'highlight-indentation)
(add-hook 'coffee-mode-hook 'highlight-indentation)
(add-hook 'html-mode-hook 'highlight-indentation)

;; Ack searches for code.
;; (add-to-list 'load-path "/path/to/ack-and-a-half")
(autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)

;; Wrap regions.
;; (require 'wrap-region)
;; (wrap-region-global-mode t)

;; Find and replace using regular expressions in an entire directory
;; of files.
(require 'findr)

(require 'helm-config)
(helm-mode 1)

;;; init.el ends here
