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
(defvar default-packages '(anything
                           anything-config
                           anything-complete
                           anything-match-plugin
                           anything-extension
                           anything-obsolete

                           ac-slime
;;                           autopair
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

                           ;; Don't need to install this separately
                           ;; as starter-kit-lisp handles it.
                           clojure-mode
                           clojurescript-mode
                           coffee-mode
                           go-mode
                           haskell-mode
                           less-css-mode
                           markdown-mode
                           js2-mode
                           protobuf-mode
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
(if (system-type-is-darwin-p)
    (setenv "PATH" (concat "/usr/local/bin:/usr/bin" (getenv "PATH"))))
(setenv "PATH" (concat (concat config-dir "bin") (getenv "PATH")))

;;; Load up environment configuration for Mac OS X
(if (system-type-is-darwin-p)
    (require 'config-osx-environment))


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
(add-hook 'emacs-lisp-mode-hook 'onuserinitsave-auto-recompile)
(add-hook 'kill-emacs-hook 'byte-compile-user-init-file t t)

(setq save-abbrevs nil)


(when (not (file-exists-p (concat user-init-file ".elc")))
  (byte-compile-file user-init-file)
  (byte-compile-dotfiles))


;; Automatically compile all modules on startup.
;; Don't enable this because it takes too much time
;; on Mac OS X.
;; (byte-compile-dotfiles)

;; Enable the menu bar.
(menu-bar-mode t)

;; Stop prompting to save abbrevs.
(setq save-abbrevs nil)

(fset 'yes-or-no-p 'y-or-n-p)
(global-linum-mode t)


;; Automatically remove trailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
(ac-config-default)
(global-auto-complete-mode t)
;; (setq ac-auto-start 1)
;; (setq ac-dwim 1)
(setq ac-ignore-case 1)

;; Snippet completion.
(require 'yasnippet)
(setq yas/snippet-dirs '(snippets-dir))
(yas/initialize)
(yas/load-directory snippets-dir)
(yas/global-mode 1)

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;; Programming language modes.
;; Don't enable clojure-mode here as starter-kit-lisp does it
;; also disabling this line allows swank and ac-slime to work without
;; breaking a sweat.
(require 'clojure-mode)
(require 'clojurescript-mode)
(require 'coffee-mode)
(require 'cljdoc)
(require 'markdown-mode)
(require 'gyp-mode)

;; Protobuf mode
(require 'protobuf-mode)
(defconst g-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))
(add-hook 'protobuf-mode-hook
          (lambda () (c-add-style "g-protobuf-style" g-protobuf-style t)))


;; JS2 Mode is way better.
(require 'js2-mode)
(add-hook 'js-mode-hook (lambda () (yas-minor-mode t)))
(add-hook 'js2-mode-hook (lambda () (yas-minor-mode t)))
(eval-after-load 'js2-mode
  '(progn
     (define-key js2-mode-map (kbd "TAB") (lambda()
                                            (interactive)
                                            (let ((yas/fallback-behavior 'return-nil))
                                              (unless (yas/expand)
                                                (indent-for-tab-command)
                                                (if (looking-back "^\s*")
                                                    (back-to-indentation))))))))

;; Python-specific
;;(require 'python)  ;; Disabled because it breaks a lot of shit.
(require 'cython-mode)
;;(autoload 'python-mode "python-mode" "Python mode." t)
;;(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
;;(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Defines the python coding style.
(defun set-python-coding-style ()
  (setq indent-tabs-mode nil)
  (setq require-final-newline 't)
  (setq tab-width 2)
  (setq python-indent-offset 2)
  (setq python-indent 2)
  (setq py-indent-offset 2)
  )
(setq auto-mode-alist
      (append '(
                ("\\wscript$" . python-mode))
              auto-mode-alist))
(add-hook 'rst-adjust-hook 'rst-toc-update)
(add-hook 'python-mode-hook 'set-python-coding-style)

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


(require 'rst)
(setq auto-mode-alist
      (append '(
                ("\\.txt$" . rst-mode)
                ("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode))
              auto-mode-alist))

;; (require 'pysmell)
;; (add-hook 'python-mode-hook (lambda () (pysmell-mode 1)))

;; Don't use tabs when indenting in HTML mode.
(add-hook
 'html-mode-hook
 '(lambda ()
    (setq indent-tabs-mode nil)))

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
(add-to-list 'load-path "/path/to/ack-and-a-half")
(autoload 'ack-and-a-half-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file-same "ack-and-a-half" nil t)
(autoload 'ack-and-a-half-find-file "ack-and-a-half" nil t)
;; Create shorter aliases
(defalias 'ack 'ack-and-a-half)
(defalias 'ack-same 'ack-and-a-half-same)
(defalias 'ack-find-file 'ack-and-a-half-find-file)
(defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)



;;; init.el ends here
