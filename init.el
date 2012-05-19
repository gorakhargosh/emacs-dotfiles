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
(setq mouse-wheel-scroll-amount '(7 ((shift) . 1) ((control) . nil)))

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

;; Loads a color theme.
;;(load-theme 'wombat t)

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

;; Load programming-specific stuff.
(require 'config-programming)

;; RFC specific
;; (require 'irfc)
;; (setq irfc-directory "~/.emacs.d/rfc")
;; (setq irfc-assoc-mode t)

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
;;(require 'findr)

(require 'helm-config)
(helm-mode 1)

;;; init.el ends here
