;; Programming language modes.
(require 'clojure-mode)
(require 'clojurescript-mode)
(require 'coffee-mode)
(require 'cljdoc)
(require 'markdown-mode)
(require 'gyp-mode)

;; Slime autocomplete.
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

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


(provide 'config-programming)
