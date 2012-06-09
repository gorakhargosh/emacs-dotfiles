;;; config-bindings.el --- Key bindings
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
;; Keywords: convenience, key bindings, keyboard shortcuts
;; Package-Requires:: ((starter-kit "2.0.1"))
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; (require 'config-bindings)
;;
;;; Code:
;;


(progn

  ;;; Based on the starter-kit-bindings.el package.

  ;; Helm M-x is awesome.
  ;;(global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-c h") 'helm-mini)

  ;; Font size
  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C--") 'text-scale-decrease)
  (define-key global-map (kbd "C-=") 'text-scale-increase)

  ;; Use regex searches by default
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "\C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward)

  ;; Jump to a definition in the current file. (Protip: this is
  ;; awesome.)
  ;;(global-set-key (kbd "C-x C-i") 'esk-ido-menu)

  ;; File finding
  (global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
  (global-set-key (kbd "C-c y") 'bury-buffer)
  (global-set-key (kbd "C-c r") 'revert-buffer)
  (global-set-key (kbd "C-x f") 'helm-for-files)
  ;; Toggle sr-speedbar/nav
  ;;(global-set-key (kbd "s-s") 'sr-speedbar-toggle)
  (global-set-key (kbd "C-x C-a") 'nav)

  ;; Code search using ack
  ;;(global-set-key (kbd "s-f") 'ack)

  ;; Should be able to eval-and-replace anywhere.
  (global-set-key (kbd "C-c e") 'esk-eval-and-replace)

  ;; Line movement.
  ;; M-<up> and M-<down> conflict with paredit-mode, so we're using
  ;; M-S-<up> and M-S-<down> instead.
  (global-set-key (kbd "M-S-<up>") 'move-text-up)
  (global-set-key (kbd "M-S-<down>") 'move-text-down)

  ;; Automatically indent on return.
  (define-key global-map (kbd "RET") 'newline-and-indent)

  ;; Killing and yanking.
  (define-key global-map (kbd "<delete>") 'delete-char)
  (define-key global-map (kbd "M-<delete>") 'kill-word)
  ;; (global-set-key (kbd "C-k") 'kill-whole-line)
  ;; (global-set-key (kbd "C-S-<backspace>") 'kill-and-join-forward)

  ;; Line insertion
  ;;(global-set-key (kbd "C-S-<return>") 'insert-empty-line-above)
  (global-set-key (kbd "S-<return>") 'insert-empty-line-below)
  (global-set-key (kbd "s-<return>") 'insert-empty-line-below-next-line)
  (global-set-key (kbd "M-S-<return>") 'insert-empty-line-above)

  ;; Line or region duplication.
  (global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)
  (global-set-key (kbd "C-c C-d") 'duplicate-current-line-or-region)

  ;; Clean up whitespace
  (global-set-key (kbd "C-c n") 'tidy-buffer)

  ;; Text selection.
  (global-set-key (kbd "M-8") 'extend-selection)
  (global-set-key (kbd "M-*") 'select-text-in-quote)

  ;; Fix the mac delete key.
  ;;(global-set-key (kbd "<kp-delete>") 'delete-forward-char)
  ;;(global-set-key (kbd "C-<kp-delete>") 'kill-word)
  ;;;;(define-key global-map (kbd "<kp-delete>") 'delete-forward-char)
  ;;;;(define-key global-map [\d] 'delete-forward-char)

  ;; Shift regions left or right.
  (global-set-key [C-S-right] 'shift-right)
  (global-set-key [C-S-left] 'shift-left)
  (global-set-key (kbd "s-]") 'shift-right)
  (global-set-key (kbd "s-[") 'shift-left)

  ;; Byte recompile the entire emacs directory.
  (global-set-key [C-S-M-down] 'byte-compile-dotfiles)
  (global-set-key [C-S-M-up] 'reload-user-init-file)

  ;; Adds an alternative binding for yas/expand for cases when TAB
  ;; doesn't work.
  (global-set-key (kbd "M-?") 'yas/expand)


  ;; Show-hide
  (global-set-key (kbd "C-c M-<right>") 'hs-show-block)
  (global-set-key (kbd "C-c M-S-<right>") 'hs-show-all)
  (global-set-key (kbd "C-c M-<left>") 'hs-hide-block)
  (global-set-key (kbd "C-c M-S-<left>") 'hs-hide-all)

  ;; Magit
  (global-set-key (kbd "C-x g") 'magit-status)

  ;; Reload init file.
  (global-set-key (kbd "C-.") 'reload-current-init-file)

  ;; Find things fast.
  ;;(global-set-key '[f5] 'find-file-in-project)
  (global-set-key '[f5] 'ftf-find-file)
  ;; (global-set-key '[f5] 'helm-find-files)
  (global-set-key '[f6] 'ftf-grepsource)
  ;;(global-set-key '[f4] 'ftf-gdb)
  ;;(global-set-key '[f5] 'ftf-compile)

  ;; Toggle between camel-case and underscores.
  (global-set-key (kbd "M-_") 'toggle-identifier-naming-style)

  ;;(global-set-key (kbd "M-\"") 'paredit-doublequote)
  ;;(global-set-key (kbd "M-'") 'paredit-singlequote)
  )

(provide 'config-bindings)

;;; config-bindings.el ends here
