;;; config-bindings.el --- Some default key bindings.
;;
;; Copyright (C) 2008-2010 Phil Hagelberg and contributors
;; Copyright (C) 2011 Yesudeep Mangalapilly
;;
;; Author: Yesudeep Mangalapilly <yesudeep@gmail.com>
;; Keywords: convenience
;; Package-Requires:: ((starter-kit "2.0.1"))
;;
;; This file is not a part of GNU Emacs.
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(progn

  ;;; Based on the starter-kit-bindings.el package.
  
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
  (global-set-key (kbd "C-x C-i") 'esk-ido-menu)

  ;; File finding
  (global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
  (global-set-key (kbd "C-x f") 'esk-recentf-ido-find-file)
  (global-set-key (kbd "C-c y") 'bury-buffer)
  (global-set-key (kbd "C-c r") 'revert-buffer)
  
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
  (global-set-key (kbd "C-k") 'kill-whole-line)
  (global-set-key (kbd "C-S-<backspace>") 'kill-and-join-forward)

  ;; Line insertion
  (global-set-key (kbd "C-S-<return>") 'insert-empty-line-above)
  (global-set-key (kbd "S-<return>") 'insert-empty-line-below)
  (global-set-key (kbd "s-<return>") 'insert-empty-line-below-next-line)

  ;; Clean up whitespace
  (global-set-key (kbd "C-c n") 'tidy-buffer)

  ;; Fix the mac delete key.
  ;;(global-set-key (kbd "<kp-delete>") 'delete-forward-char)
  ;;(global-set-key (kbd "C-<kp-delete>") 'kill-word)
  ;;;;(define-key global-map (kbd "<kp-delete>") 'delete-forward-char)
  ;;;;(define-key global-map [\d] 'delete-forward-char)
  
  )

(provide 'config-bindings)

;;; config-bindings.el ends here
