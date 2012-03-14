;;; yesudeep.el --- Personal configuration.
;;

(setq user-full-name "Yesudeep Mangalapilly")
(setq user-mail-address "yesudeep@google.com")

;; Don't use tabs when indenting in HTML mode.
(add-hook
 'html-mode-hook
 '(lambda ()
    (setq indent-tabs-mode nil)))

;; Defines the python coding style.
(defun set-python-coding-style ()
  (setq indent-tabs-mode nil)
  (setq require-final-newline 't)
  (setq tab-width 2)
  (setq py-indent-offset 2)
  (setq python-indent 2))

(add-hook 'python-mode-hook 'set-python-coding-style)

;; IRC Settings.
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#appengine" "#python" "#emacs" "#pypy" "#clojure" "##linux-india")))

(defun erc-start ()
  "Connect to IRC"
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "gorakhargosh"))

(provide 'config-erc)

;;; yesudeep.el ends here.
