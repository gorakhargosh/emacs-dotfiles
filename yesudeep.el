;;; yesudeep.el --- Personal configuration.
;;

(setq user-full-name "Yesudeep Mangalapilly")
(setq user-mail-address "yesudeep@google.com")

;;(set-face-attribute 'default nil :family "Monaco" :height 100)
(set-face-attribute 'default nil :family "Consolas" :height 140)

;; IRC Settings.
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#appengine" "#python" "#emacs" "#pypy" "#clojure" "##linux-india")))

(defun erc-start ()
  "Connect to IRC"
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "gorakhargosh"))


;;; yesudeep.el ends here.
