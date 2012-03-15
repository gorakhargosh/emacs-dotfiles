;;; yesudeep.el --- Personal configuration.
;;

(setq user-full-name "Yesudeep Mangalapilly")
(setq user-mail-address "yesudeep@google.com")

;; IRC Settings.
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#appengine" "#python" "#emacs" "#pypy" "#clojure" "##linux-india")))

(defun erc-start ()
  "Connect to IRC"
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "gorakhargosh"))


;;; yesudeep.el ends here.
