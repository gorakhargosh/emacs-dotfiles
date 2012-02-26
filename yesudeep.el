;;; yesudeep.el --- Personal configuration.
;;



;; IRC Settings.
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#appengine" "#python" "#emacs" "#pypy" "#clojure" "##linux-india")))

(defun erc-start ()
  "Connect to IRC"
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "gorakhargosh"))

(provide 'config-erc)

;;; yesudeep.el ends here.
