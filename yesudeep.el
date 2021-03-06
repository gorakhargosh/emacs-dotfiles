;;; yesudeep.el --- Personal configuration.
;;

(setq user-full-name "Yesudeep Mangalapilly")
(setq user-mail-address "yesudeep@google.com")

;;(set-face-attribute 'default nil :family "Monaco" :height 100)
;;(set-face-attribute 'default nil :family "Consolas" :height 140)

;; I like 70 as the margin width.
(setq-default fill-column 70)



;; IRC Settings.
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#appengine" "#python" "#emacs" "#pypy" "#clojure" "##linux-india")))

(defun erc-start ()
  "Connect to IRC"
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "gorakhargosh"))


;; Twitter settings
;; Requires installing gnupg.
;; See http://www.emacswiki.org/emacs/TwitteringMode
;; On Max OS X, do `brew install gnupg`.
;; (require 'twittering-mode)
;; (setq twittering-use-master-password t)


;;; yesudeep.el ends here.
