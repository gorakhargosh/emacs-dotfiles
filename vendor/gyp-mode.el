;;; gyp-mode.el --- GYP major mode for Emacs.
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
;; Keywords: gyp, emacs, major mode
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; Include the following in your .emacs file.
;;
;;     (require 'gyp-mode)
;;
;;; Code:
;;


(define-derived-mode gyp-mode python-mode "GYP"
  "Major mode for editing Generate Your Project files."
  (setq indent-tabs-mode nil
        tab-width 2
        python-indent 2))

(add-to-list 'auto-mode-alist '("\\.gyp$" . gyp-mode))
(add-to-list 'auto-mode-alist '("\\.gypi$" . gyp-mode))

(provide 'gyp-mode)

;;; gyp-mode.el ends here
