
;; Taken from: http://www.emacswiki.org/emacs/EmacsCrashCode
;; A few one-liners someone attending the EmacsCrashCourse might put in an InitFile.

;; Many of these settings are not at all typical; some are quite personal, and other still could be either annoying or confusing for beginners. However a “grab bag”-style list of Emacs customizations to experiment with may be quicker than hunting such lines down in the EmacsManual – if they’re even there.

(tool-bar-mode nil)                            ; No toolbar
(set-scroll-bar-mode 'right)                   ; Scrollbar on the right
(setq inhibit-startup-message t)               ; No message at startup
(setq shell-file-name "/bin/bash")             ; Set Shell for M-| command
(setq tex-shell-file-name "/bin/bash")         ; Set Shell used by TeX
(mouse-wheel-mode t)                           ; Mouse-wheel enabled
(setq-default indent-tabs-mode nil)            ; Use spaces instead of tabs
(setq sentence-end-double-space nil)           ; Sentences end with one space
(column-number-mode t)                         ; Show column number in mode-line
(defalias 'qrr 'query-replace-regexp)          ; Define an alias
(global-font-lock-mode 1)                      ; Color enabled
(set-background-color "darkblue")              ; Background color
(set-face-background 'region "gray80")         ; Color for selected lines
(set-cursor-color "black")                     ; Cursor color
(show-paren-mode 1)                            ; Highlight parenthesis pairs
(setq blink-matching-paren-distance nil)       ; Blinking parenthesis
(setq show-paren-style 'expression)            ; Highlight text between parens
(global-hl-line-mode t)                        ; Highlight cursor line
(blink-cursor-mode 0)                          ; No blinking cursor
(setq-default indicate-empty-lines t)          ; Show empty lines
(setq track-eol nil)                           ; Cursor don't track end-of-line
(setq mouse-yank-at-point t)                   ; Paste at cursor position
(setq scroll-preserve-screen-position t)       ; Scroll without moving cursor
(mouse-avoidance-mode 'jump)                   ; Mouse avoids cursor
(setq require-final-newline 't)                ; Always newline at end of file
(setq next-line-add-newlines t)                ; Add newline when at buffer end
(setq truncate-partial-width-windows nil)      ; Don't truncate long lines
(setq make-backup-files nil)                   ; No backup files ~
(setq visible-bell t)                          ; No beep when reporting errors
(setq window-min-height 10)                    ; Minimal height of windows
(setq auto-save-timeout 60)                    ; Autosave every minute
(setq default-major-mode 'text-mode)           ; Text-mode is default mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)  ; auto-formatting in text-mode
(defalias 'yes-or-no-p 'y-or-n-p)              ; y/n instead of yes/no
(icomplete-mode t)                             ; Completion in mini-buffer
(setq pop-up-frame t)                          ; Buffers in separate frames
(setq frame-title-format "%b - emacs")         ; Use buffer name as frame title
(global-set-key "\C-x\C-b" 'buffer-menu)       ; CxCb puts point on buffer list
(setq enable-recursive-minibuffers t)          ; Stack  minibuffers
(desktop-save-mode t)                          ; Save session before quitting
(setq confirm-kill-emacs 'yes-or-no-p)          ; Confirm quit
(speedbar t)                                   ; Quick file access with bar
(setq suggest-key-bindings nil)                ; No hints for M-x
(setq ispell-dictionary "english")             ; Set ispell dictionary
(setq calendar-week-start-day 1)               ; Week starts monday
(setq european-calendar-style 't)              ; European style calendar
(global-unset-key "\C-x\C-v")                  ; Suppress a shortcut
(setq grep-command "grep -i -nH -e ")          ; Set grep command options
(cua-mode t)                                   ; Cut/Paste with C-x/C-c/C-v
(setq ps-paper-type 'a4)                       ; Specify printing format
(setq-default case-fold-search t)              ; Search is case sensitive
(setq tab-width 4)                             ; Length of tab is 4 SPC
(require 'cl)                                  ; Use Common Lisp features
                                        ; Note: There are some problems
                                        ;  with this. It might be better
                                        ;  to just use it when compiling
                                        ; elisp files, see the elisp manual.
(add-to-list 'default-frame-alist '(font . "")); Change fonts
(windmove-default-keybindings)                 ; Shift arrows switch windows
                                        ; Note: This interferes with
                                        ;  cua-mode where shift arrow starts
                                        ;  marking. An alternative is
                                        ;  (windmove-default-keybindings 'meta)
(put 'narrow-to-region  'disabled nil)         ; Allow narrow-to-region command
(setq disabled-command-hook nil)               ; Allow all disabled commands
(define-key menu-bar-tools-menu [games] nil)   ; Remove games menu
(setq undo-limit 100000)                       ; Increase number of undo
(setq exec-path (append exec-path '("/bin")))  ; Change binary path

;; Some of the functions called with a ‘nil’ argument work slightly better with a negative numeric argument. For example the following are called with ‘nil’ but their help in Emacs 23.X mentions that

(tool-bar-mode nil)                            ; No toolbar
(mouse-wheel-mode t)                           ; Mouse-wheel enabled
(column-number-mode t)                         ; Show column number in mode-line
(global-hl-line-mode t)                        ; Highlight cursor line
(blink-cursor-mode 0)                          ; No blinking cursor
(icomplete-mode t)                             ; Completion in mini-buffer
(desktop-save-mode t)                          ; Save session before quitting
(speedbar t)                                   ; Quick file access with bar
(cua-mode t)                                   ; Cut/Paste with C-x/C-c/C-v
