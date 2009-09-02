;; =========================================================================
;; Anthony Green's GNU Emacs configuration file.
;;
;; Copyright (C) 2009  Anthony Green <green@moxielogic.com>
;;
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.
;; =========================================================================

;; -------------------------------------------------------------------------
;; ---- Load private settings (passwords, etc) -----------------------------
;; -------------------------------------------------------------------------

(load "~/.emacs.d/private.el")

;; -------------------------------------------------------------------------
;; ---- Auto byte-compile this file ----------------------------------------
;; -------------------------------------------------------------------------

(defun byte-compile-init-file ()
  (when (equal buffer-file-name user-init-file)
     (let ((byte-compile-warnings '(unresolved)))
          (when (file-exists-p (concat user-init-file ".elc"))
                (delete-file (concat user-init-file ".elc")))
          (byte-compile-file user-init-file)
          (message "Just byte-compiled %s " user-init-file))))

(add-hook 'kill-buffer-hook 'byte-compile-init-file)

;; -------------------------------------------------------------------------
;; ---- Desktop save and restore -------------------------------------------
;; -------------------------------------------------------------------------

(require 'desktop)
(desktop-save-mode t)
(setq history-lenth 1500)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(desktop-read)

;; -------------------------------------------------------------------------
;; ---- add-change-log configuration ---------------------------------------
;; -------------------------------------------------------------------------

(setq add-log-full-name "Anthony Green"
      add-log-mailing-address "green@moxielogic.com"
      add-log-keep-changes-together t)

;; -------------------------------------------------------------------------
;; ---- Tom Tromey's ELPA --------------------------------------------------
;; -------------------------------------------------------------------------

(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;; -------------------------------------------------------------------------
;; ---- Verilog Mode -------------------------------------------------------
;; -------------------------------------------------------------------------

(autoload 'verilog-mode "verilog-mode" "Verilog mode" t)
(add-to-list 'auto-mode-alist '("\\.[ds]?v\\'" . verilog-mode))

;; This uses icarus verilog as the linting tool.
(setq verilog-indent-level             2
      verilog-indent-level-module      2
      verilog-indent-level-declaration 2
      verilog-indent-level-behavioral  2
      verilog-indent-level-directive   1
      verilog-case-indent              2
      verilog-auto-newline             t
      verilog-auto-indent-on-newline   t
      verilog-tab-always-indent        t
      verilog-auto-endcomments         t
      verilog-minimum-comment-distance 40
      verilog-indent-begin-after-if    t
      verilog-auto-lineup              'declarations
      verilog-highlight-p1800-keywords nil
      verilog-linter		       "iverilog -t null"
      verilog-tool                     'verilog-linter
      )

;; -------------------------------------------------------------------------
;; ---- SLIME --------------------------------------------------------------
;; -------------------------------------------------------------------------

(add-to-list 'load-path "~/.emacs.d/slime/")  ; my SLIME directory
(setq inferior-lisp-program "sbcl")           ; my Lisp system
(require 'slime)
(slime-setup)

;; -------------------------------------------------------------------------
;; ---- Linux Kernel Hacking -----------------------------------------------
;; -------------------------------------------------------------------------

;; These settings are recommended by the kernel's
;; Documentation/CodingStyle file.  Look below and you'll see that we
;; only set linux-tabs-only when we're looking at files under a
;; "linux-2.6" directory.

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "/linux-2.6/")
                                       filename))
                (setq indent-tabs-mode t)
                (c-set-style "linux-tabs-only")))))

;; -------------------------------------------------------------------------
;; ---- ERC Configuration --------------------------------------------------
;; -------------------------------------------------------------------------

; My autojoin list
(setq erc-autojoin-channels-alist
      '((".freenode.net$" . 
	 ("#gdb" "#uclibc" "#lisp" "#fpga" "#qemu" "#classpath"))
	(".oftc.net$" . ("#gcc" "#kernelnewbies" "#openjdk"))))

; Command for joining IRC.
(defun atg/erc ()
  "Connect to my IRC servers/channels."
  (interactive)
  (progn
   (atg/erc-freenode)
   (atg/erc-oftc)))

(defun atg/erc-freenode () "Connect to Freenode."
  (erc-select :server "irc.freenode.net" 
	      :port 6667 
	      :nick private/freenode-nick 
	      :password private/freenode-password 
	      :full-name private/freenode-fullname))

(defun atg/erc-oftc () "Connect to OFTC."
  (erc-select :server "irc.oftc.net" :port 6667 
	      :port 6667 
	      :nick private/oftc-nick 
	      :password private/oftc-password 
	      :full-name private/oftc-fullname))

(defun erc-autojoin-channels (server nick)
 (dolist (l erc-autojoin-channels-alist)
    (when (string-match (car l) server)
      (dolist (chan (cdr l))
        (erc-send-command (concat "join " chan))))
    (add-hook 'erc-after-connect 'erc-autojoin-channels)))

; Don't bother me with certain kinds of IRC messages.
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477"))
(setq erc-track-exclude-server-buffer t)

;; -------------------------------------------------------------------------
;; ---- User Interface and Miscelleneous Editing Tweaks --------------------
;; -------------------------------------------------------------------------

; All files should end with a newline.
(setq require-final-newline t) 

; Turn off the annoying tool bar.
(tool-bar-mode -1)

; Disable the startup screen.
(setq inhibit-startup-screen t)

; Make the current selection visible.
(transient-mark-mode t)

; Mode line settings.
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

; Give meaningful names to buffers with the same name.
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

; Cycle through buffers.
(global-set-key (kbd "<C-tab>") 'bury-buffer)

; hippie-expand!
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
					 try-expand-dabbrev-all-buffers 
					 try-expand-dabbrev-from-kill
					 try-complete-file-name-partially 
					 try-complete-file-name
					 try-expand-all-abbrevs 
					 try-expand-list 
					 try-expand-line
					 try-complete-lisp-symbol-partially 
					 try-complete-lisp-symbol))
(global-set-key (kbd "M-/") 'hippie-expand)

; Show matching parens
(show-paren-mode t)

; Delete the selection area with a keypress
(delete-selection-mode t)

