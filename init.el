;; =========================================================================
;; Anthony Green's GNU Emacs configuration file.
;;
;; Copyright (C) 2009, 2017  Anthony Green <green@moxielogic.com>
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
;; ---- Tom Tromey's ELPA --------------------------------------------------
;; -------------------------------------------------------------------------

(require 'package)
(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

;; -------------------------------------------------------------------------
;; ---- set some load paths ------------------------------------------------
;; -------------------------------------------------------------------------

(eval-and-compile
  (mapc #'(lambda (path)
	    (add-to-list 'load-path
			 (expand-file-name path "/home/green/.emacs.d")))
	'("site-lisp" "lisp" "override"))

  (require 'cl))

(require 'use-package)

;; -------------------------------------------------------------------------
;; ---- Enable file encryption/decryption ----------------------------------
;; -------------------------------------------------------------------------

(require 'epa-file)
(epa-file-enable)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)
     
;; -------------------------------------------------------------------------
;; ---- BBDB ---------------------------------------------------------------
;; -------------------------------------------------------------------------

(use-package bbdb-com
  :load-path "override/bbdb/lisp"
  :commands bbdb-create
  :bind ("M-B" . bbdb)
  :config

  (use-package bbdb-vcard
    :disabled t
    :load-path "site-lisp/bbdb-vcard")

  (use-package bbdb-vcard-export
    :disabled t)

  (use-package bbdb-vcard-import
	       :disabled t))
(require 'bbdb)

;; -------------------------------------------------------------------------
;; ---- GNUS ---------------------------------------------------------------
;; -------------------------------------------------------------------------

(use-package dot-gnus
  :load-path ("override/gnus/lisp" "override/gnus/contrib")
  :bind (("M-G"   . switch-to-gnus)
         ("C-x m" . compose-mail))
  :init
  (setq gnus-init-file (expand-file-name "dot-gnus" user-emacs-directory)
        gnus-home-directory "~/Messages/Gnus/"))

;; -------------------------------------------------------------------------
;; ---- add-change-log configuration ---------------------------------------
;; -------------------------------------------------------------------------

(setq add-log-full-name "Anthony Green"
      add-log-mailing-address "green@moxielogic.com"
      add-log-keep-changes-together t)

;; -------------------------------------------------------------------------
;; ---- Copyright notice updating ------------------------------------------
;; -------------------------------------------------------------------------

; Prompt me about updating copyright notices.

(defun fp-copyright-update () 
  (and (not (eq major-mode 'fundamental-mode))
       (copyright-update)))

(and (fboundp 'copyright-update)
     (add-hook 'write-file-hooks 'fp-copyright-update))

;; -------------------------------------------------------------------------
;; ---- set up paperless mode ----------------------------------------------
;; -------------------------------------------------------------------------

(require 'paperless)
(require 'org-paperless)

(setq *paperless-capture-dir* "/home/green/TOL/CAPTURE")
(setq *paperless-root-dir* "/home/green/TOL")

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

(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'load-path "~/quicklisp/dists/quicklisp/software/slime-v2.18")
(add-to-list 'load-path "~/quicklisp/dists/quicklisp/software/slime-v2.18/contrib")
(require 'slime)
(slime-setup '(slime-asdf))

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

; Kill some noise by hiding certain messages...
(setq erc-hide-list '("JOIN" "PART" "QUIT" "MODE" "NICK"))

; ...but don't silence the #gcc channel.
(add-hook 'erc-join-hook
	  (lambda ()
	    (if (equal "#gcc" (buffer-name))
		(set (make-local-variable 'erc-hide-list) '()))))

;; -------------------------------------------------------------------------
;; ---- OrgMode Tweaks -----------------------------------------------------
;; -------------------------------------------------------------------------

(require 'remember)
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)


(setq org-default-notes-file "/home/green/Dropbox/org/notes.org")
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map (kbd "<f9>") 'gnorb-org-contact-link)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "/home/green/Dropbox/org/notes.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
	("m" "Meeting Notes" entry
	 (file+datetree "/home/green/TOL/Work/Red_Hat/journal.org")
	 "* Meeting Notes %^g %?
Added: %U
Attendees:  ")))

; Org tables rock!  Use them in regular text files.

(add-hook 'text-mode-hook 'turn-on-orgtbl)


;; -------------------------------------------------------------------------
;; ---- Emacs hacking ------------------------------------------------------
;; -------------------------------------------------------------------------

(eval-after-load 'flycheck
  '(flycheck-package-setup))

;; -------------------------------------------------------------------------
;; ---- User Interface and Miscelleneous Editing Tweaks --------------------
;; -------------------------------------------------------------------------

      
;; Magit rules!
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-c o") 
                (lambda () (interactive) (find-file "~/Dropbox/org/notes.org")))
(global-set-key (kbd "C-c j") 
                (lambda () (interactive) (find-file "~/TOL/Work/Red_Hat/journal.org")))

(require 'powerline)

(defun switch-full-screen ()
  (interactive)
  (shell-command "wmctrl -r :ACTIVE: -btoggle,maximized_vert,maximized_horz"))

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

; Set the editing mode for special files.
(setq auto-mode-alist (append '(("README" . text-mode)
                                ("^/etc/rc" . ksh-mode))
                              auto-mode-alist))

; Auto fill and spell when in text mode.
(setq ispell-program-name "aspell")
(setq ispell-dictionary "british")
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'flyspell-mode)

(add-hook 'after-init-hook 'global-company-mode)
(require 'company-statistics)

;; Makefile mode settings.
(add-hook 'makefile-mode-hook (lambda ()
                                (font-lock-mode 1)
                                (auto-fill-mode 1)
                                (make-local-variable 'fill-paragraph-function)
                                (setq fill-paragraph-function
                                      'makefile-fill-paragraph)))

;; Only one space between text and backslash.
(setq makefile-backslash-column 0)
(setq makefile-backslash-align nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elfeed-feeds
   (quote
    ("http://tromey.com/blog/?feed=rss2" "https://www.ansible.com/blog/rss.xml" "http://moxielogic.github.io/blog/feeds/all.atom.xml" "http://planet.lisp.org/rss20.xml" "http://planet.emacsen.org/atom.xml" "https://developers.redhat.com/blog/feed/" "http://blog.quicklisp.org" "http://moxielogic.github.io/blog/feeds/all.atom.xml" "http://moxielogic.org/blog")))
 '(org-agenda-files (quote ("/home/green/Dropbox/org/notes.org")))
 '(package-selected-packages
   (quote
    (elfeed elfeed-goodies elfeed-org elfeed-web simple-mpc org-link-travis travis git-timemachine use-package magithub paperless company company-statistics org url magit jimb-patch erc)))
 '(paperless-capture-directory "/home/green/TOL/CAPTURE")
 '(paperless-root-directory "/home/green/TOL"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")


(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Sensitive"
  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-mode)
      (progn
	;; disable backups
	(set (make-local-variable 'backup-inhibited) t)	
	;; disable auto-save
	(if auto-save-default
	    (auto-save-mode -1)))
    ;resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;resort to default auto save setting
    (if auto-save-default
	(auto-save-mode 1))))

(setq auto-mode-alist
      (append '(("\\.gpg$" . sensitive-mode))
	      auto-mode-alist))


(require 'org-link-travis)
(setq org-link-travis/user-name "atgreen")


;;; org bulk action to schedule a task for today
(defun org-agenda-reschedule-to-today ()
  (interactive)
  (cl-letf
      (((symbol-function 'org-read-date)
	(lambda (&rest rest) (current-time))))
    (call-interactively 'org-agenda-schedule)))
;;; bind that to 'T' in the bulk action menu
(setq-default org-agenda-bulk-custom-functions
	      '((84 org-agenda-reschedule-to-today)))


