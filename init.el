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
;; ---- add-change-log configuration ---------------------------------------
;; -------------------------------------------------------------------------

(setq add-log-full-name "Anthony Green"
      add-log-mailing-address "green@moxielogic.com"
      add-log-keep-changes-together t
      )

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

