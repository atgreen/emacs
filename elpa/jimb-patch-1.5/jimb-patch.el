;;; jimb-patch.el --- clean a patch for submission via email

;; Copyright (C) 2007 Jim Blandy
;; Contributors: Tom Tromey (jp-insert-file, jp-insert-buffer)

;; Author: Jim Blandy
;; Author: Tom Tromey
;; Version: 1.5
;; Keywords: tools

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;; Iterating over files in a patch.

(defun jp-for-each-patched-file (func)
  "Call FUNC on every file in the patch in the current buffer.
FUNC is given three arguments:
- FILE is the name of the file being patched,
- START is the buffer position of the file's start, and
- END is the buffer position of the file's end.
Point is at the top of the file when FUNC is called.
The match data is modified before each call.
FUNC may modify the file's patch, or delete it entirely."
  (goto-char (point-min))
  ;; I forget who produces '=== ' lines.
  ;; Subversion and CVS produce 'Index: ' lines.
  ;; Mercurial produces 'diff -r HEX' header lines, to indicate the parent
  ;; to which the diff is relative.  We match the hex, too, since '-r' is
  ;; a legitimate flag for plain diff, which takes no arguments.
  (if (re-search-forward "^\\(Index:\\|diff\\( -r [0-9a-f]+\\)?\\|===\\) \\(.*\\)$" nil t)
      (progn
        (forward-line 0)
        (let (file
              start
              (end (make-marker))
              (next-file (match-string 3)))
          (while
              (progn
                (setq file next-file)
                (setq start (point))

                ;; Find the end of this file's patch --- and possibly
                ;; the start of next file's patch.
                (save-excursion
                  ;; Skip CVS/SVN/SVK mumbling
                  (mapc (lambda (r) (if (looking-at r) (forward-line 1)))
                        '("^Index: " "^=== " "^=*$" "^RCS file: "
                          "^retrieving " "^diff "))
                  (if (re-search-forward "^\\(Index:\\|diff\\|===\\) \\(.*\\)$"
                                         nil 'to-end)
                      (progn
                        (setq next-file (match-string 2))
                        (forward-line 0))
                    (setq next-file nil))
                  (set-marker end (point)))

                (funcall func file start end)

                (goto-char end)
                next-file))))))


;;;; Deleting patches to automatically generated files.

;;;###autoload
(defun jp-remove-generated-files ()
  "Delete all patch hunks referring to generated files from the current buffer.
What files are 'generated' depends on what other files are
present in the patch.  For example, if the patch affects a
'configure.in' file, then a 'configure' script in the same
directory is considered to be generated."
  (interactive)
  (save-excursion
    (let ((gen (make-hash-table :test 'equal)))
      (goto-char (point-min))
      (jp-for-each-patched-file
          (lambda (file start end)
            (let ((file (expand-file-name file)))
              (if (jp-always-generated-p file)
                  (puthash file t gen)
                (let ((derived (jp-generates-what file)))
                  (if derived
                      (mapc (lambda (f) (puthash f t gen)) derived)))))))

      ;; Now go through and delete all patches for generated files.
      (goto-char (point-min))
      (jp-for-each-patched-file
          (lambda (file start end)
            (let ((file (expand-file-name file)))
              (if (gethash file gen) (delete-region start end))))))))

(defun jp-always-generated-p (file)
  "Return true if files named FILE should always be considered generated,
regardless of what other files are in the patch."
  (let ((basename (file-name-nondirectory file)))
    (or (string-match "\\(SRC-\\)?POTFILES.in" basename)
        (string-match "\\.pot$" basename))))

(defun jp-generates-what (original)
  "Return a list of files whose contents would be derived from ORIGINAL.
The returned filenames are speculative: the caller should take
care of checking whether they actually exist in the patch.  If
ORIGINAL has leading directory components, the returned filenames
will, too."
  (let ((base (file-name-nondirectory original))
        (dir (file-name-directory original)))
    (or (jp-automake-generates base dir)
        (jp-autoconf-generates base dir)
        (jp-cgen-generates base dir)
        (jp-gdbarch-sh-generates base dir))))

(defun jp-automake-generates (base dir)
  (if (string-equal base "Makefile.am")
      (list (expand-file-name "Makefile.in" dir)
            (expand-file-name "Makefile" dir)
            (expand-file-name "aclocal.m4" dir))))

(defun jp-autoconf-generates (base dir)
  (if (or (string-equal base "configure.in")
          (string-equal base "configure.ac"))
      (list (expand-file-name "configure" dir))))

(defun jp-cgen-generates (base dir)
  (if (string-match "^\\(.*\\).cpu$" base)
      (let ((cpu (match-string 1 base)))
        ;; A cpu file could appear in cpu/ or cgen/cpu.  Figure out the
        ;; corresponding top directory in either case.
        (let* ((top (if (and dir
                             (string-match "^\\(\\|.*/\\)\\(cgen/\\)?cpu/$" 
                                           dir))
                        (match-string 1 dir)))
               (opcodes (expand-file-name "opcodes" top)))
          (mapcar (lambda (suff)
                    (expand-file-name (concat cpu suff) opcodes))
                  '("-asm.c" "-desc.c" "-desc.h" "-dis.c" "-ibld.c"
                    "-opc.c" "-opc.h"))))))

;;; The 'gdbarch.sh' script in GDB generates a header and C file.
(defun jp-gdbarch-sh-generates (base dir)
  (if (string-equal base "gdbarch.sh")
      (list (expand-file-name "gdbarch.c" dir)
            (expand-file-name "gdbarch.h" dir))))



;;;; Turning ChangeLog patches into top-of-patch ChangeLog entries.

(defun jp-ChangeLog-patch-to-entry ()
  "Given a patch adding a ChangeLog entry, edit it into a stand-alone entry.
The patch should occupy the whole visible part of the current buffer."
  (save-excursion 

    ;; Extract the ChangeLog's filename.
    (goto-char (point-min))
    (cond
     ((looking-at "^\\(Index:\\|===\\) \\(.*\\)\n")
      (let ((file (match-string 2)))
        (delete-region (match-beginning 0) (match-end 0))
        (insert file ":\n")))
     ((looking-at "^diff .* \\(\\S-*\\)\n")
      (let ((file (match-string 1)))
        (delete-region (match-beginning 0) (match-end 0))
        (insert file ":\n"))))

    ;; Delete the patch preamble, up to the start of the hunk itself.
    (let ((preamble-start (point)))
      (cond
       ((re-search-forward "^\\*+\n" nil t)
        (if (looking-at "^\*\*\*.*\n---.*\n")
            (goto-char (match-end 0))))
       ((re-search-forward "^@@.*@@\\( .*\\)?\n" nil t))
       (t
        (error "couldn't find start of patch hunk")))
      (delete-region preamble-start (point))

      ;; Make sure the buffer only contains the one hunk.
      (if (re-search-forward "^--- .*\n\\|^@@.*@@\n" nil t)
          (error "patch for ChangeLog contains more than one hunk"))

      ;; Clean up the ChangeLog entry header. 
      (or (looking-at "^\\([-+! ] \\|[-+ ]\\)[0-9].*$")
          (error "Couldn't find ChangeLog date header"))
      (delete-region (match-beginning 1) (match-end 1))
      (forward-line 1)

      ;; Make sure there is at least one blank line after the entry
      ;; header.  We'll clean up multiple blank lines later.
      (insert "\n")
        
      ;; Keep lines with + markers; replace anything else with a blank
      ;; line.
      (while (< (point) (point-max))
        (if (looking-at "^\\+ ?")
            (progn
              (delete-region (match-beginning 0) (match-end 0))
              (forward-line 1))
          (let ((start (point)))
            (end-of-line)
            (delete-region start (point))
            (forward-line 1))))

      ;; Clean up any trailing whitespace.
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+$" nil t)
        (delete-region (match-beginning 0) (match-end 0)))

      ;; Replace sequences of more than one blank line with a single
      ;; blank line.
      (goto-char (point-min))
      (while (re-search-forward "^\n\\(\n+\\)" nil t)
        (delete-region (match-beginning 1) (match-end 1)))

      ;; Remove any blank lines at the end.
      (goto-char (point-max))
      (skip-syntax-backward "-")
      (delete-region (point) (point-max))
      (insert "\n"))))

;;;###autoload
(defun jp-extract-ChangeLog-entries ()
  "Turn ChangeLog patch entries into ChangeLog entries at the top of the file."
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (let (entries)
      (jp-for-each-patched-file 
          (lambda (name start end)
            (if (string-match "^ChangeLog"
                              (file-name-nondirectory name))
                (save-restriction
                  (narrow-to-region start end)
                  (jp-ChangeLog-patch-to-entry)
                  (push (buffer-string) entries)
                  (delete-region (point-min) (point-max))))))

      ;; Insert the entries at the top of the buffer, in the same
      ;; order they appeared in the buffer, separated by newlines.
      (goto-char (point-min))
      (mapc (lambda (e) (insert e "\n")) (reverse entries))
      
      ;; That list can be big; don't return it, lest it be printed.
      t)))

;;;###autoload
(defun jp-remove-fake-darcs-parents ()
  "Remove bogus 'old-foo' and 'new-foo' parent dirs added by DARCS.
The 'darcs patch' command generates patches with file headers that look
like this:

  diff -rN -u old-src/gdb/defs.h new-src/gdb/defs.h
  --- old-src/gdb/defs.h	2006-11-01 17:15:51.000000000 -0800
  +++ new-src/gdb/defs.h	2006-11-01 17:15:51.000000000 -0800

This means that the patches can't be applied with -p0, which is
The Right Thing.  This function trims those file names from the
patches, leaving file headers that look like this:

  diff -rN -u old-src/gdb/defs.h new-src/gdb/defs.h
  --- gdb/defs.h	2006-11-01 17:15:51.000000000 -0800
  +++ gdb/defs.h	2006-11-01 17:15:51.000000000 -0800
"
  (interactive "*")
  (jp-for-each-patched-file
   (lambda (name start end)
     (goto-char start)
     (if (looking-at "^diff \\(.*\\)\n--- old-\\([^/]*\\)/\\([^\t]*\\)\t\\(.*\\)\n\\+\\+\\+ new-\\2/\\3\t\\(.*\\)\n")
         (replace-match "diff \\1\n--- \\3\t\\4\n+++ \\3\t\\5\n")))))

(defun jp-remove-CVS-unknown-file-lines ()
  "Remove lines produced by CVS indicating unknown files.
These are lines of the form '? FILENAME'."
  (goto-char (point-min))
  (while (re-search-forward "^\\? .*\n" nil t)
    (delete-region (match-beginning 0) (match-end 0))))

;;;###autoload
(defun jp-clean-patch (start end)
  "Clean up the patch between mark and point for posting.

If the patch includes diffs to ChangeLog files, this command
turns these into plain text ChangeLog entries, and moves them to
the top, indicating which ChangeLog file they belong to.

If the patch includes changes to generated files (configure
scripts, for example), and also includes changes to the files
they are generated from (say, configure.ac in the same
directory), then this command omits the generated files' patches."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (jp-remove-CVS-unknown-file-lines)
      (jp-remove-fake-darcs-parents)
      (jp-extract-ChangeLog-entries)
      (jp-remove-generated-files))))

;;;###autoload
(defun jp-insert-file (file)
  "Insert FILE and clean it using `jp-clean-patch'."
  (interactive "fPatch file to insert: ")
  (save-excursion
    (let ((start (point)))
      (forward-char (cadr (insert-file-contents file)))
      (jp-clean-patch start (point)))))

;;;###autoload
(defun jp-insert-buffer (buffer)
  "Insert BUFFER and clean it using `jp-clean-patch'."
  (interactive "bPatch buffer to insert: ")
  (save-excursion
    (let ((start (point)))
      (insert-buffer-substring buffer)
      (jp-clean-patch start (point)))))

(provide 'jimb-patch)

;;; jimb-patch.el ends here
