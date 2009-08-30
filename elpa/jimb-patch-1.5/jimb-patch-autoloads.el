;;; jimb-patch-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (jp-insert-buffer jp-insert-file jp-clean-patch
;;;;;;  jp-remove-fake-darcs-parents jp-extract-ChangeLog-entries
;;;;;;  jp-remove-generated-files) "jimb-patch" "jimb-patch.el" (19098
;;;;;;  39030))
;;; Generated autoloads from jimb-patch.el

(autoload 'jp-remove-generated-files "jimb-patch" "\
Delete all patch hunks referring to generated files from the current buffer.
What files are 'generated' depends on what other files are
present in the patch.  For example, if the patch affects a
'configure.in' file, then a 'configure' script in the same
directory is considered to be generated.

\(fn)" t nil)

(autoload 'jp-extract-ChangeLog-entries "jimb-patch" "\
Turn ChangeLog patch entries into ChangeLog entries at the top of the file.

\(fn)" t nil)

(autoload 'jp-remove-fake-darcs-parents "jimb-patch" "\
Remove bogus 'old-foo' and 'new-foo' parent dirs added by DARCS.
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

\(fn)" t nil)

(autoload 'jp-clean-patch "jimb-patch" "\
Clean up the patch between mark and point for posting.

If the patch includes diffs to ChangeLog files, this command
turns these into plain text ChangeLog entries, and moves them to
the top, indicating which ChangeLog file they belong to.

If the patch includes changes to generated files (configure
scripts, for example), and also includes changes to the files
they are generated from (say, configure.ac in the same
directory), then this command omits the generated files' patches.

\(fn START END)" t nil)

(autoload 'jp-insert-file "jimb-patch" "\
Insert FILE and clean it using `jp-clean-patch'.

\(fn FILE)" t nil)

(autoload 'jp-insert-buffer "jimb-patch" "\
Insert BUFFER and clean it using `jp-clean-patch'.

\(fn BUFFER)" t nil)

;;;***

;;;### (autoloads nil nil ("jimb-patch-pkg.el") (19098 39030 544272))

;;;***

(provide 'jimb-patch-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; jimb-patch-autoloads.el ends here
