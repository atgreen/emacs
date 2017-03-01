;;; travis-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "travis" "travis.el" (22708 49962 589924 855000))
;;; Generated autoloads from travis.el

(autoload 'emacs-travis-version "travis" "\
Get the emacs-travis version as string.
If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.
The returned string includes both, the version from package.el
and the library version, if both a present and different.
If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

\(fn &optional SHOW-VERSION)" t nil)

;;;***

;;;### (autoloads nil nil ("travis-api.el" "travis-auth.el" "travis-builds.el"
;;;;;;  "travis-mode.el" "travis-pkg.el" "travis-repos.el" "travis-ui.el"
;;;;;;  "travis-users.el" "travis-utils.el" "travis-version.el")
;;;;;;  (22708 49962 599924 712000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; travis-autoloads.el ends here
