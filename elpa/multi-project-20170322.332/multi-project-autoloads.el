;;; multi-project-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "multi-project" "multi-project.el" (22764 60368
;;;;;;  0 0))
;;; Generated autoloads from multi-project.el

(autoload 'multi-project-compile "multi-project" "\
Compiles a project based upon the current directory of the buffer.

\(fn)" t nil)

(autoload 'multi-project-root "multi-project" "\
Jumps to the root of a project based upon current directory.

\(fn)" t nil)

(autoload 'multi-project-change-tags "multi-project" "\
Visits tags file based upon current directory. The optional
PROJECT argument will change tags to the specified PROJECT.

\(fn &optional PROJECT)" t nil)

(autoload 'multi-project-last "multi-project" "\
Jumps to the last chosen project.

\(fn)" t nil)

(autoload 'multi-project-anchor "multi-project" "\
Prevent the tracking of switching between projects and always
use the anchored project.

\(fn)" t nil)

(autoload 'multi-project-reset-anchor "multi-project" "\
Clears out the anchoring of a project.

\(fn)" t nil)

(autoload 'multi-project-display-change-tags "multi-project" "\


\(fn)" t nil)

(autoload 'multi-project-display-projects "multi-project" "\
Displays a buffer with the projects

\(fn)" t nil)

(autoload 'multi-project-find-file "multi-project" "\
Search a TAGS file for a particular file that matches a user's
input.

\(fn)" t nil)

(defadvice find-tag (before multi-project-find-tag (TAGNAME &optional NEXT-P REGEXP-P)) "\
Determine which TAGS file should be used based upon the current directory." (let ((project (multi-project-find-by-directory))) (when project (multi-project-change-tags (car project)))))

(autoload 'multi-project-current-project "multi-project" "\
Displays the current project in the minibuffer.

\(fn)" t nil)

(autoload 'multi-project-shell "multi-project" "\
Creates a shell with the buffer name of the project. The
 function first looks if the current directory is within a known
 project. If no projects are found, then the current project is
 used.

\(fn)" t nil)

(defvar global-multi-project-mode nil "\
Non-nil if Global Multi-Project mode is enabled.
See the `global-multi-project-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-multi-project-mode'.")

(custom-autoload 'global-multi-project-mode "multi-project" nil)

(autoload 'global-multi-project-mode "multi-project" "\
Toggle multi-project mode.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; multi-project-autoloads.el ends here
