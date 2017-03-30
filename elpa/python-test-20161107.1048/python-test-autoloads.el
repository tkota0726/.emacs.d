;;; python-test-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "python-test" "python-test.el" (22741 61989
;;;;;;  0 0))
;;; Generated autoloads from python-test.el
(put 'python-test-command 'safe-local-variable (lambda (a) (and (stringp a) (or (not (boundp 'compilation-read-command)) compilation-read-command))))

(autoload 'python-test-track-pdb-prompt "python-test" "\
Change compilation to `python-inferior-mode' when a pdb prompt is detected.

This function is a hack that enables `inferior-python-mode' when
a pdb prompt is detected in `compilation-mode' buffers, and to
work is meant to be added to `compilation-filter-hook'.  To go
back to `compilation-mode' you need to call
\\[python-test-back-to-compilation].

\(fn)" nil nil)

(autoload 'python-test-project "python-test" "\
Execute python project test with BACKEND.

\(fn BACKEND)" t nil)

(autoload 'python-test-file "python-test" "\
Execute python file test with BACKEND.

\(fn BACKEND)" t nil)

(autoload 'python-test-class "python-test" "\
Execute python class test with BACKEND.

\(fn BACKEND)" t nil)

(autoload 'python-test-method "python-test" "\
Execute python method test with BACKEND.

\(fn BACKEND)" t nil)

(autoload 'python-test-function "python-test" "\
Execute python function test with BACKEND.

\(fn BACKEND)" t nil)

(autoload 'python-test "python-test" "\
Execute COMMAND with python test.

\(fn COMMAND)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; python-test-autoloads.el ends here
