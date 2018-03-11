;;; test-ob-cljs.el
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:
(require 'ert)
(require 'org-id)

(defconst ob-cljs-test-dir
  (expand-file-name (file-name-directory (or load-file-name buffer-file-name))))

(defconst org-id-locations-file
  (expand-file-name ".test-org-id-locations" ob-cljs-test-dir))

(defun ob-cljs-test-update-id-locations ()
  (let ((files (directory-files
                ob-cljs-test-dir 'full
                "^\\([^.]\\|\\.\\([^.]\\|\\..\\)\\).*\\.org$")))
    (org-id-update-id-locations files)))

(defmacro org-test-at-id (id &rest body)
  "Run body after placing the point in the headline identified by ID."
  (declare (indent 1))
  `(let* ((id-location (org-id-find ,id))
	  (id-file (car id-location))
	  (visited-p (get-file-buffer id-file))
	  to-be-removed)
     (unwind-protect
				 (save-window-excursion
					 (save-match-data
						 (org-id-goto ,id)
						 (setq to-be-removed (current-buffer))
						 (condition-case nil
								 (progn
									 (org-show-subtree)
									 (org-show-block-all))
							 (error nil))
						 (save-restriction ,@body)))
       (unless (or visited-p (not to-be-removed))
				 (kill-buffer to-be-removed)))))
(def-edebug-spec org-test-at-id (form body))

(unless (featurep 'ob-cljs)
  (signal 'missing-test-dependency "Support for ClojureScript code blocks"))

(ert-deftest ob-cljs/assert ()
  (should t))

(ert-deftest ob-cljs/simple ()
  "Simple output"
  (if (executable-find org-babel-cljs-command)
      (org-test-at-id "FABFDF69-1B5A-4193-8FA7-DA57125E3352"
		      (org-babel-next-src-block 1)
		      (should (equal '(1 4 9) (org-babel-execute-src-block))))))

(ert-deftest ob-cljs/native-js ()
	"JavaScript interop"
	(if (executable-find org-babel-cljs-command)
      (org-test-at-id "FABFDF69-1B5A-4193-8FA7-DA57125E3352"
		      (org-babel-next-src-block 2)
		      (should (string-equal "hello" (org-babel-execute-src-block))))))

(defun ob-cljs-test-runall ()
  (progn
    (ob-cljs-test-update-id-locations)
    (ert t)))

(provide 'ob-cljs-test)
