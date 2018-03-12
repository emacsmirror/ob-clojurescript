;;; ob-clojurescript-test.el
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:
(require 'ert)
(require 'org-id)

(defconst ob-clojurescript-test-dir
  (expand-file-name (file-name-directory (or load-file-name buffer-file-name))))

(defconst org-id-locations-file
  (expand-file-name ".test-org-id-locations" ob-clojurescript-test-dir))

(defun ob-clojurescript-test-update-id-locations ()
  (let ((files (directory-files
                ob-clojurescript-test-dir 'full
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

(unless (featurep 'ob-clojurescript)
  (signal 'missing-test-dependency "Support for ClojureScript code blocks"))

(ert-deftest ob-clojurescript/assert ()
  (should t))

(ert-deftest ob-clojurescript/simple ()
  "Simple output"
  (if (executable-find org-babel-clojurescript-command)
      (org-test-at-id "FABFDF69-1B5A-4193-8FA7-DA57125E3352"
		      (org-babel-next-src-block 1)
		      (should (equal '(1 4 9) (org-babel-execute-src-block))))))

(ert-deftest ob-clojurescript/native-js ()
	"JavaScript interop"
	(if (executable-find org-babel-clojurescript-command)
      (org-test-at-id "FABFDF69-1B5A-4193-8FA7-DA57125E3352"
		      (org-babel-next-src-block 2)
		      (should (string-equal "hello" (org-babel-execute-src-block))))))

(ert-deftest ob-clojurescript/functions ()
	"Functions"
	(if (executable-find org-babel-clojurescript-command)
      (org-test-at-id "FABFDF69-1B5A-4193-8FA7-DA57125E3352"
		      (org-babel-next-src-block 3)
		      (should (string-equal "#'cljs.user/greet\nHello, Rich" (org-babel-execute-src-block))))))

(defun ob-clojurescript-test-runall ()
  (progn
    (ob-clojurescript-test-update-id-locations)
    (ert t)))

(provide 'ob-clojurescript-test)
