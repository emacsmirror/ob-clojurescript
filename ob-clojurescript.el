;;; ob-clojurescript.el --- org-babel functions for ClojureScript evaluation -*- lexical-binding: t; -*-

;; Author: Larry Staton Jr.
;; Maintainer: Larry Staton Jr.
;; Created: 10 March 2018
;; Keywords: literate programming, reproducible research
;; Homepage: https://gitlab.com/statonjr/ob-clojurescript

;;; Commentary:

;; Org-babel support for evaluating ClojureScript code.

;; Requirements:

;; - [[https://github.com/anmonteiro/lumo][lumo]]
;; - clojurescript-mode

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("clojurescript" . "cljs"))

(defvar org-babel-clojurescript-command "lumo"
  "The command to use to compile and run your ClojureScript code.")

(defvar org-babel-default-header-args:clojurescript '())
(defvar org-babel-header-args:clojurescript '((package . :any)))

(defun ob-clojurescript-escape-quotes (str-val)
	"Escape quotes for STR-VAL so that Lumo can understand."
	(replace-regexp-in-string "\"" "\\\"" str-val 'FIXEDCASE 'LITERAL))

(defun org-babel-expand-body:clojurescript (body params)
	"Expand BODY according to PARAMS, return the expanded body."
	(let* ((vars (org-babel--get-vars params))
				 (result-params (cdr (assq :result-params params)))
				 (print-level nil) (print-length nil)
				 (body (ob-clojurescript-escape-quotes
								(org-trim
								 (if (null vars)
										 (org-trim body)
									 (concat "(let ["
													 (mapconcat
														(lambda (var)
															(format "%S (quote %S)" (car var) (cdr var)))
														vars "\n      ")
													 "]\n" body ")"))))))
		(if (or (member "code" result-params)
						(member "pp" result-params))
				(format "(print (do %s))" body)
			body)))

(defun org-babel-execute:clojurescript (body params)
  "Execute a block of ClojureScript code in BODY with Babel using PARAMS."
  (let ((expanded (org-babel-expand-body:clojurescript body params))
				result)
		(setq result
					(org-babel-trim
					 (shell-command-to-string
						(concat "/usr/local/bin/lumo -e \"" expanded "\""))))
    (org-babel-result-cond (cdr (assoc :result-params params))
			result
      (condition-case nil (org-babel-script-escape result)
				(error result)))))

(provide 'ob-clojurescript)
;;; ob-clojurescript.el ends here
