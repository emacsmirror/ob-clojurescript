;;; ob-cljs.el --- org-babel functions for ClojureScript evaluation -*- lexical-binding: t; -*-

;; Author: Larry Staton Jr.
;; Maintainer: Larry Staton Jr.
;; Created: 10 March 2018
;; Keywords: literate programming, reproducible research
;; Homepage: https://gitlab.com/statonjr/ob-cljs

;;; Commentary:

;; Org-babel support for evaluating ClojureScript code.

;; Requirements:

;; - [[https://github.com/anmonteiro/lumo][lumo]]
;; - clojurescript-mode

;;; Code:
(require 'ob)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("cljs" . "cljs"))

(defvar org-babel-cljs-command "lumo"
  "The command to use to compile and run your ClojureScript code.")

(defvar org-babel-default-header-args:cljs '())
(defvar org-babel-header-args:cljs '((package . :any)))

(defun ob-cljs-escape-quotes (str-val)
	"Escape quotes for STR-VAL so that Lumo can understand"
	(replace-regexp-in-string "\"" "\\\"" str-val 'FIXEDCASE 'LITERAL))

(defun org-babel-expand-body:cljs (body params)
	"Expand BODY according to PARAMS, return the expanded body."
	(let* ((vars (org-babel--get-vars params))
				 (result-params (cdr (assq :result-params params)))
				 (print-level nil) (print-length nil)
				 (body (ob-cljs-escape-quotes
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

(defun org-babel-execute:cljs (body params)
  "Execute a block of ClojureScript code with Babel."
  (let ((expanded (org-babel-expand-body:cljs body params))
				result)
		(setq result
					(org-babel-trim
					 (shell-command-to-string
						(concat "/usr/local/bin/lumo -e \"" expanded "\""))))
    (org-babel-result-cond (cdr (assoc :result-params params))
			result
      (condition-case nil (org-babel-script-escape result)
				(error result)))))

(provide 'ob-cljs)
;;; ob-cljs.el ends here
