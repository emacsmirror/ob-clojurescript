.PHONY: test

test:
	@rm -f .test-org-id-locations
	emacs -Q --batch -q \
		-L . \
		-l ob-clojurescript.el \
		-l ob-clojurescript-test.el \
		--eval "(progn \
	              (setq org-confirm-babel-evaluate nil) \
	              (org-babel-do-load-languages \
	                'org-babel-load-languages '((emacs-lisp . t) \
	                                            (sh . t) \
	                                            (org . t) \
	                                            (clojurescript . t))))" \
	    -f ob-clojurescript-test-runall
