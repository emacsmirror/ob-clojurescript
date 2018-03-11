.PHONY: test

test:
	@rm -f .test-org-id-locations
	emacs -Q --batch -q \
		-L . \
		-l ob-cljs.el \
		-l test-ob-cljs.el \
		--eval "(progn \
	              (setq org-confirm-babel-evaluate nil) \
	              (org-babel-do-load-languages \
	                'org-babel-load-languages '((emacs-lisp . t) \
	                                            (sh . t) \
	                                            (org . t) \
	                                            (cljs . t))))" \
	    -f ob-cljs-test-runall
