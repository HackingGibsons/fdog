run-tests-functional: start-test-fdog
	@echo "Running functional test suite."
	$(LISP) --no-userinit \
	        --load $(QL_ROOT_PATH)/setup.lisp \
	        --eval '(ql:quickload :fdog)' \
	        --eval '(ql:quickload :fdog-tests)' \
	        --eval '(progn (funcall (intern (symbol-name :start-logging) :fdog)) \
                           (funcall (intern (symbol-name :run-functional) :fdog-tests)))' \
	        --eval '(quit)'
	@echo "Terminating functional test scaffold."
	$(MAKE) stop-test-fdog
