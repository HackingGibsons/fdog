run-tests-functional: start-test-fdog
	@echo "Running functional test suite."
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(ROOT)/vendor/libfixposix/src/lib/.libs \
	CPATH=$(ROOT)/vendor/libfixposix/src/include:$(CPATH) \
	$(LISP) --no-userinit \
	        --load $(QL_ROOT_PATH)/setup.lisp \
	        --eval '(ql:quickload :fdog)' \
	        --eval '(ql:quickload :fdog-tests)' \
	        --eval '(funcall (intern (symbol-name :start-logging) :fdog))' \
			--eval '(funcall (intern (symbol-name :run-functional) :fdog-tests))' \
	        --eval '(quit)'
	@echo "Terminating functional test scaffold."
	$(MAKE) stop-test-fdog
