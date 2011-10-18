test: clean-all init
	@echo "=> Running tests."
	$(MAKE) run-tests
	$(MAKE) clean

run-tests:
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(ROOT)/vendor/libfixposix/src/libs/.libs \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(LISP) --load $(QL_ROOT_PATH)/setup.lisp \
			--no-userinit \
			--eval '(ql:quickload :afdog-tests)' \
			--eval '(asdf:test-system :afdog)' \
			--eval '(quit)'