test: clean init
	@echo "=> Running tests."
	$(MAKE) run-tests
	$(MAKE) clean

run-tests:
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(ROOT)/vendor/libfixposix/src/lib/.libs \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(LISP) --no-userinit \
	        --load $(QL_ROOT_PATH)/setup.lisp \
	        --eval '(ql:quickload :fdog)' \
	        --eval '(ql:quickload :fdog-tests)' \
	        --eval '(asdf:test-system :fdog)' \
	        --eval '(quit)'
