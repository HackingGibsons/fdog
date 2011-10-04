test: clean-all init
	@echo "=> Running tests."
	$(MAKE) run-tests
	$(MAKE) clean

run-tests: $(CORE)
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(ROOT)/vendor/libfixposix/src/lib/.libs \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(LISP) --core $(CORE) \
	        --load $(QL_ROOT_PATH)/setup.lisp \
			--no-userinit \
	        --eval '(ql:quickload :fdog-tests)' \
	        --eval '(asdf:test-system :fdog)' \
	        --eval '(quit)'
