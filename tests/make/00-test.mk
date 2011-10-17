test: clean-all init
	@echo "=> Running tests."
	$(MAKE) run-tests
	$(MAKE) clean

test-old: clean-all init
	@echo "=> Running tests (no core)"
	$(MAKE) run-tests-old
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

run-tests-old:
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(ROOT)/vendor/libfixposix/src/lib/.libs \
		CPATH=$(ROOT)/vendor/libfixposix/src/include \
		$(LISP) --load $(QL_ROOT_PATH)/setup.lisp \
				--no-userinit \
		        --eval '(ql:quickload :fdog)' \
		        --eval '(ql:quickload :fdog-tests)' \
		        --eval '(asdf:test-system :fdog)' \
		        --eval '(quit)'
