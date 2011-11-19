test: clean-all init
	@echo "=> Running tests."
	rm -rf $(ROOT)/sbcl.core
	$(MAKE) run-tests-with-core
	$(MAKE) clean

$(ROOT)/sbcl.core:
	echo "=> Compiling test core."
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(ROOT)/vendor/libfixposix/build/lib \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(LISP)  --script $(ROOT)/bin/afdog.script \
	                  repl \
                      '(ql:quickload :afdog-tests)' \
                      '(sb-ext:save-lisp-and-die "sbcl.core")'

run-tests-with-core: $(ROOT)/sbcl.core
	@echo "=> Making sure we don't use a stale core"
	SBCL_HOME=$(ROOT) $(MAKE) run-tests
	rm -rf $(ROOT)/sbcl.core

run-tests:
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(ROOT)/vendor/libfixposix/build/lib \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(LISP) --load $(QL_ROOT_PATH)/setup.lisp \
			--no-userinit \
			--eval '(ql:quickload :afdog-tests)' \
			--eval '(asdf:test-system :afdog)' \
			--eval '(quit)'
	rm -rf $(ROOT)/sbcl.core
