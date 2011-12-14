#-*- mode:makefile-gmake; -*-

test: clean-all init
	@echo "=> Running tests."
	rm -rf $(ROOT)/afdog-test.core
	$(MAKE) run-tests-with-core
	$(MAKE) clean

$(ROOT)/afdog-test.core: afdog
	echo "=> Compiling test core."
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(ROOT)/vendor/libfixposix/build/lib \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(DEPS_DIR)/$(DEPS_NAME) \
		--noprint \
		--eval "(setf *compile-verbose* nil *compile-print* nil *load-verbose* nil *load-print* nil)" \
		--eval '(asdf:disable-output-translations)' \
		--eval "(asdf:load-system :afdog-tests)" \
		--load $(ROOT)/tests/make/helpers.lisp \
		--eval '(sb-ext:save-lisp-and-die "afdog-test.core" :executable t)'

run-tests-with-core: $(ROOT)/afdog-test.core
	@echo "=> Making sure we don't use a stale core"
	killall -9 afdog-test.core || echo "No running test cores found."
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(ROOT)/vendor/libfixposix/build/lib \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(ROOT)/afdog-test.core --eval '(afdog-tests:run-all)' \
							--eval '(quit)'
	$(MAKE) kill-everything-fast
	rm -rf $(ROOT)/afdog-test.core

clean-cores:
	@echo "=> Cleaning up old cores."
	rm -rf $(DEPS_DIR)/afdog-*$(DEPS_EXT)

run-tests:
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(ROOT)/vendor/libfixposix/build/lib \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(LISP) --load $(QL_ROOT_PATH)/setup.lisp \
		--eval '(asdf:test-system :afdog)' \
		--eval '(quit)'
