#-*- mode:makefile-gmake; -*-
AFDOG_ASD ?= $(ROOT)/afdog.asd
DEPS_PREFIX ?= afdog-$(shell (md5sum $(AFDOG_ASD) || md5 -r $(AFDOG_ASD)) | awk '{ print $$1; }')
DEPS_EXT ?= .core
DEPS_NAME ?= $(DEPS_PREFIX)$(DEPS_EXT)
DEPS_DIR  ?= /tmp

test: clean-all init afdog
	@echo "=> Running tests."
	rm -rf $(ROOT)/afdog-test.core
	$(MAKE) run-tests-with-core
	$(MAKE) clean

$(ROOT)/afdog-test.core: $(DEPS_DIR)/$(DEPS_NAME)
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
	$(MAKE) kill-everything-test
	rm -rf $(ROOT)/afdog-test.core

.PHONY: kill-everything-test
kill-everything-test: $(ROOT)/afdog-test.core
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(ROOT)/vendor/libfixposix/build/lib \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(ROOT)/afdog-test.core --eval '(afdog:kill-everything)' \
				--eval '(quit)'

clean-cores:
	@echo "=> Cleaning up old cores."
	rm -rf $(DEPS_DIR)/afdog-*$(DEPS_EXT)

$(DEPS_DIR)/$(DEPS_NAME):
	@echo "=> Compiling dependince core: $(DEPS_DIR)/$(DEPS_NAME)"
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(ROOT)/vendor/libfixposix/build/lib \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(LISP) --noinform --disable-ldb --lose-on-corruption --end-runtime-options \
		--disable-debugger \
		--noprint \
		--load $(QL_ROOT_PATH)/setup.lisp \
		--eval "(setf *compile-verbose* nil *compile-print* nil)" \
		--eval '(asdf:disable-output-translations)' \
		--eval "(mapc #'ql:quickload (cdar (asdf:component-depends-on 'asdf:compile-op (asdf:find-system :afdog))))" \
		--eval "(mapc #'ql:quickload '(#:nst))" \
		--eval '(sb-ext:save-lisp-and-die "$(DEPS_DIR)/$(DEPS_NAME)" :executable t)'

run-tests:
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(ROOT)/vendor/libfixposix/build/lib \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(LISP) --load $(QL_ROOT_PATH)/setup.lisp \
		--eval '(asdf:test-system :afdog)' \
		--eval '(quit)'
