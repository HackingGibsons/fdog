#-*- mode:makefile-gmake; -*-
AFDOG_ASD ?= $(ROOT)/afdog.asd
DEPS_PREFIX ?= afdog-$(shell (md5sum $(AFDOG_ASD) || md5 -r $(AFDOG_ASD)) | awk '{ print $$1; }')
DEPS_EXT ?= .core
DEPS_NAME ?= $(DEPS_PREFIX)$(DEPS_EXT)
DEPS_DIR  ?= /tmp

test: clean-all init
	@echo "=> Running tests."
	rm -rf $(ROOT)/sbcl.core
	$(MAKE) run-tests-with-core
	$(MAKE) clean

$(ROOT)/sbcl.core: $(DEPS_DIR)/$(DEPS_NAME)
	echo "=> Compiling test core."
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(ROOT)/vendor/libfixposix/build/lib \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(DEPS_DIR)/$(DEPS_NAME) \
		--noprint \
		--eval '(defparameter *afdog-system* (asdf:find-system :afdog))' \
		--eval '(defparameter *afdog-test-system* (asdf:find-system :afdog-tests))' \
		--eval '(setf (ASDF::COMPONENT-LOAD-DEPENDENCIES *afdog-system*) nil)' \
		--eval '(setf (ASDF::COMPONENT-LOAD-DEPENDENCIES *afdog-test-system*) nil)' \
		--eval '(asdf:disable-output-translations)' \
		--eval "(asdf:load-system *afdog-system*)" \
		--eval "(asdf:load-system *afdog-test-system*)" \
		--eval '(sb-ext:save-lisp-and-die "sbcl.core")'

run-tests-with-core: $(ROOT)/sbcl.core
	@echo "=> Making sure we don't use a stale core"
	SBCL_HOME=$(ROOT) $(MAKE) run-tests
	rm -rf $(ROOT)/sbcl.core

$(DEPS_DIR)/$(DEPS_NAME):
	@echo "=> Cleaning up old cores."
	rm -rf $(DEPS_DIR)/afdog-*$(DEPS_EXT)
	@echo "=> Compiling dependince core: $(DEPS_DIR)/$(DEPS_NAME)"
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(ROOT)/vendor/libfixposix/build/lib \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(LISP) --noinform --disable-ldb --lose-on-corruption --end-runtime-options \
		--disable-debugger \
		--noprint \
		--load $(QL_ROOT_PATH)/setup.lisp \
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
	rm -rf $(ROOT)/sbcl.core
