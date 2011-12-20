#-*- mode:makefile-gmake; -*-
BINDIR ?= $(ROOT)/bin
BIN ?= $(BINDIR)/$(TARGET)

AFDOG_ASD ?= $(ROOT)/$(TARGET).asd
DEPS_PREFIX ?= afdog-$(shell (md5sum $(AFDOG_ASD) || md5 -r $(AFDOG_ASD)) | awk '{ print $$1; }')
DEPS_EXT ?= .core
DEPS_NAME ?= $(DEPS_PREFIX)$(DEPS_EXT)
DEPS_DIR  ?= /tmp

## Developer setup ##
.PHONY: develop
develop: | init asdf
	@echo "=> You should be good to go."


## Buildapp ##
BUILDAPP ?= $(BINDIR)/buildapp
.PHONY: buildapp
buildapp: $(BUILDAPP)
$(BUILDAPP): | init
	@echo "=> Building buildapp to $(BUILDAPP)"
	$(LISP) --eval '(sb-ext:disable-debugger)' \
	        --eval '(ql:quickload :buildapp)' \
	        --eval '(buildapp:build-buildapp "$(BUILDAPP)")' \
	        --eval '(quit)'
## Clean
.PHONY: clean-buildapp
clean-buildapp:
	rm -rf $(BUILDAPP)


## Dependency core
.PHONY: dep-core
dep-core: $(DEPS_DIR)/$(DEPS_NAME)
$(DEPS_DIR)/$(DEPS_NAME):
	@echo "=> Compiling dependince core: $(DEPS_DIR)/$(DEPS_NAME)"
	$(LISP) --noinform --disable-ldb --lose-on-corruption --end-runtime-options \
		--disable-debugger \
		--noprint \
		--load $(QL_ROOT_PATH)/setup.lisp \
		--eval "(setf *compile-verbose* nil *compile-print* nil)" \
		--eval '(asdf:disable-output-translations)' \
		--eval "(mapc #'ql:quickload (cdar (asdf:component-depends-on 'asdf:compile-op (asdf:find-system :afdog))))" \
                --eval "(mapc #'ql:quickload (remove :afdog (cdar (asdf:component-depends-on 'asdf:compile-op (asdf:find-system :afdog-tests))) :test #'string-equal))" \
		--eval '(sb-ext:save-lisp-and-die "$(DEPS_DIR)/$(DEPS_NAME)" :executable t)'

## Binary ##
.PHONY: afdog
afdog: $(BIN)
$(BIN): $(BUILDAPP) dep-core
	@echo "=> Building $(TARGET) => $(BIN)"
	$(LISP_PREFIX) $(BUILDAPP) --output $(BIN) \
				               --asdf-path $(ROOT) \
				               --asdf-tree $(ROOT)/vendor \
                               --load $(ROOT)/src/patches/build.lisp \
				               --require sb-aclrepl \
				               --eval '(asdf:load-system :afdog)' \
                               --eval "(setf afdog:*git-revision* \"$$(git rev-parse HEAD || echo UNKNOWN)\")" \
				               --eval "(swank-loader:init :load-contribs t)" \
				               --dispatched-entry '/afdog-cli:main' \
				--sbcl $(DEPS_DIR)/$(DEPS_NAME) \
	|| { \
	       echo '[ERROR] Build failed!'; \
           rm -f $(BIN); \
           exit 1; \
    }
## Clean
.PHONY: clean-afdog
clean-afdog:
	rm -fr $(BIN)


## Generals ##
.PHONY: clean-build all
all: | afdog
clean-build: | clean-buildapp clean-afdog
