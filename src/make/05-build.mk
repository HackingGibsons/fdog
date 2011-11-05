#-*- mode:makefile-gmake; -*-
BINDIR ?= $(ROOT)/bin
BIN ?= $(BINDIR)/$(TARGET)

## Clean ##
.PHONY: clean-build
clean-build: clean-buildapp clean-afdog

## Buildapp ##
BUILDAPP ?= $(BINDIR)/buildapp
.PHONY: buildapp
buildapp: init $(BUILDAPP)
$(BUILDAPP):
	@echo "=> Building buildapp to $(BUILDAPP)"
	$(LISP) --eval '(sb-ext:disable-debugger)' \
	        --eval '(ql:quickload :buildapp)' \
	        --eval '(buildapp:build-buildapp "$(BUILDAPP)")' \
	        --eval '(quit)'

.PHONY: clean-buildapp
clean-buildapp:
	rm -rf $(BUILDAPP)


## Binary ##
.PHONY: afdog
afdog: $(BIN)
$(BIN): $(BUILDAPP)
	@echo "=> Building $(TARGET) => $(BIN)"
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(ROOT)/vendor/libfixposix/build/lib/ \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(BUILDAPP) --output $(BIN) \
				--asdf-path $(ROOT) \
				--asdf-tree $(ROOT)/vendor \
				--eval '(sb-ext:disable-debugger)' \
				--require sb-aclrepl \
				--load $(QL_SETUP) \
				--eval '(ql:quickload :afdog)' \
				--eval "(swank-loader:init :load-contribs t)" \
				--dispatched-entry '/afdog-cli:main' \
	|| { \
	       echo '[ERROR] Build failed!'; \
           rm -f $(BIN); \
           exit 1; \
    }


.PHONY: clean-afdog
clean-afdog:
	rm -fr $(BIN)
