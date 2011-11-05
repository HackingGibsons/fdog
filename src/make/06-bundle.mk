#-*- mode:makefile-gmake; -*-
STAGEDIR ?= $(ROOT)/build

$(STAGEDIR): $(BIN)
	@echo "=> Preparing release in $(STAGEDIR)"
	mkdir -p $(STAGEDIR)/bin
	mkdir -p $(STAGEDIR)/lib
	cp $(BIN) $(STAGEDIR)/bin

	@echo "=> Archiving revision"
	echo `git rev-parse HEAD` > $(STAGEDIR)/REV

	@echo "=> Copying native libs"
	cp vendor/libfixposix/build/lib/*.* $(STAGEDIR)/lib

	@echo "=> Staging select FFI build libs"
	find vendor/clsql -name '*.dylib' -or -name '*.so' | xargs -I % cp % $(STAGEDIR)/lib

	@echo "=> Building bootstrap script: $(STAGEDIR)/$(TARGET)"
	touch $(STAGEDIR)/$(TARGET)
	@echo '#!/bin/sh' > $(STAGEDIR)/$(TARGET)
	@echo 'BASE=$$(dirname $$(greadlink -f $$0 2> /dev/null || readlink -f $$0 2> /dev/null))' >> $(STAGEDIR)/$(TARGET)
	@echo 'export AFDOG="$$BASE/$$0"' >> $(STAGEDIR)/$(TARGET)
	@echo 'LD_LIBRARY_PATH="$$BASE/lib":$$LD_LIBRARY_PATH $$BASE/bin/'$(TARGET) '$$@' >> $(STAGEDIR)/$(TARGET)
	chmod +x $(STAGEDIR)/$(TARGET)

.PHONY: clean-stagedir
clean-stagedir:
	rm -rf $(STAGEDIR)
