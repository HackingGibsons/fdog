all: fdog $(STAGEDIR) bundle

bundle: $(FDOG).bundle
$(FDOG).bundle: quicklisp $(STAGEDIR) $(BUILDAPP)
	@echo "=> Bundling up a distributable"
	tar cz -C $(STAGEDIR) -f /tmp/fdog.bundle.tgz .
	$(BUILDAPP) --output $(FDOG).bundle \
		--eval '(sb-ext:disable-debugger)' \
		--load $(QL_ROOT_PATH)/setup.lisp \
	  	--eval "(ql:quickload :unix-options)" \
	  	--eval "(ql:quickload :flexi-streams)" \
		--eval "(ql:quickload :external-program)" \
		--eval "(ql:quickload :cl-ppcre)" \
                --load $(ROOT)/src/bundler.lisp \
		--eval '(read-data-from "/tmp/fdog.bundle.tgz")' \
	        --entry bundle
	rm -rf /tmp/fdog.bundle.tgz

$(STAGEDIR): $(FODG)
	@echo "=> Preparing release in $(STAGEDIR)"
	mkdir -p $(STAGEDIR)/bin
	cp $(FDOG) $(STAGEDIR)/bin
	mkdir -p $(STAGEDIR)/lib

	@echo "=> Archiving revision"
	echo `git rev-parse HEAD` > $(STAGEDIR)/REV


	@echo "=> Copying native libs"
	cp vendor/libfixposix/build/lib/*.* $(STAGEDIR)/lib

	@echo "=> Staging select FFI build libs"
	find vendor/clsql -name '*.dylib' -or -name '*.so' | xargs -I % cp % $(STAGEDIR)/lib

	@echo "=> Building bootstrap script: $(STAGEDIR)/fdog"
	touch $(STAGEDIR)/fdog
	@echo '#!/bin/sh' > $(STAGEDIR)/fdog
	@echo 'BASE=$$(dirname $$(readlink -f $$0))' >> $(STAGEDIR)/fdog
	@echo 'LD_LIBRARY_PATH="$$BASE/lib" $$BASE/bin/fdog $$@' >> $(STAGEDIR)/fdog
	chmod +x $(STAGEDIR)/fdog

install: all
	@echo "=> Install is disabled while the build is restructured."
#	sudo install -B prev -b $(FDOG) $(DESTDIR)

fdog: init buildapp $(FDOG)
$(FDOG):
	@echo "=> Assuring subsystem build"
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(ROOT)/vendor/libfixposix/src/lib/.libs \
	CPATH=$(ROOT)/vendor/libfixposix/src/include:$(CPATH) \
	$(LISP) --eval '(sb-ext:disable-debugger)' \
		--eval "(require 'sb-aclrepl)" \
	        --eval '(declaim (optimize (debug $(DEBUG))))' \
	        --load $(QL_ROOT_PATH)/setup.lisp \
	        --eval '(ql:quickload :fdog)' \
	        --eval '(quit)'
	@echo "=> Building fdog"
	LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$(ROOT)/vendor/libfixposix/src/lib/.libs \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(BUILDAPP) --output $(FDOG) \
	            --asdf-path $(ROOT) \
	            --asdf-tree $(ROOT)/vendor \
                    --require sb-aclrepl \
		    --eval '(sb-ext:disable-debugger)' \
                    --load $(ROOT)/src/builder.lisp \
	            --eval '(declaim (optimize (debug $(DEBUG))))' \
	            --load $(QL_ROOT_PATH)/setup.lisp \
	            --eval '(ql:quickload :fdog)' \
	            --dispatched-entry '/fdog-cli:fdog-main' \
	|| { echo '[ERROR] Build failed!'; \
	     rm -f $(FDOG); exit 1; }

clean-build:
	@echo "=> Removing fdog builds"
	rm -rf $(FDOG)
	@echo "=> Removing the staged release"
	rm -rf $(STAGEDIR)

clean: externals-clean
	@echo "=> Clearing common-lisp cache"
	rm -rf ~/.cache/common-lisp/
	@echo "=> Cleaning up buildapp"
	rm -rf $(BUILDAPP)
	$(MAKE) clean-build


