all: fdog $(STAGEDIR) bundle

bundle: $(FDOG).bundle
$(FDOG).bundle: quicklisp $(STAGEDIR)
	@echo "=> Bundling up a distributable"
	tar cz -C $(STAGEDIR) -f /tmp/fdog.bundle.tgz .
	$(LISP) --noinform \
			--no-userinit \
			--no-sysinit \
			--eval "(sb-ext:disable-debugger)" \
			--load $(QL_ROOT_PATH)/setup.lisp \
			--eval "(ql:quickload :unix-options)" \
			--eval "(ql:quickload :flexi-streams)" \
			--eval "(ql:quickload :external-program)" \
			--eval "(ql:quickload :cl-ppcre)" \
			--load $(ROOT)/src/bundler.lisp \
			--eval '(read-data-from "/tmp/fdog.bundle.tgz")' \
			--eval "(sb-ext:save-lisp-and-die #p\"$(FDOG).bundle\" :executable t :save-runtime-options t :toplevel #'(lambda () (bundle sb-ext:*posix-argv*)))"
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
	@echo 'LD_LIBRARY_PATH="$$BASE/lib":$$LD_LIBRARY_PATH $$BASE/bin/fdog $$@' >> $(STAGEDIR)/fdog
	chmod +x $(STAGEDIR)/fdog

install: all
	@echo "=> Install is disabled while the build is restructured."
#	sudo install -B prev -b $(FDOG) $(DESTDIR)

fdog: init $(FDOG)
$(FDOG): $(CORE)
	@echo "=> Assuring subsystem build"
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(ROOT)/vendor/libfixposix/src/lib/.libs \
	CPATH=$(ROOT)/vendor/libfixposix/src/include:$(CPATH) \
	$(LISP) --core $(CORE) \
		    --eval '(sb-ext:disable-debugger)' \
			--eval "(require 'sb-aclrepl)" \
	        --eval '(declaim (optimize (debug $(DEBUG))))' \
	        --load $(QL_ROOT_PATH)/setup.lisp \
	        --eval '(quit)'
	@echo "=> Building fdog"
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(ROOT)/vendor/libfixposix/src/lib/.libs \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(LISP) --core $(CORE) \
			--noinform \
			--no-userinit \
			--no-sysinit \
			--eval "(require 'sb-aclrepl)" \
			--eval "(sb-ext:disable-debugger)" \
			--load $(ROOT)/src/builder.lisp \
			--eval "(declaim (optimize (debug $(DEBUG))))" \
			--load $(QL_ROOT_PATH)/setup.lisp \
			--eval "(swank-loader:init :load-contribs t)" \
			--eval "(sb-ext:save-lisp-and-die #p\"$(FDOG)\" :executable t :save-runtime-options t :toplevel #'(lambda () (fdog-cli:fdog-main sb-ext:*posix-argv*)))" \
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
	$(MAKE) clean-build

