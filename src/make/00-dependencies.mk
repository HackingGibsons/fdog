# Dependency targets

CORE = $(ROOT)/fdog.core

$(CORE):
	LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:$(ROOT)/vendor/libfixposix/src/lib/.libs \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(LISP) --eval '(ql:quickload :fdog)' \
			--eval '(sb-ext:gc :full t)' \
		    --eval '(save-lisp-and-die #p"fdog.core")';

core-clean:
	rm -f $(CORE)

QL_TEST ?= $(LISP) --eval '(quit :unix-status (if (find-package :ql) 0 1))'
QL_URL ?= "https://github.com/quicklisp/quicklisp-bootstrap/raw/master/quicklisp.lisp"
quicklisp: sanity-check
	$(QL_TEST) || {\
	  echo "=> QL is missing. Installing"; \
	  curl -L $(QL_URL) > /tmp/quicklisp.lisp; \
	  $(LISP) --eval '(sb-ext:disable-debugger)' --load /tmp/quicklisp.lisp \
	    --eval '(quicklisp-quickstart:install :path "$(QL_ROOT_PATH)/")' \
	    --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
	    --eval '(quit)'; \
	}

configured-asdf: configure-asdf-registry $(FDOG_ASDF_CONF_NAME) $(VENDOR_ASDF_CONF_NAME)

configure-asdf-registry:
	@echo "=> Configuring your local lisp to find me."
	@mkdir -p $(REGISTRYD)

$(FDOG_ASDF_CONF_NAME):
	@echo "=> Into: $(FDOG_ASDF_CONF_NAME)"
	@echo "==> Config: $(FDOG_ASDF_CONF)"
	echo "$(FDOG_ASDF_CONF)" > $(FDOG_ASDF_CONF_NAME)

$(VENDOR_ASDF_CONF_NAME):
	@echo "=> Into: $(VENDOR_ASDF_CONF_NAME)"
	@echo "==> Config: $(VENDOR_ASDF_CONF)"
	echo "$(VENDOR_ASDF_CONF)" > $(VENDOR_ASDF_CONF_NAME)
