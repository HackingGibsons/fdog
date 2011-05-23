ROOT ?= $(shell pwd)
LISP ?= $(shell which sbcl)
QL_URL ?= "https://github.com/quicklisp/quicklisp-bootstrap/raw/master/quicklisp.lisp"
REGISTRYD ?= $(HOME)/.config/common-lisp/source-registry.conf.d

FDOG_ASDF_CONF = (:directory \"$(ROOT)/\")
FDOG_ASDF_CONF_NAME = $(REGISTRYD)/"01-fdog.conf"

VENDOR_ASDF_CONF = (:tree \"$(ROOT)/vendor/\")
VENDOR_ASDF_CONF_NAME = $(REGISTRYD)/"02-fdog-vendor.conf"


# Inteded UI targets
init: submodules sanity-check configured-asdf

submodules:
	git submodule update --init --recursive

# WIP
quicklisp: sanity-check
	@echo "Lisp: " $(LISP)
	@echo "Quicklisp: " $(QL_URL)

# Dependency targets
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

# Utility targets
sanity-check: $(ROOT)/fdog.asd $(LISP)
	@echo "!> Environment looks sane. I'll allow this."

# Quick helper to build all ubuntu deps
SBCL_URL_BIN ?= "http://prdownloads.sourceforge.net/sbcl/sbcl-1.0.48-x86-64-linux-binary.tar.bz2"
ubuntu-req:
	@echo "=> Making sure we have aptitude"
	yes Y | sudo apt-get install aptitude
	@echo "=> Round one of dependancies"
	yes Y | sudo aptitude install curl build-essential git sqlite3 libsqlite3-dev
	@echo "=> Fetching/extracting/installing SBCL binary"
	mkdir -p /tmp/sbcl-build && \
	  cd /tmp/sbcl-build && \
	  curl -L $(SBCL_URL_BIN) | tar xjf - && \
	  cd sbcl* && \
	  sudo sh install.sh
	@echo "=> Cleaning up"
	rm -rf /tmp/sbcl-build



