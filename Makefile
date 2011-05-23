ROOT ?= $(shell pwd)
LISP ?= $(shell which sbcl)
REGISTRYD ?= $(HOME)/.config/common-lisp/source-registry.conf.d

FDOG_ASDF_CONF = (:directory \"$(ROOT)/\")
FDOG_ASDF_CONF_NAME = $(REGISTRYD)/"01-fdog.conf"

VENDOR_ASDF_CONF = (:tree \"$(ROOT)/vendor/\")
VENDOR_ASDF_CONF_NAME = $(REGISTRYD)/"02-fdog-vendor.conf"


# Inteded UI targets
init: submodules sanity-check quicklisp configured-asdf

submodules:
	git submodule update --init --recursive

# Dependency targets
QL_TEST ?= $(LISP) --eval '(quit :unix-status (if (find-package :ql) 0 1))'
QL_URL ?= "https://github.com/quicklisp/quicklisp-bootstrap/raw/master/quicklisp.lisp"
quicklisp: sanity-check
	$(QL_TEST) || {\
	  echo "=> QL is missing. Installing"; \
	  curl -L $(QL_URL) > /tmp/quicklisp.lisp; \
	  $(LISP) --eval '(sb-ext:disable-debugger)' --load /tmp/quicklisp.lisp \
	    --eval '(quicklisp-quickstart:install)' \
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

# Utility targets
sanity-check: $(ROOT)/fdog.asd $(LISP)
	@echo "!> Environment looks sane. I'll allow this."

ubuntu: ubuntu-sbcl ubuntu-mongrel2

MONGREL2_URL_SRC ?= "http://mongrel2.org/static/downloads/mongrel2-1.6.tar.bz2"
ubuntu-mongrel2: ubuntu-0mq
	@echo "=> Installing mongrel2"
	yes Y | sudo aptitude install sqlite3 libsqlite3-dev
	rm -rf /tmp/mongrel2-build && \
	mkdir -p /tmp/mongrel2-build && \
	  cd /tmp/mongrel2-build && \
	  curl $(MONGREL2_URL_SRC) | tar xjf - && \
	  cd mongrel2* && \
	  make && sudo make install

0MQ_URL_SRC ?= "http://download.zeromq.org/zeromq-2.1.7.tar.gz"
ubuntu-0mq: ubuntu-basics
	@echo "=> Installing 0mq"
	yes Y | sudo aptitude install uuid-dev
	rm -rf /tmp/0mq-build && \
	mkdir -p /tmp/0mq-build && \
	  cd /tmp/0mq-build && \
	  curl $(0MQ_URL_SRC) | tar xzf - && \
	  cd zeromq* && \
	  ./configure && \
	  make && sudo make install && \
	  sudo ldconfig



SBCL_URL_BIN ?= "http://prdownloads.sourceforge.net/sbcl/sbcl-1.0.48-x86-64-linux-binary.tar.bz2"
ubuntu-sbcl: ubuntu-basics
	@echo "=> Fetching/extracting/installing SBCL binary"
	[ -e "$(LISP)" ] && echo "SBCL Already installed." || { \
	  rm -rf /tmp/sbcl-build &&
	  mkdir -p /tmp/sbcl-build && \
	    cd /tmp/sbcl-build && \
	    curl -L $(SBCL_URL_BIN) | tar xjf - && \
	    cd sbcl* && \
	    sudo sh install.sh; \
	}
	@echo "=> Cleaning up"
	rm -rf /tmp/sbcl-build

ubuntu-basics:
	@echo "=> Making sure we have aptitude"
	yes Y | sudo apt-get install aptitude
	@echo "=> Round one of dependancies"
	yes Y | sudo aptitude install curl build-essential
