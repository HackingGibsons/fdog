ROOT ?= $(shell pwd)
TARGET ?= bin/fdog
LISP ?= $(shell which sbcl || echo /sbcl/does/not/exist)
REGISTRYD ?= $(HOME)/.config/common-lisp/source-registry.conf.d
BUILDAPP ?= $(ROOT)/bin/buildapp
DESTDIR ?= /usr/local/bin

FDOG ?= $(ROOT)/$(TARGET)

FDOG_ASDF_CONF = (:directory \"$(ROOT)/\")
FDOG_ASDF_CONF_NAME = $(REGISTRYD)/"01-fdog.conf"

VENDOR_ASDF_CONF = (:tree \"$(ROOT)/vendor/\")
VENDOR_ASDF_CONF_NAME = $(REGISTRYD)/"02-fdog-vendor.conf"

QL_ROOT_NAME ?= quicklisp
QL_ROOT_PATH = $(HOME)/$(QL_ROOT_NAME)

DEBUG ?= 1 # (declaim (debug 1)) is the SBCL default


# Inteded UI targets
help:
	@echo "Use: [OPTION=value..] make [target]"
	@echo
	@echo "Some suggested targets:"
	@echo " init - Sanity check your lisp installation,"
	@echo "        init the git submodules, test for"
	@echo "        and possibly install quicklisp"
	@echo "        and configures ASDF to find this repository"
	@echo
	@echo " test - Cleans everything, runs the test suite, and cleans"
	@echo "        everything one more time."
	@echo
	@echo " all - Builds a distribution to $(TARGET)"
	@echo
	@echo " install - Install the distribution to $(DESTDIR)"
	@echo
	@echo " clean-build - Clean out any built artifacts"
	@echo
	@echo " clean - Sanitize the environment, removing the common-lisp cache"
	@echo
	@echo " ubuntu - Build out all the dependencies on an ubuntu system"
	@echo
	@echo
	@echo "Some interesting options:"
	@echo " QL_ROOT_NAME = $(QL_ROOT_NAME)"
	@echo '   Directory relative to $$HOME of the quicklisp install'
all: fdog

test: clean init
	@echo "=> Running tests."
	$(MAKE) run-tests
	$(MAKE) clean

run-tests:
	LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$(ROOT)/vendor/libfixposix/src/lib/.libs \
	CPATH=$(ROOT)/vendor/libfixposix/src/include \
	$(LISP) --no-userinit \
	        --load $(QL_ROOT_PATH)/setup.lisp \
	        --eval '(ql:quickload :fdog)' \
	        --eval '(ql:quickload :fdog-tests)' \
	        --eval '(asdf:test-system :fdog)' \
	        --eval '(quit)'

install: all
	sudo install -B prev -b $(FDOG) $(DESTDIR)

init: sanity-check submodules quicklisp configured-asdf

submodules:
	git submodule update --init --recursive
	$(MAKE) externals

externals-clean:
	$(MAKE) -C $(ROOT)/vendor clean

externals:
	$(MAKE) -C $(ROOT)/vendor all

fdog: init buildapp $(FDOG)
$(FDOG):
	@echo "=> Assuring subsystem build"
	LD_LIBRARY_PATH=$(LD_LIBRARY_PATH):$(ROOT)/vendor/libfixposix/src/lib/.libs \
	CPATH=$(ROOT)/vendor/libfixposix/src/include:$(CPATH) \
	$(LISP) --eval '(sb-ext:disable-debugger)' \
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
		    --eval '(sb-ext:disable-debugger)' \
	            --eval '(declaim (optimize (debug $(DEBUG))))' \
	            --load $(QL_ROOT_PATH)/setup.lisp \
	            --eval '(ql:quickload :fdog)' \
	            --dispatched-entry '/fdog-cli:fdog-main' \
	|| { echo '[ERROR] Build failed!'; \
	     rm -f $(FDOG); exit 1; }

clean-build:
	@echo "=> Removing fdog builds"
	rm -rf $(FDOG)

clean: externals-clean
	@echo "=> Clearing common-lisp cache"
	rm -rf ~/.cache/common-lisp/
	@echo "=> Cleaning up buildapp"
	rm -rf $(BUILDAPP)
	$(MAKE) clean-build

# Dependency targets
buildapp: quicklisp $(BUILDAPP)
$(BUILDAPP):
	$(LISP) --eval '(sb-ext:disable-debugger)' \
	        --eval '(ql:quickload :buildapp)' \
	        --eval '(buildapp:build-buildapp "$(BUILDAPP)")' \
	        --eval '(quit)'

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

# Utility targets
sanity-check: $(ROOT)/fdog.asd $(LISP)
	@echo "!> Environment looks sane. I'll allow this."

# Ubuntu buildout
ubuntu: ubuntu-sbcl ubuntu-mongrel2

MONGREL2_URL_SRC ?= "http://mongrel2.org/static/downloads/mongrel2-1.6.tar.bz2"
mongrel2: 0mq
	@echo "=> Installing mongrel2"
	rm -rf /tmp/mongrel2-build && \
	mkdir -p /tmp/mongrel2-build && \
	  cd /tmp/mongrel2-build && \
	  curl $(MONGREL2_URL_SRC) | tar xjf - && \
	  cd mongrel2* && \
	  make && sudo make install

0MQ_URL_SRC ?= "http://download.zeromq.org/zeromq-2.1.7.tar.gz"
0mq:
	@echo "=> Installing 0mq"
	rm -rf /tmp/0mq-build && \
	mkdir -p /tmp/0mq-build && \
	  cd /tmp/0mq-build && \
	  curl $(0MQ_URL_SRC) | tar xzf - && \
	  cd zeromq* && \
	  ./configure && \
	  make && sudo make install && \
	  sudo ldconfig

ubuntu-mongrel2: ubuntu-0mq
	@echo "=> Installing Ubuntu mongrel2 deps"
	yes Y | sudo aptitude install sqlite3 libsqlite3-dev
	$(MAKE) mongrel2

ubuntu-0mq: ubuntu-basics
	@echo "=> Installing Ubuntu 0mq deps"
	yes Y | sudo aptitude install uuid-dev
	$(MAKE) 0mq


SBCL_URL_BIN ?= "http://prdownloads.sourceforge.net/sbcl/sbcl-1.0.48-x86-64-linux-binary.tar.bz2"
ubuntu-sbcl: ubuntu-basics
	@echo "=> Fetching/extracting/installing SBCL binary"
	[ -e "$(LISP)" ] && echo "SBCL Already installed." || { \
	  rm -rf /tmp/sbcl-build && \
	  mkdir -p /tmp/sbcl-build && \
	    cd /tmp/sbcl-build && \
	    curl -L $(SBCL_URL_BIN) | tar xjf - && \
	    cd sbcl* && \
	    sudo sh install.sh; \
	}
	@echo "=> Cleaning up"
	rm -rf /tmp/sbcl-build

ubuntu-basics:
	sudo apt-get update
	@echo "=> Making sure we have aptitude"
	yes Y | sudo apt-get install aptitude
	@echo "=> Round one of dependancies"
	yes Y | sudo aptitude install curl build-essential
