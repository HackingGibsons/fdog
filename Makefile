#-*- mode:makefile-gmake; -*-
ROOT ?= $(shell pwd)
TARGET ?= bin/fdog
LISP ?= $(shell which sbcl || echo /sbcl/does/not/exist)
REGISTRYD ?= $(HOME)/.config/common-lisp/source-registry.conf.d
BUILDAPP ?= $(ROOT)/bin/buildapp
DESTDIR ?= /usr/local/bin
STAGEDIR ?= $(ROOT)/build

FDOG ?= $(ROOT)/$(TARGET)

FDOG_ASDF_CONF = (:directory \"$(ROOT)/\")
FDOG_ASDF_CONF_NAME = $(REGISTRYD)/"01-fdog.conf"

VENDOR_ASDF_CONF = (:tree \"$(ROOT)/vendor/\")
VENDOR_ASDF_CONF_NAME = $(REGISTRYD)/"02-fdog-vendor.conf"

QL_ROOT_NAME ?= quicklisp
QL_ROOT_PATH = $(HOME)/$(QL_ROOT_NAME)

DEBUG ?= 1 # (declaim (debug 1)) is the SBCL default

## TODO: Clean up the mess below in these smaller messes
.DEFAULT_GOAL=help
include $(ROOT)/src/make/*.mk
include $(ROOT)/tests/make/*.mk

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

