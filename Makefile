#-*- mode:makefile-gmake; -*-
ROOT ?= $(shell pwd)
TARGET ?= afdog
include $(ROOT)/src/make/*.mk

README ?= README.txt
define HELP
	if [ "$(VERBOSITY)" -gt "1" ]; then \
		echo "$(README)"; \
		echo "---------"; \
		cat $(ROOT)/$(README); \
		echo "---------"; \
		echo; \
	fi

	echo "Interesting targets:"
	echo "--------------------"
	echo "make help"
	echo "  This noise"
	echo
	echo "make init"
	echo "  Configures the submodules, vendor packages, user lisp dependencies"
	echo "  and anything else needed to actually boot the package"
	echo
endef
