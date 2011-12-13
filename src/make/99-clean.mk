clean-all: | clean
clean: | lisp-fasl-clean asdf-clean lisp-clean externals-clean clean-build clean-all-bundle

.PHONY: kill-everything-clean
kill-everything-fast:
	KILL_EVERYTHING_FAST=1 $(MAKE) kill-everything

.PHONY: kill-everything
.IGNORE: kill-everything
kill-everything:
ifeq ($(KILL_EVERYTHING_FAST),1)
else
	@echo "=> Killing all processes"
	cat $(ROOT)/run/* | xargs kill
	@echo "=> Waiting for processes to die"
	sleep 10
endif
	@echo "=> Kill -9'ing stragglers"
	cat $(ROOT)/run/* | xargs kill -9
	@echo "=> Deleting stale pidfiles"
	rm $(ROOT)/run/*
