# Lithp thtuff
LISP ?= $(shell which sbcl || echo /sbcl/does/not/exist)

# Asdf configuration
REGISTRYD ?= $(HOME)/.config/common-lisp/source-registry.conf.d

lisp-clean:
	@echo "=> Clearing common-lisp cache"
	rm -rf ~/.cache/common-lisp/

lisp-fasl-clean:
	@echo "=> Clearing fasls from $(ROOT)"
	find $(ROOT) -name '*.fasl' -delete
