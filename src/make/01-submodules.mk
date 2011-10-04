submodules:
	git submodule update --init --recursive
	$(MAKE) externals

externals-clean:
	$(MAKE) -C $(ROOT)/vendor clean

externals:
	$(MAKE) -C $(ROOT)/vendor all


