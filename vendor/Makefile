#-*- mode:makefile-gmake; -*-
ROOT ?= $(shell pwd)

init:
	@echo "=> Updating submodules"
	(cd .. ; git submodule update --init --recursive;)

all: libfixposix-build

clean: libfixposix-clean

libfixposix-build: libfixposix/build/lib
libfixposix-clean:
	cd libfixposix && rm -rf * && git reset --hard HEAD

libfixposix/build/lib: libfixposix/config.h
	$(MAKE) -C libfixposix install
libfixposix/config.h: libfixposix/configure
	cd libfixposix && ./configure --prefix `pwd`/build
libfixposix/configure:
	cd libfixposix && \
	autoreconf -i
