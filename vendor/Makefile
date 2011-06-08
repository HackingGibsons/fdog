#These will be fetched and extracted here

all: nst libfixposix-build

clean: nst-clean libfixposix-clean

libfixposix-build: libfixposix/build/lib
libfixposix-clean:
	cd libfixposix && rm -rf * && git reset --hard HEAD

libfixposix/build/lib: libfixposix/config.h
	$(MAKE) -C libfixposix install
libfixposix/config.h: libfixposix/configure
	cd libfixposix && ./configure --prefix `pwd`/build
libfixposix/configure:
	cd libfixposix && \
	ls && \
	autoreconf -i

nst: nst-4.0.0
nst-clean:
	rm -rf nst-4.0.0
nst-4.0.0:
	curl -L http://nst.maraist.org/download/nst-4.0.0.tar.gz | tar zxf -


