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
