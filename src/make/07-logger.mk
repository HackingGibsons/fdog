#-*- mode:makefile-gmake; -*-

.PHONY: log-all
log-all: logger log-sender forwarder

.PHONY: log-all-macports
log-all-macports: logger-macports log-sender-macports forwarder-macports

logger: LIBS = -lzmq
logger: logger.c
	gcc -o bin/$@ $^ $(OPTFLAGS) $(LIBFLAGS) $(LIBS)

log-sender: LIBS = -lzmq
log-sender: log-sender.c
	gcc -o bin/$@ $^ $(OPTFLAGS) $(LIBFLAGS) $(LIBS)

forwarder: LIBS = -lzmq
forwarder: forwarder.c
	gcc -o bin/$@ $^ $(OPTFLAGS) $(LIBFLAGS) $(LIBS)


.PHONY: log-sender-macports
log-sender-macports: OPTFLAGS += -I/opt/local/include/
log-sender-macports: LIBFLAGS += -L/opt/local/lib/
log-sender-macports: log-sender

.PHONY: logger-macports
logger-macports: OPTFLAGS += -I/opt/local/include/
logger-macports: LIBFLAGS += -L/opt/local/lib/
logger-macports: logger

.PHONY: forwarder-macports
forwarder-macports: OPTFLAGS += -I/opt/local/include/
forwarder-macports: LIBFLAGS += -L/opt/local/lib/
forwarder-macports: forwarder

.PHONY: logger-clean
logger-clean:
	rm bin/logger bin/log-sender bin/forwarder
