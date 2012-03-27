#-*- mode:makefile-gmake; -*-

logger: LIBS = -lzmq
logger: logger.c
	gcc -o bin/$@ $^ $(OPTFLAGS) $(LIBFLAGS) $(LIBS)

log-sender: LIBS = -lzmq
log-sender: log-sender.c
	gcc -o bin/$@ $^ $(OPTFLAGS) $(LIBFLAGS) $(LIBS)

.PHONY: log-sender-macports
log-sender-macports: OPTFLAGS += -I/opt/local/include/
log-sender-macports: LIBFLAGS += -L/opt/local/lib/
log-sender-macports: log-sender

.PHONY: logger-macports
logger-macports: OPTFLAGS += -I/opt/local/include/
logger-macports: LIBFLAGS += -L/opt/local/lib/
logger-macports: logger

.PHONY: logger-clean
logger-clean:
	rm bin/logger
