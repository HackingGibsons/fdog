#-*- mode:makefile-gmake; -*-

logger: LIBS = -lzmq
logger: logger.c
	gcc -o bin/$@ $^ $(OPTFLAGS) $(LIBFLAGS) $(LIBS)

.PHONY: logger-macports
logger-macports: OPTFLAGS += -I/opt/local/include/
logger-macports: LIBFLAGS += -L/opt/local/lib/
logger-macports: logger

logger-clean:
	rm bin/logger
