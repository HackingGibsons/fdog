# $Id: Makefile,v 1.1.1.1 2003/11/13 18:32:45 eenge Exp $
# $Source: /project/cl-syslog/cvsroot/cl-syslog/Makefile,v $

clean:
	find -name "*.fasl" -o -name "*.faslmt" -o -name "*~" -o -name "*.err" -o -name "*.x86f" | xargs rm 

commit:
	make clean; cvs up; cvs ci

