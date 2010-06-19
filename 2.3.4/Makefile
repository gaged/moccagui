WM=gtk2

COMPILE = gcc -c -v -g
COMPILEPAS = fpc
COMPILEMOC = fpc -S2cghi -CX -O1 -TLinux -XX -vewnhi -l -k-L-dLCL -dLCL$(WM)
LINK = gcc -v

EMCDIR = /home/gtom/emc2-2.3.4
LAZDIR = /usr/lib/lazarus/0.9.28.2

INCLUDES = -I$(EMCDIR)/include

LCL = -Fu.
LCL += -Fu../src
LCL += -Fu./lib
LCL += -Fu../pkg
LCL += -Fu../pkg/lib/i386-linux
LCL += -Fu$(LAZDIR)/lcl/units/i386-linux
LCL += -Fu$(LAZDIR)/lcl/units/i386-linux/$(WM)
LCL += -Fu$(LAZDIR)/components/custom
LCL += -Fu$(LAZDIR)/components/opengl
LCL += -Fu$(LAZDIR)/packager/units/i386-linux

emcpas.o: ../src/emcpas.cc
	$(COMPILE) $(INCLUDES) -o./lib/emcpas.o -DVER_23 -UDSOHANDLE ../src/emcpas.cc

simcanon.o: ../src/simcanon.cc
	$(COMPILE) $(INCLUDES) -o./lib/simcanon.o -DVER_23 ../src/simcanon.cc

mocca.o: ../src/mocca.lpr
	$(COMPILEMOC) -k-L$(EMCDIR)/lib $(LCL) -dVER_23 -Fl./lib -FU./lib -o./mocca2.3 ../src/mocca.lpr
	cp ./mocca2.3 $(EMCDIR)/bin/mocca

clean:
	rm ./lib/*
	
all: simcanon.o emcpas.o mocca.o


          

