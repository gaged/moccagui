WM=gtk2

COMPILE = gcc -c -v -g
COMPILEPAS = fpc
COMPILEMOC = fpc -S2cghi -CX -O1 -TLinux -XX -vewnhi -l -k-L-dLCL -dLCL$(WM)
LINK = gcc -v

EMCDIR = /home/gtom/emc2-2.5.1
LAZDIR = /usr/lib/lazarus/0.9.30.4

INCLUDES = -I$(EMCDIR)/include
INCLUDES += -I$(EMCDIR)/src/emc/nml_intf
INCLUDES += -I$(EMCDIR)/src/emc/pythonplugin
INCLUDES += -I$(EMCDIR)/src/emc/rs274ngc
INCLUDES += -I/usr/include/python2.6

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

emcpas.o: emcpas.cc
	$(COMPILE) $(INCLUDES) -o./lib/emcpas.o -DVER_25 -UDSOHANDLE emcpas.cc

simcanon.o: simcanon.cc
	$(COMPILE) $(INCLUDES) -o./lib/simcanon.o -DVER_25 simcanon.cc

mocca.o: ../src/mocca.lpr
	$(COMPILEMOC) -k-L$(EMCDIR)/lib $(LCL) -dVER_25 -dLINUX_CNC -Fl./lib -FU./lib -o./mocca2.5 ../src/mocca.lpr
	cp ./mocca2.5 $(EMCDIR)/bin/mocca
	
clean:
	rm ./lib/*

all: simcanon.o emcpas.o mocca.o


          

