WM=gtk2

COMPILE = gcc -c -v -g
COMPILEPAS = fpc
COMPILEMOC = fpc -S2cghi -CX -O1 -TLinux -XX -vewnhi -l -k-L-dLCL -dLCL$(WM)
LINK = gcc -v

EMCDIR = /home/gtom/emc2-2.3.4
LAZDIR = /usr/lib/lazarus/0.9.28.2

INCLUDES = -I$(EMCDIR)/include
#INCLUDES += -I$(EMCDIR)/src/emc/nml_intf
#INCLUDES += -I$(EMCDIR)/src/emc/rs274ngc

LCL = -Fu.
#LCL += -Fu../src
LCL += -Fu../lib
LCL += -Fu../pkg
LCL += -Fu../pkg/lib/i386-linux
LCL += -Fu$(LAZDIR)/lcl/units/i386-linux
LCL += -Fu$(LAZDIR)/lcl/units/i386-linux/$(WM)
LCL += -Fu$(LAZDIR)/components/custom
LCL += -Fu$(LAZDIR)/components/opengl
LCL += -Fu$(LAZDIR)/packager/units/i386-linux

emcpas.o: emcpas.cc
	$(COMPILE) $(INCLUDES) -o../lib/emcpas.o -DVER_23 -DDSOHANDLE emcpas.cc

simcanon.o: simcanon.cc
	$(COMPILE) $(INCLUDES) -o../lib/simcanon.o -DVER_23 simcanon.cc

mocca.o: mocca.lpr
	$(COMPILEMOC) -k-L$(EMCDIR)/lib $(LCL)  -Fl../lib -FU../lib -o../lib/mocca mocca.lpr
	
all: simcanon.o emcpas.o mocca.o


          

