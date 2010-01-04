
WM=gtk2

COMPILE = gcc -c -v -g
COMPILEPAS = fpc
COMPILEMOC = fpc -S2cghi -CX -O1 -TLinux -XX -vewnhi -l -dLCL -dLCL$(WM)
LINK = gcc -v

EMCDIR = /home/gtom/emc
LAZDIR = /usr/lib/lazarus/0.9.28.2
#LAZDIR = /usr/lib/lazarus

INCLUDES = -I$(EMCDIR)/include
INCLUDES += -I$(EMCDIR)/src
INCLUDES += -I$(EMCDIR)/lib
INCLUDES += -I$(EMCDIR)/bin
INCLUDES += -I$(EMCDIR)/src/emc/ini
INCLUDES += -I/usr/lib
INCLUDES += -I/lib

LCL = -Fu.
LCL += -Fu$(LAZDIR)/lcl/units/i386-linux
LCL += -Fu$(LAZDIR)/lcl/units/i386-linux/$(WM)
LCL += -Fu$(LAZDIR)/components/custom
LCL += -Fu$(LAZDIR)/components/opengl
LCL += -Fu$(LAZDIR)/packager/units/i386-linux

LIBDIR = $(EMCDIR)/lib

emcpas.o: emcpas.cc
	$(COMPILE) $(INCLUDES) -o../lib/emcpas.o -l/usr/lib emcpas.cc

simcanon.o: simcanon.cc
	$(COMPILE) $(INCLUDES) -o../lib/simcanon.o simcanon.cc

glcanon.o: glcanon.pas
	$(COMPILEPAS) -FU../lib -o../lib/glcanon.o glcanon.pas

mocca.o: mocca.lpr
	$(COMPILEMOC) $(LCL) -Fl$(LIBDIR) -Fl../lib -Fu../lib -Fu../opengl -FU../lib -o../lib/mocca mocca.lpr
	
all: simcanon.o emcpas.o mocca.o


          

