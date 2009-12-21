# makefile to generate mocca, a gui for EMC2
# adjust WM (gtk, gtk2), EMCDIR, LAZDIR


WM=gtk
EMCDIR = /home/gerd/emc2/emc2-2.3.4
LAZDIR = /usr/lib/lazarus/0.9.28.2

COMPILE = gcc -c -v -g
COMPILEPAS = fpc
COMPILEMOC = fpc -S2cghi -CX -O1 -TLinux -XX -vewnhi -l -dLCL -dLCL$(WM)
LINK = gcc -v

#INCLUDES = -I$(EMCDIR)/include
INCLUDES += -I$(EMCDIR)/src
#INCLUDES += -I$(EMCDIR)/lib
#INCLUDES += -I$(EMCDIR)/bin
#INCLUDES += -I$(EMCDIR)/src/emc/ini
#INCLUDES += -I$(EMCDIR)/src/emc/iotask
#INCLUDES += -I$(EMCDIR)/src/emc/kinematics
INCLUDES += -I$(EMCDIR)/src/emc/motion
INCLUDES += -I$(EMCDIR)/src/emc/nml_intf
#INCLUDES += -I$(EMCDIR)/src/emc/usr_intf
INCLUDES += -I$(EMCDIR)/src/emc/rs274ngc
#INCLUDES += -I$(EMCDIR)/src/emc/task
INCLUDES += -I$(EMCDIR)/src/libnml/posemath
INCLUDES += -I$(EMCDIR)/src/libnml/inifile
INCLUDES += -I$(EMCDIR)/src/libnml/os_intf
INCLUDES += -I$(EMCDIR)/src/libnml/nml
INCLUDES += -I$(EMCDIR)/src/libnml/cms
INCLUDES += -I$(EMCDIR)/src/libnml/rcs
#INCLUDES += -I/usr/lib
#INCLUDES += -I/lib

LCL = -Fu.
LCL += -Fu$(LAZDIR)/lcl/units/i386-linux
LCL += -Fu$(LAZDIR)/lcl/units/i386-linux/$(WM)
LCL += -Fu$(LAZDIR)/components/custom
LCL += -Fu$(LAZDIR)/components/codetools/units/i386-linux
LCL += -Fu$(LAZDIR)/components/opengl/lib/i386-linux/$(WM)
LCL += -Fu$(LAZDIR)/packager/units/i386-linux

LIBDIR = $(EMCDIR)/lib

COBJ = ../lib/emcpas.o ../lib/simcanon.o

PASPAS = editordlg.pas emc2pas.pas glcanon.pas gllist.pas \
glview_old.pas glview.pas jogclient.pas mdiclient.pas mocemc.pas \
mocglb.pas mocini.pas mocjoints.pas mocmain.pas mocstatusdlg.pas \
offsetdlg.pas runclient.pas simclient.pas touchoff.pas

PASLRS = editordlg.lrs jogclient.lrs mdiclient.lrs mocmain.lrs \
mocstatusdlg.lrs offsetdlg.lrs runclient.lrs simclient.lrs touchoff.lrs

PASFRM = editordlg.lfm jogclient.lfm mdiclient.lfm mocmain.lfm \
mocstatusdlg.lfm offsetdlg.lfm runclient.lfm simclient.lfm touchoff.lfm

../lib/emcpas.o: Makefile emcpas.cc emcpas.h
	$(COMPILE) $(INCLUDES) -o../lib/emcpas.o -l/usr/lib emcpas.cc

../lib/simcanon.o: Makefile simcanon.cc emcpas.h
	$(COMPILE) $(INCLUDES) -o../lib/simcanon.o simcanon.cc

#../lib/glcanon.o: glcanon.pas
#	$(COMPILEPAS) -FU../lib -o../lib/glcanon.o glcanon.pas

../lib/mocca: Makefile mocca.lpr $(COBJ) $(PASPAS) $(PASLRS) $(PASFRM)
	$(COMPILEMOC) $(LCL) -Fl$(LIBDIR) -Fl../lib -Fu../lib -FU../lib -o../lib/mocca mocca.lpr
	
all: ../lib/mocca


          

