COMPILE = g++ -c -v -g
COMPILEPAS = fpc
LINK = g++ -v

INCLUDES = -I/home/gtom/emc/include
INCLUDES += -I/home/gtom/emc/src
INCLUDES += -I/home/gtom/emc/lib
INCLUDES += -I/home/gtom/emc/bin
INCLUDES += -I/home/gtom/emc/src/emc/ini
INCLUDES += -I/home/gtom/emc/src/emc/iotask
INCLUDES += -I/home/gtom/emc/src/emc/kinematics
INCLUDES += -I/home/gtom/emc/src/emc/motion
INCLUDES += -I/home/gtom/emc/src/emc/nml_intf
INCLUDES += -I/home/gtom/emc/src/emc/usr_intf
INCLUDES += -I/home/gtom/emc/src/emc/rs274ngc
INCLUDES += -I/home/gtom/emc/src/emc/task
INCLUDES += -I/home/gtom/emc/src/libnml/posemath
INCLUDES += -I/home/gtom/emc/src/libnml/inifile
INCLUDES += -I/home/gtom/emc/src/libnml/os_intf
INCLUDES += -I/home/gtom/emc/src/libnml/nml
INCLUDES += -I/home/gtom/emc/src/libnml/cms
INCLUDES += -I/home/gtom/emc/src/libnml/rcs

simmodule.o: simmodule.cc
	$(COMPILE) $(INCLUDES) -o simmodule.o simmodule.cc

emcpas.o: emcpas.cc
	$(COMPILE) $(INCLUDES) -o emcpas.o emcpas.cc

sim.o: sim.pas
	$(COMPILEPAS) sim.pas
	
all: simmodule.o emcpas.o sim.o
	

# $(LINK) $(INCLUDES) -o mocgcode.o ~/emc/lib/librs274.so ~/emc/lib/libemcini.so simmodule.o sim.o -lm

