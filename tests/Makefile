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

saicanon.o: saicanon.cc
	$(COMPILE) $(INCLUDES) -o saicanon.o saicanon.cc

driver.o: driver.cc
	$(COMPILE) $(INCLUDES) -o driver.o driver.cc

driver: driver.o saicanon.o
	$(LINK) -o driver /home/gtom/emc/lib/librs274.so /home/gtom/emc/lib/libemcini.so driver.o saicanon.o -lm
