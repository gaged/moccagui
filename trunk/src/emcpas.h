// emcpas interface

#ifndef EMCPAS_H
#define EMCPAS_H

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <signal.h>
#include <math.h>
#include <ctype.h>
#include <values.h>           
#include <limits.h>    
#include <stdarg.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>   
#include "rcs.hh"               
#include "emc.hh"              
#include "emc_nml.hh"
#include "emcglb.h"             
#include "emccfg.h"            
#include "inifile.hh"         
#include "rcs_print.hh"
#include "nml_oi.hh"
#include "timer.hh"

extern "C" void getActiveGCodes();
extern "C" void getActiveMCodes();

extern "C" bool geterror(char *msg);
extern "C" double convertLinearUnits(double u);

extern "C" int iniOpen(const char *filename);
extern "C" int iniClose();
extern "C" int iniGet(char *varstr, char *secstr, char *buffer);

extern "C" int emcTaskNmlGet();
extern "C" int emcErrorNmlGet();
extern "C" int emcNmlInit();

extern "C" int updateStatus();
extern "C" int updateError();

extern "C" int emcCommandWaitReceived(int serial_number);
extern "C" int emcCommandWaitDone(int serial_number);

extern "C" int sendDebug(int level);
extern "C" int sendEstop();
extern "C" int sendEstopReset();
extern "C" int sendMachineOn();
extern "C" int sendMachineOff();
extern "C" int sendManual();
extern "C" int sendAuto();
extern "C" int sendMdi();
extern "C" int sendOverrideLimits(int axis);
extern "C" int sendJogStop(int axis);
extern "C" int sendJogCont(int axis, double speed);
extern "C" int sendJogIncr(int axis, double speed, double incr);
extern "C" int sendMistOn();
extern "C" int sendMistOff();
extern "C" int sendFloodOn();
extern "C" int sendFloodOff();
extern "C" int sendLubeOn();
extern "C" int sendLubeOff();
extern "C" int sendSpindleForward();
extern "C" int sendSpindleReverse();
extern "C" int sendSpindleOff();
extern "C" int sendSpindleIncrease();
extern "C" int sendSpindleDecrease();
extern "C" int sendSpindleConstant();
extern "C" int sendBrakeEngage();
extern "C" int sendBrakeRelease();
extern "C" int sendAbort();
extern "C" int sendHome(int axis);
extern "C" int sendUnHome(int axis);
extern "C" int sendFeedOverride(double override);
extern "C" int sendMaxVelocity(double velocity);
extern "C" int sendSpindleOverride(double override);
extern "C" int sendTaskPlanInit();
extern "C" int sendProgramOpen(char *program);
extern "C" int sendProgramRun(int line);
extern "C" int sendProgramPause();
extern "C" int sendProgramResume();
extern "C" int sendSetOptionalStop(bool state);
extern "C" int sendProgramStep();
extern "C" int sendMdiCmd(const char *mdi);
extern "C" int sendLoadToolTable(const char *file);
extern "C" int sendToolSetOffset(int tool, double length, double diameter);
extern "C" int sendToolSetOffset2(int id, double zoffset, double xoffset, double diameter, double frontangle, double backangle, int orientation);
extern "C" int sendAxisSetBacklash(int axis, double backlash);
extern "C" int sendAxisSetOutput(int axis, double output);
extern "C" int sendAxisEnable(int axis, int val);
extern "C" int sendAxisLoadComp(int axis, const char *file, int type);
extern "C" int sendClearProbeTrippedFlag();
extern "C" int sendProbe(double x, double y, double z);

extern "C" bool getEStop();
extern "C" bool getMachineOn();

extern "C" int getTaskMode();

extern "C" bool getMistOn();
extern "C" bool getFloodOn();
extern "C" bool getLubeOn();

extern "C" int getSpindle();

extern "C" bool getBrakeOn();

extern "C" bool getJointHomed(int joint);

extern "C" int getFeedOverride();

extern "C" double getDtgPos(int axis);
extern "C" double getAbsCmdPos(int axis);
extern "C" double getAbsPos(int axis);
extern "C" double getRelCmdPos(int axis);
extern "C" double getRelPos(int axis);


#endif
