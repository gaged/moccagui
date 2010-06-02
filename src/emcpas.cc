//    Copyright 2005-2009 Alex Joni, Chris Radek, Jeff Epler, John Kasunich,
//    Paul Corner, Peter G. Vavaroutsos, Ray Henry
//
//    This program is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License version 2 as
//    published by the Free Software Foundation.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#include "emcpas.h"

#define EMC_COMMAND_DELAY   0.1	// how long to sleep between checks
#define CM_PER_MM 0.1
#define DEFAULT_PATH "../../nc_files/"
#define MDI_LINELEN 80

enum LINEAR_UNIT_CONVERSION {
    LINEAR_UNITS_CUSTOM = 1,
    LINEAR_UNITS_AUTO,
    LINEAR_UNITS_MM,
    LINEAR_UNITS_INCH,
    LINEAR_UNITS_CM
};

enum ANGULAR_UNIT_CONVERSION {
    ANGULAR_UNITS_CUSTOM = 1,
    ANGULAR_UNITS_AUTO,
    ANGULAR_UNITS_DEG,
    ANGULAR_UNITS_RAD,
    ANGULAR_UNITS_GRAD
};

enum EMC_WAIT_TYPE {
    EMC_WAIT_NONE = 1,
    EMC_WAIT_RECEIVED,
    EMC_WAIT_DONE
};

extern EMC_WAIT_TYPE emcWaitType;
extern LINEAR_UNIT_CONVERSION linearUnitConversion;
extern ANGULAR_UNIT_CONVERSION angularUnitConversion;

LINEAR_UNIT_CONVERSION linearUnitConversion ;
ANGULAR_UNIT_CONVERSION angularUnitConversion ;
EMC_STAT *emcStatus;
EMC_WAIT_TYPE emcWaitType;

int emcCommandSerialNumber;
static int emcSavedCommandSerialNumber;

// the NML channels to the EMC task
static RCS_CMD_CHANNEL *emcCommandBuffer;
static RCS_STAT_CHANNEL *emcStatusBuffer;

// the NML channel for errors
static NML *emcErrorBuffer;
static IniFile inifile;

char errorString[NML_ERROR_LEN];
char operatorTextStr[NML_TEXT_LEN];
char operatorDisplayStr[NML_DISPLAY_LEN];

char activeGCodes[MDI_LINELEN] = "";
char activeMCodes[MDI_LINELEN] = "";
char activeFWords[MDI_LINELEN] = "";
char activeSWords[MDI_LINELEN] = "";

double emcTimeout;
double emcSpindleDefaultSpeed = 500;
 
// these are the axis status items, maybe well need them later...
/*    
    minFerror = 1.0;
    maxFerror = 1.0;
    ferrorCurrent = 0.0;
    ferrorHighMark = 0.0;
    output = 0.0;
    input = 0.0;
    inpos = 1;
*/


// this is a workaround for the __dso_handle problem
// undefine this if neccessary

// #define dsohandle

#ifdef DSOHANDLE
extern "C"
{
void *__dso_handle = NULL;
}
#endif

char *ttcomments[CANON_TOOL_MAX+1];
CANON_TOOL_TABLE toolTable[CANON_TOOL_MAX+1];

extern "C" void InitToolTable()
{
  for(int i=0; i<=CANON_TOOL_MAX; i++) {
    ttcomments[i] = (char *)malloc(CANON_TOOL_ENTRY_LEN);
  }
}

extern "C" void DoneToolTable()
{
  for(int i=0; i<CANON_TOOL_MAX; i++) {
    free(ttcomments[i]);
  }
}

extern "C" int loadToolTable(char *filename)
{
    int t;
    FILE *fp;
    char buffer[CANON_TOOL_ENTRY_LEN];
    char comment[CANON_TOOL_ENTRY_LEN];
    const char *name;
    if (filename[0] == 0) {
	name = TOOL_TABLE_FILE;
    } else {
	name = filename;
    }
    if (NULL == (fp = fopen(name, "r"))) {
	return -1;
    }
    for (t = 0; t <= CANON_TOOL_MAX; t++) {
        toolTable[t].id = 0;
	toolTable[t].zoffset = 0.0;
	toolTable[t].diameter = 0.0;
        toolTable[t].xoffset = 0.0;
        toolTable[t].frontangle = 0.0;
        toolTable[t].backangle = 0.0;
        toolTable[t].orientation = 0;
        ttcomments[t][0] = '\0';
    }

    if (NULL == fgets(buffer, 256, fp)) {
	printf("Error: toolfile exists, but is empty\n");
	fclose(fp);
	return -1;
    }

    while (!feof(fp)) {
	int pocket;
	int id;
	double zoffset;
        double xoffset;
	double diameter;
        double frontangle, backangle;
        int orientation;
        int scanned;

	// just read pocket, ID, and length offset
	if (NULL == fgets(buffer, CANON_TOOL_ENTRY_LEN, fp)) {
	    break;
	}
        if((scanned = sscanf(buffer, "%d %d %lf %lf %lf %lf %lf %d %[^\n]",
                             &pocket, &id, &zoffset, &xoffset, &diameter,
                             &frontangle, &backangle, &orientation, comment)) &&
           (scanned == 8 || scanned == 9)) {
            if (pocket < 0 || pocket > CANON_TOOL_MAX) {
                printf("skipping tool: bad pocket number %d\n", pocket);
                continue;
            } else {
                /* lathe tool */
                toolTable[pocket].id = id;
                toolTable[pocket].zoffset = zoffset;
                toolTable[pocket].xoffset = xoffset;
                toolTable[pocket].diameter = diameter;

                toolTable[pocket].frontangle = frontangle;
                toolTable[pocket].backangle = backangle;
                toolTable[pocket].orientation = orientation;
                if(scanned == 9) strcpy(ttcomments[pocket], comment);
            }
        } else if ((scanned = sscanf(buffer, "%d %d %lf %lf %[^\n]",
                                     &pocket, &id, &zoffset, &diameter, comment)) &&
                   (scanned == 4 || scanned == 5)) {
            if (pocket < 0 || pocket > CANON_TOOL_MAX) {
                printf("skipping tool: bad pocket number %d\n", pocket);
                continue;
            } else {
                /* mill tool */
                toolTable[pocket].id = id;
                toolTable[pocket].zoffset = zoffset;
                toolTable[pocket].diameter = diameter;

                // these aren't used on a mill
                toolTable[pocket].frontangle = toolTable[pocket].backangle = 0.0;
                toolTable[pocket].xoffset = 0.0;
                toolTable[pocket].orientation = 0;
                if(scanned == 5) strcpy(ttcomments[pocket], comment);
            }
        } else {
            /* invalid line. skip it silently */
            continue;
        }
    }

    // close the file
    fclose(fp);

    return 0;
}

extern "C" int saveToolTable(char *filename)
{
    int pocket;
    FILE *fp;
    const char *name;
    int lathe_style = 0;

    for(pocket=1; pocket <= CANON_TOOL_MAX; pocket++) {
        if(toolTable[pocket].orientation != 0) {
            lathe_style = 1;
            break;
        }
    }

    // check filename
    if (filename[0] == 0) {
	name = TOOL_TABLE_FILE;
    } else {
	// point to name provided
	name = filename;
    }

    // open tool table file
    if (NULL == (fp = fopen(name, "w"))) {
	// can't open file
	return -1;
    }

    if(lathe_style) {
        fprintf(fp, "%6s%4s%11s%11s%11s%12s%12s%7s  %s\n\n",
                "POCKET", "FMS", "ZOFFSET", "XOFFSET",
                "DIAMETER", "FRONTANGLE", "BACKANGLE", "ORIENT", 
                "COMMENT");
        for (pocket = 1; pocket <= CANON_TOOL_MAX; pocket++) {
            if (toolTable[pocket].id)
                fprintf(fp, "%6d%4d%+11f%+11f%11f%+12f%+12f%7d  %s\n",
                        pocket,
                        toolTable[pocket].id,
                        toolTable[pocket].zoffset, toolTable[pocket].xoffset, toolTable[pocket].diameter,
                        toolTable[pocket].frontangle, toolTable[pocket].backangle, 
                        toolTable[pocket].orientation, ttcomments[pocket]);
        }
    } else {
        fprintf(fp, "%7s%4s%11s%11s  %s\n\n",
                "POCKET", "FMS", "LENGTH", "DIAMETER", "COMMENT");
        for (pocket = 1; pocket <= CANON_TOOL_MAX; pocket++) {
            if (toolTable[pocket].id)
                fprintf(fp, "%7d%4d%+11f%11f  %s\n",
                        pocket,
                        toolTable[pocket].id,
                        toolTable[pocket].zoffset, toolTable[pocket].diameter,
                        ttcomments[pocket]);
        }
    }

    fclose(fp);
    return 0;
}


// some "special" stuff
// we need the emctask interp state to execute scripts 

extern "C" int GetTaskInterpState() { return emcStatus->task.interpState; }
extern "C" int GetTaskExecState() { return emcStatus->task.execState; }

extern "C" int AxisAxisType(int joint) { return emcStatus->motion.axis[joint].axisType; }
extern "C" double AxisUnits(int joint) { return emcStatus->motion.axis[joint].units; }
extern "C" double AxisBacklash(int joint) { return emcStatus->motion.axis[joint].backlash; }
extern "C" double AxisMinPositionLimit(int joint) { return emcStatus->motion.axis[joint].minPositionLimit; }
extern "C" double AxisMaxPositionLimit(int joint) { return emcStatus->motion.axis[joint].maxPositionLimit; }
// extern "C" double AxisVelocity(int joint) { return emcStatus->motion.axis[joint].velocity; }
extern "C" bool AxisHoming(int joint) { return emcStatus->motion.axis[joint].homing; }
extern "C" bool AxisHomed(int joint) { return emcStatus->motion.axis[joint].homed; }
extern "C" bool AxisEnabled(int joint) { return emcStatus->motion.axis[joint].enabled; }
extern "C" bool AxisFault(int joint) { return emcStatus->motion.axis[joint].fault; }
extern "C" double AxisMinSoftLimit(int joint) { return emcStatus->motion.axis[joint].minSoftLimit; }
extern "C" double AxisMaxSoftLimit(int joint) { return emcStatus->motion.axis[joint].maxSoftLimit; }
extern "C" double AxisMinHardLimit(int joint) { return emcStatus->motion.axis[joint].minHardLimit; }
extern "C" double AxisMaxHardLimit(int joint) { return emcStatus->motion.axis[joint].maxHardLimit; }
extern "C" bool AxisOverrideLimits(int joint) { return emcStatus->motion.axis[joint].overrideLimits; }


// these are the traj status items, maybe well need them later...
/*
    cycleTime = 0.0;
    axes = 1;
    axis_mask = 1;
    enabled = OFF;
    inpos = ON;
    queue = 0;
    activeQueue = 0;
    queueFull = OFF;
    id = 0;
    paused = OFF;
    ZERO_EMC_POSE(position);
    ZERO_EMC_POSE(actualPosition);
    maxAcceleration = 1.0;
    ZERO_EMC_POSE(probedPosition);
    probeval = 0;
    ZERO_EMC_POSE(dtg);
    kinematics_type = 0;
    motion_type = 0;
 */
// trajstate read functions

extern "C" int trajAxes() { return emcStatus->motion.traj.axes; }
extern "C" int trajAxisMask() { return emcStatus->motion.traj.axis_mask; }

extern "C" int trajMode() { return emcStatus->motion.traj.scale; }
extern "C" double trajlinearUnits() { return emcStatus->motion.traj.linearUnits; }
extern "C" double trajangularUnits() { return emcStatus->motion.traj.angularUnits; }
extern "C" double trajScale() { return emcStatus->motion.traj.scale; }
extern "C" double trajSpindleScale() { return emcStatus->motion.traj.spindle_scale; }
extern "C" double trajVel() { return emcStatus->motion.traj.velocity; }
extern "C" double trajAcceleration() { return emcStatus->motion.traj.acceleration; }
extern "C" double trajMaxVel() { return emcStatus->motion.traj.maxVelocity; }
extern "C" double trajDtg() { return emcStatus->motion.traj.distance_to_go; }
extern "C" double trajCurrentVel() { return emcStatus->motion.traj.current_vel; }
extern "C" bool trajFeedORideEnabled() { return emcStatus->motion.traj.feed_override_enabled; }
extern "C" bool trajSpindleORideEnabled() { return emcStatus->motion.traj.spindle_override_enabled; }
extern "C" bool trajAdaptiveFeedEnabled() { return emcStatus->motion.traj.adaptive_feed_enabled; }
extern "C" bool trajFeedHoldEnabled() { return emcStatus->motion.traj.feed_hold_enabled; }
extern "C" bool trajProbing() { return emcStatus->motion.traj.probing; }
extern "C" bool trajProbeTripped() { return emcStatus->motion.traj.probe_tripped; }

// these are the TASK_STAT items, maybe well need them later...
/*
    execState = EMC_TASK_EXEC_DONE;
    input_timeout = OFF;
    file[0] = 0;
    ZERO_EMC_POSE(toolOffset);
*/


extern "C" bool taskGetFile(char *msg)
{
  if (emcStatus->task.file[0] != 0) {
    strcpy(msg,emcStatus->task.file);
    return true;
  }
  return false;
}

extern "C" bool taskGetCommand(char *msg)
{
  if (emcStatus->task.command[0] != 0) {
    strcpy(msg,emcStatus->task.command);
    return true;
  }
  return false;
}

// extern "C" int taskInterpSeqNum() { return emcStatus Interp.sequence_number(););

// taskstate read functions
extern "C" int taskMode() { return emcStatus->task.mode; }
extern "C" int taskState() { return emcStatus->task.state; }
extern "C" int taskInterpState() { return emcStatus->task.interpState; }
extern "C" int taskMotionline() { return emcStatus->task.motionLine; }
extern "C" int taskCurrentLine() { return emcStatus->task.currentLine; }
extern "C" int taskReadLine() { return emcStatus->task.readLine; }
// extern "C" double taskRotationXY() { return emcStatus->task.rotation_xy; }
extern "C" bool taskTloIsAlongW() { return emcStatus->task.tloIsAlongW; }
extern "C" int taskProgramUnits() { return emcStatus->task.programUnits; }
extern "C" int taskInterpErrorCode() { return emcStatus->task.interpreter_errcode; }
// extern "C" double taskDelayLeft() { return emcStatus->task.delayLeft; }
extern "C" bool taskBlockDelete() { return emcStatus->task.block_delete_state; }
extern "C" bool taskOptStop() { return emcStatus->task.optional_stop_state; }


// spindlestat read functions
extern "C" double spindleSpeed() { return emcStatus->motion.spindle.speed; }
extern "C" int spindleDirection() { return emcStatus->motion.spindle.direction; }
extern "C" int spindleBrake() { return emcStatus->motion.spindle.brake; }
extern "C" int spindleIncreasing() { return emcStatus->motion.spindle.increasing; }
extern "C" int spindleEnabled() { return emcStatus->motion.spindle.enabled; }

// coolantstat read functions
extern "C" bool coolantMist() { return emcStatus->io.coolant.mist != 0; }
extern "C" bool coolantFlood() { return emcStatus->io.coolant.flood != 0; }

extern "C" int toolPrepped() { return emcStatus->io.tool.toolPrepped; }
extern "C" int toolInSpindle() { return emcStatus->io.tool.toolInSpindle; }
extern "C" double toolLengthOffset() { return emcStatus->task.toolOffset.tran.z; }

// Lubestat read functions
extern "C" bool lubeOn() { return emcStatus->io.lube.on != 0; }
extern "C" int lubeLevel() { return emcStatus->io.lube.level; }

// this is a one call -do all function to get the m,g codes and settings
extern "C" void taskActiveCodes()
{
  int t;
  int code; 
  char string[256];  

  // fill in the active G codes
  activeGCodes[0] = 0;
  for (t = 1; t < ACTIVE_G_CODES; t++) {
    code = emcStatus->task.activeGCodes[t];
    if (code == -1) { continue; }
    if (code % 10) {
      sprintf(string, "G%.1f ", (double) code / 10.0);
    }
    else {
      sprintf(string, "G%d ", code / 10);
    }
    strcat(activeGCodes, string);
  }

  // fill in the active M codes, settings too
  activeMCodes[0] = 0;
  for (t = 1; t < ACTIVE_M_CODES; t++) {
    code = emcStatus->task.activeMCodes[t];
    if (code == -1) {
      continue;
    }
    sprintf(string, "M%d ", code);
    strcat(activeMCodes, string);
  }

  // fill in F and S codes also
  activeFWords[0] = 0;  
  sprintf(string, " F%.0f", emcStatus->task.activeSettings[1]);
  strcat(activeFWords, string);
  
  activeSWords[0] = 0;
  sprintf(string, " S%.0f", emcStatus->task.activeSettings[2]);
  strcat(activeSWords, string);  
}

extern "C" bool geterror(char *msg)
{
  if (errorString[0] != 0) {
    strcpy(msg,errorString);
    errorString[0] = 0;
    return true;
  }
  return false;
}

extern "C" int updateStatus()
{
    NMLTYPE type;
    if (0 == emcStatus || 0 == emcStatusBuffer
	|| !emcStatusBuffer->valid()) {
	return -1;
    }
    switch (type = emcStatusBuffer->peek()) {
    case -1:  // error on CMS channel
	return -1;
	break;
    case 0:  // no new data
    case EMC_STAT_TYPE:	// new data
	break;
    default:
	return -1;
	break;
    }
    return 0;
}

extern "C" int emcCommandWaitReceived(int serial_number)
{
    double end = 0.0;
    while (emcTimeout <= 0.0 || end < emcTimeout) {
      updateStatus();
      if (emcStatus->echo_serial_number == serial_number) {
        return 0;
      }
      esleep(EMC_COMMAND_DELAY);
      end += EMC_COMMAND_DELAY;
    }
    return -1;
}

extern "C" int emcCommandWaitDone(int serial_number)
{
    double end = 0.0;
    if (0 != emcCommandWaitReceived(serial_number)) {
	return -1;
    }
    while (emcTimeout <= 0.0 || end < emcTimeout) {
	updateStatus();
	if (emcStatus->status == RCS_DONE) {
	    return 0;
	}
	if (emcStatus->status == RCS_ERROR) {
	    return -1;
	}
	esleep(EMC_COMMAND_DELAY);
	end += EMC_COMMAND_DELAY;
    }
    return -1;
}

extern "C" int emcPollStatus()
{
  updateStatus();
  return emcStatus->status;
} 
  
extern "C" double convertLinearUnits(double u)
{
    double in_mm;
    /* convert u to mm */
    in_mm = u / emcStatus->motion.traj.linearUnits;
    /* convert u to display units */
    switch (linearUnitConversion) {
    case LINEAR_UNITS_MM:
	return in_mm;
	break;
    case LINEAR_UNITS_INCH:
	return in_mm * INCH_PER_MM;
	break;
    case LINEAR_UNITS_CM:
	return in_mm * CM_PER_MM;
	break;
    case LINEAR_UNITS_AUTO:
	switch (emcStatus->task.programUnits) {
	case CANON_UNITS_MM:
	    return in_mm;
	    break;
	case CANON_UNITS_INCHES:
	    return in_mm * INCH_PER_MM;
	    break;
	case CANON_UNITS_CM:
	    return in_mm * CM_PER_MM;
	    break;
	}
	break;
    case LINEAR_UNITS_CUSTOM:
	return u;
	break;
    }
    return u;
}

extern "C" int emcTaskNmlGet()
{
    int retval = 0;
    // try to connect to EMC cmd
    if (emcCommandBuffer == 0) {
	emcCommandBuffer =
	    new RCS_CMD_CHANNEL(emcFormat, "emcCommand", "xemc",
				EMC_NMLFILE);
	if (!emcCommandBuffer->valid()) {
	    delete emcCommandBuffer;
	    emcCommandBuffer = 0;
	    retval = -1;
	}
    }
    // try to connect to EMC status
    if (emcStatusBuffer == 0) {
	emcStatusBuffer =
	    new RCS_STAT_CHANNEL(emcFormat, "emcStatus", "xemc",
				 EMC_NMLFILE);
	if (!emcStatusBuffer->valid()
	    || EMC_STAT_TYPE != emcStatusBuffer->peek()) {
	    delete emcStatusBuffer;
	    emcStatusBuffer = 0;
	    emcStatus = 0;
	    retval = -1;
	} else {
	    emcStatus = (EMC_STAT *) emcStatusBuffer->get_address();
	}
    }
    return retval;
}

extern "C" int emcErrorNmlGet()
{
    int retval = 0;
    if (emcErrorBuffer == 0) {
	emcErrorBuffer =
	    new NML(nmlErrorFormat, "emcError", "xemc", EMC_NMLFILE);
	if (!emcErrorBuffer->valid()) {
	    delete emcErrorBuffer;
	    emcErrorBuffer = 0;
	    retval = -1;
	}
    }
    return retval;
}

extern "C" int emcNmlInit()
{
    double end;
    int good;
#define RETRY_TIME 10.0		// seconds to wait for subsystems to come up
#define RETRY_INTERVAL 1.0	// seconds between wait tries for a subsystem

    if ((EMC_DEBUG & EMC_DEBUG_NML) == 0) {
	set_rcs_print_destination(RCS_PRINT_TO_NULL);	// inhibit diag messages
    }
    end = RETRY_TIME;
    good = 0;
    do {
	if (0 == emcTaskNmlGet()) {
	    good = 1;
	    break;
	}
	esleep(RETRY_INTERVAL);
	end -= RETRY_INTERVAL;
    } while (end > 0.0);
    if ((EMC_DEBUG & EMC_DEBUG_NML) == 0) {
	set_rcs_print_destination(RCS_PRINT_TO_STDOUT);	// inhibit diag messages
    }
    if (!good) {
	return -1;
    }
    if ((EMC_DEBUG & EMC_DEBUG_NML) == 0) {
	set_rcs_print_destination(RCS_PRINT_TO_NULL);	// inhibit diag messages
    }
    end = RETRY_TIME;
    good = 0;
    do {
	if (0 == emcErrorNmlGet()) {
	    good = 1;
	    break;
	}
	esleep(RETRY_INTERVAL);
	end -= RETRY_INTERVAL;
    } while (end > 0.0);
    if ((EMC_DEBUG & EMC_DEBUG_NML) == 0) {
	set_rcs_print_destination(RCS_PRINT_TO_STDOUT);	// inhibit diag messages
    }
    if (!good) {
	return -1;
    }
    return 0;
#undef RETRY_TIME
#undef RETRY_INTERVAL
}

extern "C" void emcNmlQuit()
{
    EMC_NULL emc_null_msg;
    if (0 != emcStatusBuffer) {
	emcCommandWaitReceived(emcCommandSerialNumber);
    }
    if (0 != emcCommandBuffer) {
	emc_null_msg.serial_number = emcSavedCommandSerialNumber;
	emcCommandBuffer->write(emc_null_msg);
    }
    if (emcErrorBuffer != 0) {
	delete emcErrorBuffer;
	emcErrorBuffer = 0;
    }
    if (emcStatusBuffer != 0) {
	delete emcStatusBuffer;
	emcStatusBuffer = 0;
	emcStatus = 0;
    }
    if (emcCommandBuffer != 0) {
	delete emcCommandBuffer;
	emcCommandBuffer = 0;
    }
}

extern "C" double getDtgPos(int axis)
{
  if (axis == 0) {
    return convertLinearUnits(emcStatus->motion.traj.dtg.tran.x);
  } else 
  if (axis == 1) {
    return convertLinearUnits(emcStatus->motion.traj.dtg.tran.y);
  } else 
  if (axis == 2) {
    return convertLinearUnits(emcStatus->motion.traj.dtg.tran.z);
  } else {
  if (axis == 3) {
    return convertLinearUnits(emcStatus->motion.traj.dtg.a);
  } else 
  if (axis == 4) {
    return convertLinearUnits(emcStatus->motion.traj.dtg.b);
  } else 
  if (axis == 5) {
    return convertLinearUnits(emcStatus->motion.traj.dtg.c);
  } else 
  if (axis == 6) {	
    return convertLinearUnits(emcStatus->motion.traj.dtg.u);
  } else 
  if (axis == 7) {	
    return convertLinearUnits(emcStatus->motion.traj.dtg.v);
  } else 
  if (axis == 8) {	
    return convertLinearUnits(emcStatus->motion.traj.dtg.w);
  } else {
    return 0;
    }
  }
}

extern "C" double getAbsCmdPos(int axis)
{
  if (axis == 0) {
    return convertLinearUnits(emcStatus->motion.traj.position.tran.x);
  } else 
  if (axis == 1) {
    return convertLinearUnits(emcStatus->motion.traj.position.tran.y);
  } else 
  if (axis == 2) {
    return convertLinearUnits(emcStatus->motion.traj.position.tran.z);
  } else {
  if (axis == 3) {
    return convertLinearUnits(emcStatus->motion.traj.position.a);
  } else 
  if (axis == 4) {
    return convertLinearUnits(emcStatus->motion.traj.position.b);
  } else 
  if (axis == 5) {
    return convertLinearUnits(emcStatus->motion.traj.position.c);
  } else 
  if (axis == 6) {	
    return convertLinearUnits(emcStatus->motion.traj.position.u);
  } else 
  if (axis == 7) {	
    return convertLinearUnits(emcStatus->motion.traj.position.v);
  } else 
  if (axis == 8) {	
    return convertLinearUnits(emcStatus->motion.traj.position.w);
  } else {
    return 0;
    }
  }
}

extern "C" double getAbsPos(int axis)
{
  if (axis == 0) {
    return convertLinearUnits(emcStatus->motion.traj.actualPosition.tran.x);
  } else 
  if (axis == 1) {
    return convertLinearUnits(emcStatus->motion.traj.actualPosition.tran.y);
  } else 
  if (axis == 2) {
    return convertLinearUnits(emcStatus->motion.traj.actualPosition.tran.z);
  } else 	    
  if (axis == 3) {	
    return convertLinearUnits(emcStatus->motion.traj.actualPosition.a);
  } else 
  if (axis == 4) {
    return convertLinearUnits(emcStatus->motion.traj.actualPosition.b);
  } else 
  if (axis == 5) {
    return convertLinearUnits(emcStatus->motion.traj.actualPosition.c);
  } else 
  if (axis == 6) {
    return convertLinearUnits(emcStatus->motion.traj.actualPosition.u);
  } else 
  if (axis == 7) {	
    return convertLinearUnits(emcStatus->motion.traj.actualPosition.v);
  } else 
  if (axis == 8) {	
    return convertLinearUnits(emcStatus->motion.traj.actualPosition.w);
  } else {
    return 0;	 
    }
}

extern "C" double getRelCmdPos(int axis)
{
  if (axis == 0) {
    return convertLinearUnits(emcStatus->motion.traj.position.tran.x - 
      emcStatus->task.origin.tran.x);
  } else 
  if (axis == 1) {
    return convertLinearUnits(emcStatus->motion.traj.position.tran.y - 
      emcStatus->task.origin.tran.y);
  } else 
  if (axis == 2) {
    return convertLinearUnits(emcStatus->motion.traj.position.tran.z - 
      emcStatus->task.origin.tran.z - emcStatus->task.toolOffset.tran.z);
  } else 
  if (axis == 3) {	
    return convertLinearUnits(emcStatus->motion.traj.position.a - 
      emcStatus->task.origin.a);
  } else 
  if (axis == 4) {	
    return convertLinearUnits(emcStatus->motion.traj.position.b - 
      emcStatus->task.origin.b);
  } else 
  if (axis == 5) {
    return convertLinearUnits(emcStatus->motion.traj.position.c - 
      emcStatus->task.origin.c);
  } else 
  if (axis == 6) {
    return convertLinearUnits(emcStatus->motion.traj.position.u - 
      emcStatus->task.origin.u);
  } else 
  if (axis == 7) {
    return convertLinearUnits(emcStatus->motion.traj.position.v - 
      emcStatus->task.origin.v);
  } else 
  if (axis == 8) {	
    return convertLinearUnits(emcStatus->motion.traj.position.w - 
      emcStatus->task.origin.w);
  } else {
    return 0;
  }
}

extern "C" double getRelPos(int axis)
{
  if (axis == 0) {
    return convertLinearUnits(emcStatus->motion.traj. actualPosition.tran.x -
      emcStatus->task.origin.tran.x);
  } else 
  if (axis == 1) {
    return convertLinearUnits(emcStatus->motion.traj. actualPosition.tran.y - 
      emcStatus->task.origin.tran.y);
  } else 
  if (axis == 2) {
    return convertLinearUnits(emcStatus->motion.traj. actualPosition.tran.z -
      emcStatus->task.origin.tran.z -emcStatus->task.toolOffset.tran.z);
  } else 
  if (axis == 3) {
    return convertLinearUnits(emcStatus->motion.traj.actualPosition.a - 
      emcStatus->task.origin.a);
  } else 
  if (axis == 4) {
    return convertLinearUnits(emcStatus->motion.traj.actualPosition.b - 
      emcStatus->task.origin.b);
  } else 
  if (axis == 5) {
    return convertLinearUnits(emcStatus->motion.traj.actualPosition.c - 
      emcStatus->task.origin.c);
  } else 
  if (axis == 6) {
    return convertLinearUnits(emcStatus->motion.traj.actualPosition.u - 
      emcStatus->task.origin.u);
  } else 
  if (axis == 7) {
    return convertLinearUnits(emcStatus->motion.traj.actualPosition.v - 
      emcStatus->task.origin.v);
  } else 
  if (axis == 8) {	
    return convertLinearUnits(emcStatus->motion.traj.actualPosition.w - 
      emcStatus->task.origin.w);
  } else {
    return 0;
  }
}

extern "C" double getOrigin(int axis)
{
  if (axis == 0) {
    return convertLinearUnits(emcStatus->task.origin.tran.x);
  } else
  if (axis == 1) {
    return convertLinearUnits(emcStatus->task.origin.tran.y);
  } else
  if (axis == 2) {
    return convertLinearUnits(emcStatus->task.origin.tran.z);
  } else
  if (axis == 3) {
    return convertLinearUnits(emcStatus->task.origin.a);
  } else
  if (axis == 4) {
    return convertLinearUnits(emcStatus->task.origin.b);
  } else
  if (axis == 5) {
    return convertLinearUnits(emcStatus->task.origin.c);
  } else
  if (axis == 6) {
    return convertLinearUnits(emcStatus->task.origin.u);
  } else
  if (axis == 7) {
    return convertLinearUnits(emcStatus->task.origin.v);
  } else
  if (axis == 8) {
    return convertLinearUnits(emcStatus->task.origin.w);
  } else {
    return 0;
  }
}

extern "C" double getJointPos(int axis)
{
  return emcStatus->motion.axis[axis].input;
}

extern "C" int updateError()
{
    NMLTYPE type;

    if (0 == emcErrorBuffer || !emcErrorBuffer->valid()) {
	return -1;
    }

    switch (type = emcErrorBuffer->read()) {
    case -1:
	// error reading channel
	return -1;
	break;

    case 0:
	// nothing new
	break;

    case EMC_OPERATOR_ERROR_TYPE:
	strncpy(errorString,
		((EMC_OPERATOR_ERROR *) (emcErrorBuffer->get_address()))->
		error, LINELEN - 1);
	errorString[NML_ERROR_LEN - 1] = 0;
	break;

    case EMC_OPERATOR_TEXT_TYPE:
	strncpy(operatorTextStr,
		((EMC_OPERATOR_TEXT *) (emcErrorBuffer->get_address()))->
		text, LINELEN - 1);
	operatorTextStr[NML_TEXT_LEN - 1] = 0;
	break;

    case EMC_OPERATOR_DISPLAY_TYPE:
	strncpy(operatorDisplayStr,
		((EMC_OPERATOR_DISPLAY *) (emcErrorBuffer->
					   get_address()))->display,
		LINELEN - 1);
	operatorDisplayStr[NML_DISPLAY_LEN - 1] = 0;
	break;

    case NML_ERROR_TYPE:
	strncpy(errorString,
		((NML_ERROR *) (emcErrorBuffer->get_address()))->error,
		NML_ERROR_LEN - 1);
	errorString[NML_ERROR_LEN - 1] = 0;
	break;

    case NML_TEXT_TYPE:
	strncpy(operatorTextStr,
		((NML_TEXT *) (emcErrorBuffer->get_address()))->text,
		NML_TEXT_LEN - 1);
	operatorTextStr[NML_TEXT_LEN - 1] = 0;
	break;

    case NML_DISPLAY_TYPE:
	strncpy(operatorDisplayStr,
		((NML_DISPLAY *) (emcErrorBuffer->get_address()))->display,
		NML_DISPLAY_LEN - 1);
	operatorDisplayStr[NML_DISPLAY_LEN - 1] = 0;
	break;

    default:
	// if not recognized, set the error string
	sprintf(errorString, "unrecognized error %ld", type);
	return -1;
	break;
    }

    return 0;
}


extern "C" int sendDebug(int level)
{
    EMC_SET_DEBUG debug_msg;
    debug_msg.debug = level;
    debug_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(debug_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendEstop()
{
    EMC_TASK_SET_STATE state_msg;
    state_msg.state = EMC_TASK_STATE_ESTOP;
    state_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(state_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendEstopReset()
{
    EMC_TASK_SET_STATE state_msg;
    state_msg.state = EMC_TASK_STATE_ESTOP_RESET;
    state_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(state_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" bool getEStop()
{
  return (emcStatus->task.state == EMC_TASK_STATE_ESTOP);
}
	
extern "C" int sendMachineOn()
{
    EMC_TASK_SET_STATE state_msg;
    state_msg.state = EMC_TASK_STATE_ON;
    state_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(state_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendMachineOff()
{
    EMC_TASK_SET_STATE state_msg;
    state_msg.state = EMC_TASK_STATE_OFF;
    state_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(state_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" bool getMachineOn()
{
  return (emcStatus->task.state == EMC_TASK_STATE_ON);
}

extern "C" int sendManual()
{
    EMC_TASK_SET_MODE mode_msg;
    mode_msg.mode = EMC_TASK_MODE_MANUAL;
    mode_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(mode_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendAuto()
{
    EMC_TASK_SET_MODE mode_msg;
    mode_msg.mode = EMC_TASK_MODE_AUTO;
    mode_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(mode_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendMdi()
{
    EMC_TASK_SET_MODE mode_msg;
    mode_msg.mode = EMC_TASK_MODE_MDI;
    mode_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(mode_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendOverrideLimits(int axis)
{
    EMC_AXIS_OVERRIDE_LIMITS lim_msg;
    lim_msg.axis = axis;	// neg means off, else on for all
    lim_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(lim_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendJogStop(int axis)
{
    EMC_AXIS_ABORT emc_axis_abort_msg;
    if (axis < 0 || axis >= EMC_AXIS_MAX) {
	return -1;
    }
   emc_axis_abort_msg.serial_number = ++emcCommandSerialNumber;
   emc_axis_abort_msg.axis = axis;
   emcCommandBuffer->write(emc_axis_abort_msg);
   if (emcWaitType == EMC_WAIT_RECEIVED) {
     return emcCommandWaitReceived(emcCommandSerialNumber);
   } else if (emcWaitType == EMC_WAIT_DONE) {
     return emcCommandWaitDone(emcCommandSerialNumber);
   }
  return 0;
}

extern "C" int sendJogCont(int axis, double speed)
{
    EMC_AXIS_JOG emc_axis_jog_msg;
    if (axis < 0 || axis >= EMC_AXIS_MAX) {
	return -1;
    }
    emc_axis_jog_msg.serial_number = ++emcCommandSerialNumber;
    emc_axis_jog_msg.axis = axis;
    emc_axis_jog_msg.vel = speed / 60.0;
    emcCommandBuffer->write(emc_axis_jog_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendJogIncr(int axis, double speed, double incr)
{
    EMC_AXIS_INCR_JOG emc_axis_incr_jog_msg;
    if (axis < 0 || axis >= EMC_AXIS_MAX) {
	return -1;
    }
    emc_axis_incr_jog_msg.serial_number = ++emcCommandSerialNumber;
    emc_axis_incr_jog_msg.axis = axis;
    emc_axis_incr_jog_msg.vel = speed / 60.0;
    emc_axis_incr_jog_msg.incr = incr;
    emcCommandBuffer->write(emc_axis_incr_jog_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendMistOn()
{
    EMC_COOLANT_MIST_ON emc_coolant_mist_on_msg;
    emc_coolant_mist_on_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_coolant_mist_on_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendMistOff()
{
    EMC_COOLANT_MIST_OFF emc_coolant_mist_off_msg;
    emc_coolant_mist_off_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_coolant_mist_off_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}
 
extern "C" int sendFloodOn()
{
    EMC_COOLANT_FLOOD_ON emc_coolant_flood_on_msg;
    emc_coolant_flood_on_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_coolant_flood_on_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendFloodOff()
{
    EMC_COOLANT_FLOOD_OFF emc_coolant_flood_off_msg;
    emc_coolant_flood_off_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_coolant_flood_off_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendLubeOn()
{
    EMC_LUBE_ON emc_lube_on_msg;
    emc_lube_on_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_lube_on_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendLubeOff()
{
    EMC_LUBE_OFF emc_lube_off_msg;
    emc_lube_off_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_lube_off_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendSpindleForward()
{
    EMC_SPINDLE_ON emc_spindle_on_msg;
    if (emcStatus->task.activeSettings[2] != 0) {
	emc_spindle_on_msg.speed = fabs(emcStatus->task.activeSettings[2]);
    } else {
	emc_spindle_on_msg.speed = +emcSpindleDefaultSpeed;
    }
    emc_spindle_on_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_spindle_on_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendSpindleReverse()
{
    EMC_SPINDLE_ON emc_spindle_on_msg;
    if (emcStatus->task.activeSettings[2] != 0) {
	emc_spindle_on_msg.speed =
	    -1 * fabs(emcStatus->task.activeSettings[2]);
    } else {
	emc_spindle_on_msg.speed = -emcSpindleDefaultSpeed;
    }
    emc_spindle_on_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_spindle_on_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendSpindleOff()
{
    EMC_SPINDLE_OFF emc_spindle_off_msg;
    emc_spindle_off_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_spindle_off_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendSpindleIncrease()
{
    EMC_SPINDLE_INCREASE emc_spindle_increase_msg;
    emc_spindle_increase_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_spindle_increase_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendSpindleDecrease()
{
    EMC_SPINDLE_DECREASE emc_spindle_decrease_msg;
    emc_spindle_decrease_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_spindle_decrease_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendSpindleConstant()
{
    EMC_SPINDLE_CONSTANT emc_spindle_constant_msg;
    emc_spindle_constant_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_spindle_constant_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}
    
extern "C" int sendBrakeEngage()
{
    EMC_SPINDLE_BRAKE_ENGAGE emc_spindle_brake_engage_msg;
    emc_spindle_brake_engage_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_spindle_brake_engage_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendBrakeRelease()
{
    EMC_SPINDLE_BRAKE_RELEASE emc_spindle_brake_release_msg;
    emc_spindle_brake_release_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_spindle_brake_release_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendAbort()
{
    EMC_TASK_ABORT task_abort_msg;
    task_abort_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(task_abort_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendHome(int axis)
{
    EMC_AXIS_HOME emc_axis_home_msg;
    emc_axis_home_msg.serial_number = ++emcCommandSerialNumber;
    emc_axis_home_msg.axis = axis;
    emcCommandBuffer->write(emc_axis_home_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendUnHome(int axis)
{
    EMC_AXIS_UNHOME emc_axis_home_msg;
    emc_axis_home_msg.serial_number = ++emcCommandSerialNumber;
    emc_axis_home_msg.axis = axis;
    emcCommandBuffer->write(emc_axis_home_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendFeedOverride(double override)
{
    EMC_TRAJ_SET_SCALE emc_traj_set_scale_msg;
    if (override < 0.0) {
      override = 0.0;
    }
    emc_traj_set_scale_msg.serial_number = ++emcCommandSerialNumber;
    emc_traj_set_scale_msg.scale = override;
    emcCommandBuffer->write(emc_traj_set_scale_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendMaxVelocity(double velocity)
{
    EMC_TRAJ_SET_MAX_VELOCITY mv;
    if (velocity < 0.0) {
        velocity = 0.0;
    }
    mv.serial_number = ++emcCommandSerialNumber;
    mv.velocity = velocity;
    emcCommandBuffer->write(mv);
    return emcCommandWaitReceived(emcCommandSerialNumber);
}


extern "C" int sendSpindleOverride(double override)
{
    EMC_TRAJ_SET_SPINDLE_SCALE emc_traj_set_spindle_scale_msg;
    if (override < 0.0) {
      override = 0.0;
    }
    emc_traj_set_spindle_scale_msg.serial_number = ++emcCommandSerialNumber;
    emc_traj_set_spindle_scale_msg.scale = override;
    emcCommandBuffer->write(emc_traj_set_spindle_scale_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendTaskPlanInit()
{
    EMC_TASK_PLAN_INIT task_plan_init_msg;
    task_plan_init_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(task_plan_init_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendProgramOpen(char *program)
{
    EMC_TASK_PLAN_OPEN emc_task_plan_open_msg;
    emc_task_plan_open_msg.serial_number = ++emcCommandSerialNumber;
    strcpy(emc_task_plan_open_msg.file, program);
    emcCommandBuffer->write(emc_task_plan_open_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendProgramRun(int line)
{
    EMC_TASK_PLAN_RUN emc_task_plan_run_msg;
    emc_task_plan_run_msg.serial_number = ++emcCommandSerialNumber;
    emc_task_plan_run_msg.line = line;
    emcCommandBuffer->write(emc_task_plan_run_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendProgramPause()
{
    EMC_TASK_PLAN_PAUSE emc_task_plan_pause_msg;
    emc_task_plan_pause_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_task_plan_pause_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendProgramResume()
{
    EMC_TASK_PLAN_RESUME emc_task_plan_resume_msg;
    emc_task_plan_resume_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_task_plan_resume_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendSetOptionalStop(bool state)
{
    EMC_TASK_PLAN_SET_OPTIONAL_STOP emc_task_plan_set_optional_stop_msg;
    emc_task_plan_set_optional_stop_msg.state = state;
    emc_task_plan_set_optional_stop_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_task_plan_set_optional_stop_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}


extern "C" int sendSetBlockDelete(bool state)
{
  EMC_TASK_PLAN_SET_BLOCK_DELETE m;
  m.state = state;
  m.serial_number = ++emcCommandSerialNumber;
  emcCommandBuffer->write(m);
  return emcCommandWaitReceived(emcCommandSerialNumber);
}

extern "C" int sendProgramStep()
{
    EMC_TASK_PLAN_STEP emc_task_plan_step_msg;
    emc_task_plan_step_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_task_plan_step_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendMdiCmd(const char *mdi)
{
    EMC_TASK_PLAN_EXECUTE emc_task_plan_execute_msg;
    strcpy(emc_task_plan_execute_msg.command, mdi);
    emc_task_plan_execute_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_task_plan_execute_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendLoadToolTable(const char *file)
{
    EMC_TOOL_LOAD_TOOL_TABLE emc_tool_load_tool_table_msg;
    strcpy(emc_tool_load_tool_table_msg.file, file);
    emc_tool_load_tool_table_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_tool_load_tool_table_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendToolSetOffset(int id, double zoffset, double diameter)
{
    EMC_TOOL_SET_OFFSET emc_tool_set_offset_msg;
    emc_tool_set_offset_msg.id = id;
    emc_tool_set_offset_msg.zoffset = zoffset;
    emc_tool_set_offset_msg.diameter = diameter;
    emc_tool_set_offset_msg.orientation = 0; // mill style tool table
    emc_tool_set_offset_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_tool_set_offset_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendToolSetOffset2(int id, double zoffset, double xoffset, 
                      double diameter, double frontangle, double backangle,
                      int orientation)
{
    EMC_TOOL_SET_OFFSET emc_tool_set_offset_msg;
    emc_tool_set_offset_msg.id = id;                  
    emc_tool_set_offset_msg.zoffset = zoffset;        
    emc_tool_set_offset_msg.xoffset = xoffset;        
    emc_tool_set_offset_msg.diameter = diameter;      
    emc_tool_set_offset_msg.frontangle = frontangle;  
    emc_tool_set_offset_msg.backangle = backangle;    
    emc_tool_set_offset_msg.orientation = orientation;
    emc_tool_set_offset_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_tool_set_offset_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendAxisSetBacklash(int axis, double backlash)
{
    EMC_AXIS_SET_BACKLASH emc_axis_set_backlash_msg;
    emc_axis_set_backlash_msg.axis = axis;
    emc_axis_set_backlash_msg.backlash = backlash;
    emc_axis_set_backlash_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_axis_set_backlash_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendAxisEnable(int axis, int val)
{
    EMC_AXIS_ENABLE emc_axis_enable_msg;
    EMC_AXIS_DISABLE emc_axis_disable_msg;

    if (val) {
	emc_axis_enable_msg.axis = axis;
	emc_axis_enable_msg.serial_number = ++emcCommandSerialNumber;
	emcCommandBuffer->write(emc_axis_enable_msg);
    } else {
	emc_axis_disable_msg.axis = axis;
	emc_axis_disable_msg.serial_number = ++emcCommandSerialNumber;
	emcCommandBuffer->write(emc_axis_disable_msg);
    }
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }

    return 0;
}

extern "C" int sendAxisLoadComp(int axis, const char *file, int type)
{
    EMC_AXIS_LOAD_COMP emc_axis_load_comp_msg;
    strcpy(emc_axis_load_comp_msg.file, file);
    emc_axis_load_comp_msg.type = type;
    emc_axis_load_comp_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_axis_load_comp_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendClearProbeTrippedFlag()
{
    EMC_TRAJ_CLEAR_PROBE_TRIPPED_FLAG emc_clear_probe_tripped_flag_msg;

    emc_clear_probe_tripped_flag_msg.serial_number =
	++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_clear_probe_tripped_flag_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" int sendProbe(double x, double y, double z)
{
    EMC_TRAJ_PROBE emc_probe_msg;
    emc_probe_msg.pos.tran.x = x;
    emc_probe_msg.pos.tran.y = y;
    emc_probe_msg.pos.tran.z = z;
    emc_probe_msg.serial_number = ++emcCommandSerialNumber;
    emcCommandBuffer->write(emc_probe_msg);
    if (emcWaitType == EMC_WAIT_RECEIVED) {
	return emcCommandWaitReceived(emcCommandSerialNumber);
    } else if (emcWaitType == EMC_WAIT_DONE) {
	return emcCommandWaitDone(emcCommandSerialNumber);
    }
    return 0;
}

extern "C" bool iniGet(char *varstr, char *secstr, char *buffer)
{
  const char *inistring;
  if (NULL != (inistring = inifile.Find(secstr,varstr))) {
     strcpy(buffer, inistring);
     return true;
  }
  return false;
}

extern "C" bool iniClose()
{
  inifile.Close();
  return true;
}

extern "C" bool iniOpen(const char *filename)
{
  return inifile.Open(filename);
}



