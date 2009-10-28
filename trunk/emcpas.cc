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
extern LINEAR_UNIT_CONVERSION linearUnitConversion;

enum ANGULAR_UNIT_CONVERSION {
    ANGULAR_UNITS_CUSTOM = 1,
    ANGULAR_UNITS_AUTO,
    ANGULAR_UNITS_DEG,
    ANGULAR_UNITS_RAD,
    ANGULAR_UNITS_GRAD
};
extern ANGULAR_UNIT_CONVERSION angularUnitConversion;

enum EMC_WAIT_TYPE {
    EMC_WAIT_NONE = 1,
    EMC_WAIT_RECEIVED,
    EMC_WAIT_DONE
};

extern EMC_WAIT_TYPE emcWaitType;

LINEAR_UNIT_CONVERSION linearUnitConversion ;
ANGULAR_UNIT_CONVERSION angularUnitConversion ;

static int emcCommandSerialNumber;
static int saveEmcCommandSerialNumber;

// the NML channels to the EMC task
static RCS_CMD_CHANNEL *emcCommandBuffer;
static RCS_STAT_CHANNEL *emcStatusBuffer;

EMC_STAT *emcStatus;

// the NML channel for errors
static NML *emcErrorBuffer;
static IniFile inifile;

char error_string[NML_ERROR_LEN];
char operator_text_string[NML_TEXT_LEN];
char operator_display_string[NML_DISPLAY_LEN];
char active_g_codes_string[MDI_LINELEN] = "";
char active_m_codes_string[MDI_LINELEN] = "";

// char defaultPath[80] = DEFAULT_PATH;
double emcTimeout;
int programStartLine;

EMC_WAIT_TYPE emcWaitType;

// polarities for axis jogging, from ini file
int jogPol[EMC_AXIS_MAX];

    
extern "C" int getFeedOverride()
{
  return (int) (emcStatus->motion.traj.scale * 100.0 + 0.5);
}


extern "C" void getActiveGCodes()
{
  int t;
  int code; 
  char string[256];  
  active_g_codes_string[0] = 0;
  for (t = 1; t < ACTIVE_G_CODES; t++) {
    code = emcStatus->task.activeGCodes[t];
    if (code == -1) {
      continue;
    }
    if (code % 10) {
      sprintf(string, "G%.1f ", (double) code / 10.0);
    }
    else {
      sprintf(string, "G%d ", code / 10);
    }
    strcat(active_g_codes_string, string);
  }
}

extern "C" void getActiveMCodes()
{
  int t;
  int code; 
  char string[256];   
  active_m_codes_string[0] = 0;
  for (t = 1; t < ACTIVE_M_CODES; t++) {
    code = emcStatus->task.activeMCodes[t];
    if (code == -1) {
      continue;
    }
    sprintf(string, "M%d ", code);
    strcat(active_m_codes_string, string);
  }
}

extern "C" bool geterror(char *msg)
{
  if (error_string[0] != 0) {
    strcpy(msg,error_string);
    error_string[0] = 0;
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
    case -1:
	// error on CMS channel
	return -1;
	break;
    case 0:			// no new data
    case EMC_STAT_TYPE:	// new data
	// new data
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
	emc_null_msg.serial_number = saveEmcCommandSerialNumber;
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
    return convertLinearUnits(emcStatus->motion.traj. actualPosition.tran. x);
  } else 
  if (axis == 1) {
    return convertLinearUnits(emcStatus->motion.traj.actualPosition.tran. y);
  } else 
  if (axis == 2) {
    return convertLinearUnits(emcStatus->motion.traj.actualPosition.tran. z);
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
	strncpy(error_string,
		((EMC_OPERATOR_ERROR *) (emcErrorBuffer->get_address()))->
		error, LINELEN - 1);
	error_string[NML_ERROR_LEN - 1] = 0;
	break;

    case EMC_OPERATOR_TEXT_TYPE:
	strncpy(operator_text_string,
		((EMC_OPERATOR_TEXT *) (emcErrorBuffer->get_address()))->
		text, LINELEN - 1);
	operator_text_string[NML_TEXT_LEN - 1] = 0;
	break;

    case EMC_OPERATOR_DISPLAY_TYPE:
	strncpy(operator_display_string,
		((EMC_OPERATOR_DISPLAY *) (emcErrorBuffer->
					   get_address()))->display,
		LINELEN - 1);
	operator_display_string[NML_DISPLAY_LEN - 1] = 0;
	break;

    case NML_ERROR_TYPE:
	strncpy(error_string,
		((NML_ERROR *) (emcErrorBuffer->get_address()))->error,
		NML_ERROR_LEN - 1);
	error_string[NML_ERROR_LEN - 1] = 0;
	break;

    case NML_TEXT_TYPE:
	strncpy(operator_text_string,
		((NML_TEXT *) (emcErrorBuffer->get_address()))->text,
		NML_TEXT_LEN - 1);
	operator_text_string[NML_TEXT_LEN - 1] = 0;
	break;

    case NML_DISPLAY_TYPE:
	strncpy(operator_display_string,
		((NML_DISPLAY *) (emcErrorBuffer->get_address()))->display,
		NML_DISPLAY_LEN - 1);
	operator_display_string[NML_DISPLAY_LEN - 1] = 0;
	break;

    default:
	// if not recognized, set the error string
	sprintf(error_string, "unrecognized error %ld", type);
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

extern "C" int getTaskMode()
{
  return emcStatus->task.mode;
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

static int axisJogging = -1;

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
  axisJogging = -1;
  return 0;
}

extern "C" int sendJogCont(int axis, double speed)
{
    EMC_AXIS_JOG emc_axis_jog_msg;
    if (axis < 0 || axis >= EMC_AXIS_MAX) {
	return -1;
    }
    if (0 == jogPol[axis]) {
      speed = -speed;
    }
    emc_axis_jog_msg.serial_number = ++emcCommandSerialNumber;
    emc_axis_jog_msg.axis = axis;
    emc_axis_jog_msg.vel = speed / 60.0;
    emcCommandBuffer->write(emc_axis_jog_msg);
    axisJogging = axis;
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
    if (0 == jogPol[axis]) {
	speed = -speed;
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
    axisJogging = -1;
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

extern "C" int getMistOnOff()
{
  if (emcStatus->io.coolant.mist == 1) {
    return 1;
  } else {
    return 0;
  }
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

extern "C" bool getFloodOn()
{
  return (emcStatus->io.coolant.flood == 1);
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

extern "C" bool getLubeOn()
{
  return emcStatus->io.lube.on;
} 

extern "C" int sendSpindleForward()
{
    EMC_SPINDLE_ON emc_spindle_on_msg;
    if (emcStatus->task.activeSettings[2] != 0) {
	emc_spindle_on_msg.speed = fabs(emcStatus->task.activeSettings[2]);
    } else {
	emc_spindle_on_msg.speed = +500;
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
	emc_spindle_on_msg.speed = -500;
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

extern "C" int getSpindle()
{
  int retval;
  retval = emcStatus->motion.spindle.increasing;
  if (retval == 0) {
    retval = emcStatus->motion.spindle.direction;
  }
  return retval;
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

extern "C" bool getBrakeOn()
{
  return emcStatus->motion.spindle.brake;
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

extern "C" bool getJointHomed(int joint)
{
  return emcStatus->motion.axis[joint].homed;
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

// saved value of last program opened
static char lastProgramFile[LINELEN] = "";

extern "C" int sendProgramOpen(char *program)
{
    EMC_TASK_PLAN_OPEN emc_task_plan_open_msg;
    // save this to run again
    strcpy(lastProgramFile, program);
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
    if (0 == emcStatus->task.file[0]) {
	sendProgramOpen(lastProgramFile);
    }
    programStartLine = line;
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


extern "C" int sendProgramStep()
{
    EMC_TASK_PLAN_STEP emc_task_plan_step_msg;
    // clear out start line, if we had a verify before it would be -1
    programStartLine = 0;
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

extern "C" int iniGet(char *varstr, char *secstr, char *buffer)
{
  const char *inistring;
  if (NULL != (inistring = inifile.Find(secstr,varstr))) {
     strcpy(buffer, inistring);
     return 0;
  }
  return 1;
}

extern "C" int iniClose()
{
  inifile.Close();
  return 0;
}

extern "C" int iniOpen(const char *filename)
{
    const char *inistring;
    char displayString[LINELEN] = "";
    int t;
    int i;

    // open it
    if (inifile.Open(filename) == false) {
	return -1;
    }

    if (NULL != (inistring = inifile.Find("DEBUG", "EMC"))) {
	// copy to global
	if (1 != sscanf(inistring, "%i", &EMC_DEBUG)) {
	    EMC_DEBUG = 0;
	}
    } else {
	// not found, use default
	EMC_DEBUG = 0;
    }

    if (NULL != (inistring = inifile.Find("NML_FILE", "EMC"))) {
	// copy to global
	strcpy(EMC_NMLFILE, inistring);
    } else {
	// not found, use default
    }

    for (t = 0; t < EMC_AXIS_MAX; t++) {
	jogPol[t] = 1;		// set to default
	sprintf(displayString, "AXIS_%d", t);
	if (NULL != (inistring =
		     inifile.Find("JOGGING_POLARITY", displayString)) &&
	    1 == sscanf(inistring, "%d", &i) && i == 0) {
	    // it read as 0, so override default
	    jogPol[t] = 0;
	}
    }

    if (NULL != (inistring = inifile.Find("LINEAR_UNITS", "DISPLAY"))) {
	if (!strcmp(inistring, "AUTO")) {
	    linearUnitConversion = LINEAR_UNITS_AUTO;
	} else if (!strcmp(inistring, "INCH")) {
	    linearUnitConversion = LINEAR_UNITS_INCH;
	} else if (!strcmp(inistring, "MM")) {
	    linearUnitConversion = LINEAR_UNITS_MM;
	} else if (!strcmp(inistring, "CM")) {
	    linearUnitConversion = LINEAR_UNITS_CM;
	}
    } else {
	// not found, leave default alone
    }

    if (NULL != (inistring = inifile.Find("ANGULAR_UNITS", "DISPLAY"))) {
	if (!strcmp(inistring, "AUTO")) {
	    angularUnitConversion = ANGULAR_UNITS_AUTO;
	} else if (!strcmp(inistring, "DEG")) {
	    angularUnitConversion = ANGULAR_UNITS_DEG;
	} else if (!strcmp(inistring, "RAD")) {
	    angularUnitConversion = ANGULAR_UNITS_RAD;
	} else if (!strcmp(inistring, "GRAD")) {
	    angularUnitConversion = ANGULAR_UNITS_GRAD;
	}
    } else {
	// not found, leave default alone
    }

    // close it
    return 0;
}



