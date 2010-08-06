unit emc2pas;

{$I mocca.inc}

{$mode objfpc}

{$link emcpas.o}
{$linklib libemc.a}
{$linklib libnml.so}
{$linklib c}

interface

uses
  emcint;

const
  LINELEN = 255;
  BUFFERLEN = 80;
  MDI_LINELEN = 80;
  MM_PER_INCH = 25.4;
  INCH_PER_MM = (1.0/25.4);

  CANON_TOOL_MAX = 56;	// from canon.hh
  CANON_TOOL_ENTRY_LEN = 256; // from canon.hh

  CANON_UNITS_INCH = 1;
  CANON_UNITS_MM = 2;


type
  TTool = packed record
    toolno: integer;
    xoffset: double;
    {$ifdef VER_24}
    yoffset: double;
    {$endif} 
    zoffset: double;
    {$ifdef VER_24}
    aoffset: double;
    boffset: double;
    coffset: double;
    uoffset: double;
    voffset: double;
    woffset: double;
    {$endif}
    diameter: double;
    frontangle: double;
    backangle: double;
    orientation: integer;
  end;

const
  EmptyTool : TTool =
    (
    toolno: -1; xoffset: 0;
    {$ifdef VER_24} yoffset: 0; {$endif}
    zoffset: 0;
    {$ifdef VER_24}
    aoffset: 0; boffset: 0; coffset: 0; uoffset: 0;voffset: 0; woffset: 0;
    {$endif}
    diameter: 0;
    frontangle: 0; backangle: 0;
    orientation: 0;
    );


type
  TTools = array[0..CANON_TOOL_MAX + 1] of TTool;

const
  TaskModeManual = 1;
  TaskModeAuto   = 2;
  TaskModeMDI    = 3;
  
  EMC_WAIT_NONE     = 1;
  EMC_WAIT_RECEIVED = 2;
  EMC_WAIT_DONE     = 3;

  RCS_DONE = 1;
  RCS_EXEC = 2;
  RCS_ERROR = 3;

  INTERP_IDLE    = 1;
  INTERP_READING = 2;
  INTERP_PAUSED  = 3;
  INTERP_WAITING = 4;

  EMC_TASK_EXEC_ERROR = 1;
  EMC_TASK_EXEC_DONE = 2;
  EMC_TASK_EXEC_WAITING_FOR_MOTION = 3;
  EMC_TASK_EXEC_WAITING_FOR_MOTION_QUEUE = 4;
  EMC_TASK_EXEC_WAITING_FOR_IO = 5;
  EMC_TASK_EXEC_WAITING_FOR_PAUSE = 6;
  EMC_TASK_EXEC_WAITING_FOR_MOTION_AND_IO = 7;
  EMC_TASK_EXEC_WAITING_FOR_DELAY = 8;
  EMC_TASK_EXEC_WAITING_FOR_SYSTEM_CMD = 9;
  
var
  ErrorStr: Array[0..LINELEN-1] of Char; external name 'errorString';
  EMC_NMLFILE: Array[0..LINELEN-1] of Char; external name 'EMC_NMLFILE';
  
  OperatorTextStr: Array[0..LINELEN-1] of Char; external name 'operatorTextStr';
  OperatorDisplayStr: Array[0..LINELEN-1] of Char; external name 'operatorDisplayStr';

  EmcUpdateType: Byte; external name 'emcUpdateType';  	// enum = byte ???
  EmcTimeOut: Double; external name 'emcTimeout';
  EmcSpindleDefaultSpeed: Integer; external name 'emcSpindleDefaultSpeed';
  emcCommandSerialNumber: integer; external name 'emcCommandSerialNumber';

  ActiveGCodes: Array[0..MDI_LINELEN-1] of Char; external name 'activeGCodes';
  ActiveMCodes: Array[0..MDI_LINELEN-1] of Char; external name 'activeMCodes';
  ActiveFWords: Array[0..MDI_LINELEN-1] of Char; external name 'activeFWords';
  ActiveSWords: Array[0..MDI_LINELEN-1] of Char; external name 'activeSWords';

  {$ifdef VER_24}
  RandomToolchanger: integer; external name 'random_toolchanger';
  {$endif}

  Tools: TTools; external name 'toolTable';
  ToolComments: Array[0..CANON_TOOL_MAX] of PChar; external name 'ttcomments';

// Toolfile
function  getnumtools: integer; cdecl; external;
function  SetCanonTool(tool: integer): boolean; cdecl; external;

procedure InitToolTable; cdecl; external;
procedure DoneToolTable; cdecl; external;
function  loadToolTable(const filename: PChar): integer; cdecl; external;
function  saveToolTable(const filename: PChar): integer; cdecl; external;

function  getDtgPos(axis: integer): double; cdecl; external;
function  getAbsCmdPos(axis: integer): double; cdecl; external;
function  getAbsPos(axis: integer): double; cdecl; external;
function  getRelCmdPos(axis: integer): double; cdecl; external;
function  getRelPos(axis: integer): double; cdecl; external;
function  getJointPos(joint: integer): double; cdecl; external;
function  getOrigin(axis: integer): double; cdecl; external;
function  getLoggerPos(axis: integer): double; cdecl; external;

// axis related functions
function AxisAxisType(Joint: integer): integer cdecl; external; { motion.axis.*.axisType; }
function AxisUnits(Joint: integer): Double; cdecl; external; { motion.axis.*.units; }
function AxisBacklash(Joint: integer): Double; cdecl; external; { motion.axis.*.backlash; }
function AxisMinPositionLimit(Joint: integer): Double; cdecl; external; { motion.axis.*.minPositionLimit; }
function AxisMaxPositionLimit(Joint: integer): Double; cdecl; external; { motion.axis.*.maxPositionLimit; }
function AxisVelocity(Joint: integer): Double; cdecl; external; { motion.axis.*.velocity; }
function AxisHoming(Joint: integer): Boolean; cdecl; external; { motion.axis.*.homing; }
function AxisHomed(Joint: integer): Boolean; cdecl; external; { motion.axis.*.homed; }
function AxisEnabled(Joint: integer): Boolean; cdecl; external; { motion.axis.*.enabled; }
function AxisFault(Joint: integer): Boolean; cdecl; external; { motion.axis.*.fault; }
function AxisMinSoftLimit(Joint: integer): Double; cdecl; external; { motion.axis.*.minSoftLimit; }
function AxisMaxSoftLimit(Joint: integer): Double; cdecl; external; { motion.axis.*.maxSoftLimit; }
function AxisMinHardLimit(Joint: integer): Double; cdecl; external; { motion.axis.*.minHardLimit; }
function AxisMaxHardLimit(Joint: integer): Double; cdecl; external; { motion.axis.*.maxHardLimit; }
function AxisOverrideLimits(Joint: integer): Boolean; cdecl; external; { motion.axis.*.overrideLimits; }

// traj related functions

function trajAxes: integer; cdecl; external; { return emcStatus->motion.traj.axes; }
function trajAxisMask: integer; cdecl; external; { return emcStatus->motion.traj.axis_mask; }
function trajScale: double;  cdecl; external; { motion.traj.scale}
function trajlinearUnits: Double;  cdecl; external; { motion.traj.linearUnits}
function trajangularUnits: Double; cdecl; external; { motion.traj.angularUnits}
function trajMode: integer; cdecl; external;{ motion.traj.mode}
function trajSpindleScale: Double; cdecl; external; { motion.traj.spindle_scale}
function trajVel: Double; cdecl; external; { motion.traj.velocity}
function trajAcceleration: Double; cdecl; external; { motion.traj.acceleration}
function trajMaxVel: Double; cdecl; external; { motion.traj.maxVelocity}
function trajDtg: Double; cdecl; external; { motion.traj.distance_to_go}
function trajCurrentVel: Double; cdecl; external; { motion.traj.current_vel}
function trajFeedORideEnabled: Boolean;  cdecl; external; { motion.traj.feed_override_enabled}
function trajSpindleORideEnabled: Boolean;   cdecl; external;  { motion.traj.spindle_override_enabled}
function trajAdaptiveFeedEnabled: Boolean;   cdecl; external; { motion.traj.adaptive_feed_enabled}
function trajFeedHoldEnabled: Boolean;   cdecl; external; { motion.traj.feed_hold_enabled}
function trajProbing: Boolean;   cdecl; external; { motion.traj.probing}
function trajProbeTripped: Boolean;   cdecl; external; { motion.traj.probe_tripped}

// task related functions
function taskMode: integer; cdecl; external; { task.mode; }
function taskState: integer; cdecl; external; { task.state; }
function taskExecState: integer; cdecl; external; { task.execstate; }
function taskInterpState: integer; cdecl; external; { task.interpState; }
function taskMotionline: integer; cdecl; external; { task.motionLine; }
function taskCurrentLine: integer; cdecl; external; { task.currentLine; }
function taskReadLine: integer; cdecl; external; { task.readLine; }
function taskRotationXY: double; cdecl; external; { task.rotation_xy; }
{$ifdef VER_23}
function taskTloIsAlongW: boolean; cdecl; external; { task.tloIsAlongW; }
{$endif}
function taskProgramUnits: integer; cdecl; external; { task.programUnits; }
function taskInterpErrorCode: integer; cdecl; external; { task.interpreter_errcode; }
function taskDelayLeft: double; cdecl; external; { task.delayLeft; }
function taskBlockDelete: boolean; cdecl; external; { task.block_delete_state; }
function taskOptStop: boolean; cdecl; external; { task.optional_stop_state; }
function taskGetFile(ProgFile: PChar): boolean; cdecl; external;
// read and update aciveGCodes,MCodes,FWords,SWords
procedure taskActiveCodes; cdecl; external;

// spindlestat read functions
function spindleSpeed: double; cdecl; external; { motion.spindle.speed; }
function spindleDirection: integer; cdecl; external; { motion.spindle.direction; }
function spindleBrake: integer; cdecl; external; { motion.spindle.brake; }
function spindleIncreasing: integer; cdecl; external; { motion.spindle.increasing; }
function spindleEnabled: integer; cdecl; external; { motion.spindle.enabled; }

// toolstat read functions
function toolPrepped: integer; cdecl; external; { io.tool.toolPrepped; }
function toolInSpindle: integer; cdecl; external; { io.tool.toolInSpindle; }
function toolLengthOffset: double; cdecl; external; { emcStatus->task.toolOffset.tran.z }

// coolantstat read functions
function coolantMist: boolean; cdecl; external; { io.coolant.mist != 0; }
function coolantFlood: boolean; cdecl; external; { io.coolant.flood != 0; }

// Lubestat read functions
function lubeOn: boolean; cdecl; external; { io.lube.on != 0; }
function lubeLevel: integer; cdecl; external; { io.lube.level; }

function  geterror(const msg: PChar): Boolean; cdecl; external;

// ini- related functions C++ wrapper
function  iniOpen(filename: PChar): boolean; cdecl; external libemcini;
function  iniClose: boolean; cdecl; external libemcini;
function  iniGet(const gvar,gsec,buffer: PChar): boolean; cdecl; external libemcini;

function  emcTaskNmlGet: Longint; cdecl; external;
function  emcErrorNmlGet: Longint; cdecl; external;

function  emcNmlInit: Longint; cdecl; external;
procedure emcNmlQuit; cdecl; external;

function  getFeedOverride: Longint; cdecl; external;
function  getEStop: Boolean; cdecl; external;
function  getMachineOn: Boolean; cdecl; external;
function  getTaskMode: Longint; cdecl; external;
function  getMistOn: Boolean; cdecl; external;
function  getFloodOn: Boolean; cdecl; external;
function  getLubeOn: Boolean; cdecl; external;
function  getSpindle: Longint; cdecl; external;
function  getBrakeOn: Boolean; cdecl; external;

function  updateStatus: Longint; cdecl; external;
function  updateError: Longint; cdecl; external;

function  emcCommandWaitReceived(Serialnumber: integer): Longint; cdecl; external;
function  emcCommandWaitDone(Serialnumber: integer): Longint; cdecl; external;
function  emcPollStatus: integer; cdecl; external;

// function  convertLinearUnits(u: Double): Double; cdecl; external;

function  sendDebug(level: integer): longint; cdecl; external;

function  sendEstop: Longint; cdecl; external;
function  sendEstopReset: Longint; cdecl; external;

function  sendMachineOn: Longint; cdecl; external;
function  sendMachineOff: Longint; cdecl; external;

function  sendManual: Longint; cdecl; external;
function  sendAuto: Longint; cdecl; external;
function  sendMdi: Longint; cdecl; external;

function  sendOverrideLimits(axis: integer): Longint; cdecl; external;

function  sendJogStop(axis: integer): longint; cdecl; external;
function  sendJogCont(axis: integer; Speed: Double): longint; cdecl; external;
function  sendJogIncr(axis: integer; Speed,Increment: Double): longint; cdecl; external;

function  sendMistOn: Longint; cdecl; external;
function  sendMistOff: Longint; cdecl; external;

function  sendFloodOn: Longint; cdecl; external;
function  sendFloodOff: Longint; cdecl; external;

function  sendLubeOn: Longint; cdecl; external;
function  sendLubeOff: Longint; cdecl; external;

function  sendSpindleForward: Longint; cdecl; external;
function  sendSpindleReverse: Longint; cdecl; external;
function  sendSpindleOff: Longint; cdecl; external;
function  sendSpindleIncrease: Longint; cdecl; external;
function  sendSpindleDecrease: Longint; cdecl; external;
function  sendSpindleConstant: Longint; cdecl; external;

function  sendBrakeEngage: Longint; cdecl; external;
function  sendBrakeRelease: Longint; cdecl; external;

function  sendAbort: Longint; cdecl; external;

function  sendHome(axis: Longint): Longint; cdecl; external;
function  sendUnHome(axis: Longint): Longint; cdecl; external;

function  sendFeedOverride(Value: Double): Longint; cdecl; external;
function  sendMaxVelocity(Velocity: Double): Longint; cdecl; external;

function  sendSpindleOverride(Value: Double): Longint; cdecl; external;

function  sendTaskPlanInit: Longint; cdecl; external;

function  sendProgramOpen(const Prog: PChar): Longint; cdecl; external;
function  sendProgramRun(Line: integer): Longint; cdecl; external;
function  sendProgramPause: Longint; cdecl; external;
function  sendProgramResume: Longint; cdecl; external;
function  sendProgramStep: Longint; cdecl; external;

function  sendSetOptionalStop(State: Boolean): Longint; cdecl; external;
function  sendSetBlockDelete(State: Boolean): Longint; cdecl; external;

function  sendMdiCmd(const cmd: PChar): Longint; cdecl; external;

function  sendLoadToolTable(const filename: PChar): Longint; cdecl; external;
function  sendToolSetOffset(tool: integer; length,diameter: double): Longint; cdecl; external;
function  sendToolSetOffset2(id: integer; zoffset,xoffset,diameter,frontangle,
  backangle: double; orientation: integer): Longint; cdecl; external;

function  sendAxisSetBacklash(axis: integer; backlash: double): Longint; cdecl; external;
function  sendAxisSetOutput(axis: integer; output: double): Longint; cdecl; external;
function  sendAxisEnable(axis,value: integer): Longint; cdecl; external;
function  sendAxisLoadComp(axis: integer; const filename: PChar; t: integer): Longint; cdecl; external;

function  sendProbe(x,y,z: double): Longint; cdecl; external;
function  sendClearProbeTrippedFlag: Longint; cdecl; external;

implementation

initialization

{$ifdef VER_24}
RandomToolchanger:= 0;
{$endif}

end.
