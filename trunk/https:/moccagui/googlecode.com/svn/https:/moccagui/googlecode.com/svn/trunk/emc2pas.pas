unit emc2pas;

{$link emcpas.o}
{$linklib libemcini.so}
{$linklib libnml.so}
{$link /home/gtom/emc2-dev/lib/libemc.a}
{$linklib c}

interface

uses CTypes;

{$H+}

const
  LINELEN = 255;
  BUFFERLEN = 80;
  MDI_LINELEN = 80;
  MM_PER_INCH = 25.4;
  INCH_PER_MM = (1.0/25.4);

var
  ErrorStr: Array[0..LINELEN-1] of Char; external name 'error_string';
  OperatorTextStr: Array[0..LINELEN-1] of Char; external name 'operator_text_string';
  OperatorDisplayStr: Array[0..LINELEN-1] of Char; external name 'operator_display_string';
  DefaultPath: Array[0..80-1] of Char; external name 'defaultPath';
  ProgramStartLine: LongInt; external name 'programStartLine';
  EmcUpdateType: byte; external name 'emcUpdateType';  	// enum = byte ???
  EmcWaitType: byte; external name 'emcWaitType';	// enum = byte ???
  EmcTimeOut: Double; external name 'emcTimeout';
  ActiveGCodes: Array[0..MDI_LINELEN-1] of Char; external name 'active_g_codes_string';
  ActiveMCodes: Array[0..MDI_LINELEN-1] of Char; external name 'active_m_codes_string';


procedure getActiveGCodes(); cdecl; external;
procedure getActiveMCodes(); cdecl; external;
function  geterror(const msg: PChar): Boolean; cdecl; external;

function  iniOpen(const filename: PChar): Longint; cdecl; external;
function  iniClose: Longint; cdecl; external;
function  iniGet(const gvar,gsec,buffer: PChar): longint; cdecl; external;

function  emcTaskNmlGet: Longint; cdecl; external;
function  emcErrorNmlGet: Longint; cdecl; external;

function  emcNmlInit: Longint; cdecl; external;
procedure emcNmlQuit; cdecl; external;

function  getJointPos(joint: integer): Double; cdecl; external;
function  getJointHomed(joint: integer): Boolean; cdecl; external;

function  getDtgPos(axis: integer): double; cdecl; external;
function  getAbsCmdPos(axis: integer): double; cdecl; external;
function  getAbsPos(axis: integer): double; cdecl; external;
function  getRelCmdPos(axis: integer): double; cdecl; external;
function  getRelPos(axis: integer): double; cdecl; external;

function  updateStatus: Longint; cdecl; external;
function  updateError: Longint; cdecl; external;

function  emcCommandWaitReceived(Serialnumber: integer): Longint; cdecl; external;
function  emcCommandWaitDone(Serialnumber: integer): Longint; cdecl; external;

function  convertLinearUnits(u: Double): Double; cdecl; external;

function  sendDebug(level: integer): longint; cdecl; external;
function  sendEstop: Longint; cdecl; external;
function  sendEstopReset: Longint; cdecl; external;
function  sendMachineOn: Longint; cdecl; external;
function  sendMachineOff: Longint; cdecl; external;
function  sendManual: Longint; cdecl; external;
function  sendAuto: Longint; cdecl; external;
function  sendMdi: Longint; cdecl; external;

function  sendOverrideLimits(axis: integer): Longint; cdecl; external;

function  sendJogStop(axis: integer): Longint; cdecl; external;
function  sendJogCont(axis: integer; Speed: Double): Longint; cdecl; external;
function  sendJogIncr(axis: integer; Speed,Increment: Double): Longint; cdecl; external;

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

function  sendHome(axis: integer): Longint; cdecl; external;
function  sendUnHome(axis: integer): Longint; cdecl; external;
function  sendFeedOverride(Value: Double): Longint; cdecl; external;
function  sendMaxVelocity(Velocity: Double): Longint; cdecl; external;
function  sendSpindleOverride(Value: Double): Longint; cdecl; external;
function  sendTaskPlanInit: Longint; cdecl; external;
function  sendProgramOpen(const Prog: PChar): Longint; cdecl; external;
function  sendProgramRun(Line: integer): Longint; cdecl; external;
function  sendProgramPause: Longint; cdecl; external;
function  sendProgramResume: Longint; cdecl; external;
function  sendSetOptionalStop(State: Boolean): Longint; cdecl; external;
function  sendProgramStep: Longint; cdecl; external;
function  sendMdiCmd(const cmd: PChar): Longint; cdecl; external;
function  sendLoadToolTable(const filename: PChar): Longint; cdecl; external;
function  sendToolSetOffset(tool: integer; length,diameter: double): Longint; cdecl; external;
function  sendToolSetOffset2(id: integer; zoffset,xoffset,diameter,frontangle,
  backangle: double; orientation: integer): Longint; cdecl; external;
function  sendAxisSetBacklash(axis: integer; backlash: double): Longint; cdecl; external;
function  sendAxisSetOutput(axis: integer; output: double): Longint; cdecl; external;
function  sendAxisEnable(axis,value: integer): Longint; cdecl; external;
function  sendAxisLoadComp(axis: integer; const filename: PChar; t: integer): Longint; cdecl; external;
function  sendClearProbeTrippedFlag: Longint; cdecl; external;
function  sendProbe(x,y,z: double): Longint; cdecl; external;

function  getFeedOverride: Longint; cdecl; external;

function  getEStop: Boolean; cdecl; external;
function  getMachineOn: Boolean; cdecl; external;
function  getTaskMode: Longint; cdecl; external;
function  getMistOn: Boolean; cdecl; external;
function  getFloodOn: Boolean; cdecl; external;
function  getLubeOn: Boolean; cdecl; external;
function  getSpindle: Longint; cdecl; external;
function  getBrakeOn: Boolean; cdecl; external;

implementation

end.
