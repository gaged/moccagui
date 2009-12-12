unit mocemc;

{$I mocca.inc}

interface

uses
  Classes, SysUtils,
  Forms,ExtCtrls,StdCtrls;
  
type
  TEMC = class
    function  UpdateState: Boolean;                     // true if Taskmode changed
    function  ForceTaskMode(ToMode: integer): Boolean;  // true if Taskmode changed
    function  ForceMachineOff: Boolean;

    function  SetFeedORide(NewFeed: integer): Boolean;  // true if Feed changed
    function  SetMaxVel(NewVel: integer): Boolean;      // true if Maxvelocity changed

    function  HandleCommand(Cmd: integer): Boolean;

    procedure TaskStop;
    procedure TaskPauseResume;
    procedure TaskResume;
    procedure TaskPause;
    procedure TaskStep;
    procedure TaskRun;

    procedure TaskGoto(Line: integer);

    procedure OverrideLimits;

    procedure ExecMDI(S: string);
    procedure ExecMDILocked(S: string);

    function  GetActiveCoordSys: integer;
    procedure SetCoordsZero;

    function  WaitEmcDone: integer;
  end;


function CheckError: Boolean;

implementation

uses
  mocglb,emc2pas,mocjoints,glcanon;

{
function TEmc.CanonExecute(LineNo: integer; cmd: string): integer;
begin
  result:= execute_command(LineNo,PChar(Cmd));
end;

procedure TEmc.CanonReset;
begin
  reset_interpreter;
end;
}

function TEmc.WaitEmcDone: integer;
begin
  Result:= emcCommandWaitDone(emcCommandSerialNumber)
end;

procedure TEmc.OverrideLimits;
begin
  if emcState.ORideLimits then
    sendOverrideLimits(-1)
  else
    sendOverrideLimits(1);
end;

procedure TEmc.ExecMDI(S: string);
begin
  sendMDICmd(PCHar(S));
  // UpdateState;
end;

procedure TEmc.ExecMDILocked(S: string);
var
  OldMode: integer;
begin
  UpdateLock:= True;
  Sleep(10);
  try
    OldMode:= emcState.taskMode;
    if OldMode <> TASKMODEMDI then
      begin
        sendMDI;
        WaitEmcDone;
      end;
    sendMDICmd(PChar(S));
    WaitEmcDone;
    if OldMode <> TASKMODEMDI then
      begin
        if OldMode = TASKMODEMANUAL then
          sendManual
        else
          sendAuto;
        WaitEmcDone;
      end;
  finally
    UpdateLock:= False;
  end;
end;

function TEmc.GetActiveCoordSys: integer;
var
  s: string;
  i: integer;
begin
  Result:= -1;
  for i:= 1 to G5SysMax do
    if Pos(G5Systems[i],ActiveGCodes) > 0 then
      begin
        Result:= i;
        Exit;
      end;
end;

procedure TEmc.SetCoordsZero;
var
  i: integer;
  S: string;
  PosX,PosY,PosZ: Double;
begin
  i:= GetActiveCoordSys;
  if i < 0 then Exit;
  PosX:= GetAbsPos(Joints.AxisByChar('X'));
  PosY:= GetAbsPos(Joints.AxisByChar('Y'));
  PosZ:= GetAbsPos(Joints.AxisByChar('Z'));
  if (PosX = 0) and (PosY = 0) and (PosZ = 0) then
    Exit;
  if taskTloIsAlongW then
    begin
      // fixme
    end;
  // S:= Format('%s%d%s%s%n%s%n%s%n',['G10P',i,'L2','X',PosX,'Y',PosY,'Z',PosZ]);
  S:= Format('%s%d%s',['G10P',i,'L2 X0 Y0 Z0']);
  LastError:= S;
  ExecMDILocked(S);
end;

function TEmc.ForceTaskMode(ToMode: integer): Boolean;
begin
  Result:= False;
  {$ifdef DEBUG_EMC}
  writeln('Forcetaskmode to: ' + intToStr(ToMode));
  {$endif}
  if ToMode = emcState.TaskMode then Exit;
  if Assigned(Joints) then
    Joints.CheckJogExit;     // ensure we do not jog any more
  with emcState do
    if SpindleDirection <> 0 then
      begin
        sendSpindleOff;
        WaitEmcDone;
      end;
  case ToMode of
    TASKMODEMANUAL: sendManual;
    TASKMODEAUTO: sendAuto;
    TASKMODEMDI: sendMDI;
  end; //case
  Result:= True;
  {$ifdef DEBUG_EMC}
  writeln('Forcetaskmode new: ' + intToStr(emcState.TaskMode));
  {$endif}
end;

function TEmc.ForceMachineOff: Boolean;
begin
  Result:= False;
  if emcState.Machine then
    sendMachineOff;
  WaitEmcDone;
  Result:= True;
end;

function TEMC.SetFeedORide(NewFeed: integer): Boolean;  // true if Feed changed
begin
  with emcState do
    if ActFeed <> NewFeed then
      begin
        sendFeedOverride(NewFeed / 100);
        ActFeed:= NewFeed;
        Result:= True;
      end
    else
      Result:= False;
end;

function TEMC.SetMaxVel(NewVel: integer): Boolean;  // true if maxvel changed
begin
  with emcState do
    if ActVel <> NewVel then
      begin
        sendMaxVelocity(NewVel / 60);
        ActVel:= NewVel;
        Result:= True;
       end
     else
       Result:= False;
end;

function TEMC.UpdateState: Boolean;
var
  i: integer;
begin
  i:= taskMode;
  if UpdateLock then Exit;
  Result:= (i <> emcState.TaskMode);
  taskActiveCodes;
  with emcState do
    begin
      TaskMode:= i;
      EStop:= GetEStop;
      Machine:= GetMachineOn;
      SpDir:= SpindleDirection;
      SpInc:= SpindleIncreasing;
      SpSpeed:= SpindleSpeed;
      SpEnabled:= SpindleEnabled <> 0;
      SpBrake:= SpindleBrake <> 0;

      Flood:= coolantFlood;
      Mist:= coolantMist;
      Lube:= lubeOn;
      LubeLvl:= LubeLevel;

      Dtg:= trajDtg;
      Vel:= trajVel;
      Acc:= trajAcceleration;
      Probing:= trajProbing;

      ORideLimits:= AxisOverrideLimits(0);
      //ActVel: Integer;
      //ActFeed: Integer;

      if TaskMode = TASKMODEAUTO then
        begin
          InterpState:= taskInterpState;
          CurrentLn:= taskCurrentLine;
          ReadLn:= taskReadLine;
          ProgUnits:= taskProgramUnits;
          OptStop:= taskOptStop;
          BlockDel:= taskBlockDelete;
        end;
     end;

end;

procedure TEmc.TaskGoto(Line: integer);
var
  P: TGotoParams;
  S: string;
  ln: integer;
begin
  if emcVars.ProgramFile = '' then
    Exit;
  P.FileName:= emcVars.ProgramFile;
  P.InitCode:= 'G54';
  P.UnitCode:= 'G21';
  P.UseMetric:= True;
  //ln:= GotoLine(Line,P);
  //with P do
  //  S:= GCode + #32 + MCode + #32 + Settings;
  // LastError:= IntToStr(Line) + ':' + IntToStr(ln) + #32 + S;
end;

procedure TEmc.TaskRun;
begin
  if emcState.TaskMode <> TASKMODEAUTO then Exit;
  sendProgramRun(emcVars.StartFromLine);
end;

procedure TEmc.TaskStep;
begin
  if (emcState.TaskMode <> TASKMODEAUTO) or
    (emcState.InterpState <> INTERP_IDLE) then
      Exit;
  sendProgramStep;
end;

procedure TEmc.TaskPause;
begin
  if (emcState.TaskMode <> TASKMODEAUTO) or
    not (emcState.InterpState in [INTERP_READING,INTERP_WAITING]) then
      Exit;
  sendProgramPause;
end;

procedure TEmc.TaskResume;
begin
  UpdateState;
  if not emcState.InterpState = INTERP_PAUSED then
    Exit;
  if not (emcState.TaskMode in [TASKMODEAUTO,TASKMODEMDI]) then
    Exit;
  sendProgramResume;
end;

procedure TEmc.TaskPauseResume;
begin
  if not (emcState.TaskMode in [TASKMODEAUTO,TASKMODEMDI]) then
   Exit;
  UpdateState;
  if emcState.InterpState = INTERP_PAUSED then
    sendProgramResume
  else
    if emcState.InterpState <> INTERP_IDLE then
      sendProgramPause;
end;

procedure TEmc.TaskStop;
begin
  if emcState.TaskMode = TASKMODEAUTO then
    begin
      sendAbort;
      WaitEmcDone;
    end;
end;

function TEMC.HandleCommand(Cmd: integer): boolean;
begin
  case Cmd of
    cmESTOP:
      if not emcState.EStop then
        begin
          sendAbort;
          sendEStop;
        end
      else
        SendEStopReset;
    cmMACHINE:
      if emcState.Machine then
        SendMachineOff
      else
        SendMachineOn;
    cmJOG: ForceTaskMode(TASKMODEMANUAL);
    cmAUTO: ForceTaskMode(TASKMODEAUTO);
    cmMDI: ForceTaskMode(TASKMODEMDI);
    cmSPCW:
      if emcState.SpDir <> 0 then
        sendSpindleOff
      else
        sendSpindleForward;  // assuming that "forward" is CW
    cmSPCCW:
      if emcState.SpDir <> 0 then
        sendSpindleOff
      else
        sendSpindleReverse; // assuming that "reverse" is CCW
    cmSPPLUS:
      // if emcState.SpDir <> 0 then
        sendSpindleIncrease;
    cmSPMINUS:
      // if emcState.SpDir <> 0 then
        sendSpindleDecrease;
    cmSPBRAKE:
      if emcState.SpBrake then
        sendBrakeEngage
      else
        sendBrakeRelease;
    cmFLOOD:
      if emcState.Flood then
        SendFloodOff
      else
        SendFloodOn;
    cmMIST:
      if emcState.Mist then
        sendMistOff
      else
        sendMistOn;
    cmREFACT: Joints.HomeActive;
    cmREFALL: Joints.HomeAll;
    cmOFFSALL: SetCoordsZero;
  else
    begin
      Result:= False;
      Exit; // we do exit here cause the command is not handled...
    end;
  end; //case
  Result:= true;
end;
  
function CheckError: Boolean;
begin
  if ErrorStr[0] <> #0 then
    begin
      LastError:= PChar(ErrorStr);
      ErrorStr[0]:= #0;
      Result:= true;
    end
  else
    Result:= False;
end;
  
end.

