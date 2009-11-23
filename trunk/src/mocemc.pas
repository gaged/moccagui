unit mocemc;

{$I mocca.inc}

interface

uses
  Classes, SysUtils,
  Forms,ExtCtrls,StdCtrls;
  
type
  TEMC = class
    function  UpdateState: Boolean; // true if Taskmode changed
    function  ForceTaskMode(ToMode: integer): Boolean; // true if Taskmode changed
    function  ForceMachineOff: Boolean;
    function  SetFeedORide(NewFeed: integer): Boolean;  // true if Feed changed
    function  SetMaxVel(NewVel: integer): Boolean;  // true if Maxvelocity changed
    function  HandleCommand(Cmd: integer): Boolean;
    procedure TaskStop;
    procedure TaskPauseResume;
    procedure TaskResume;
    procedure TaskPause;
    procedure TaskStep;
    procedure TaskRun;
  end;

function CheckError: Boolean;

implementation

uses
  mocglb,emc2pas,mocjoints;

function TEmc.ForceTaskMode(ToMode: integer): Boolean;
begin
  Result:= False;
  {$ifdef DEBUG_EMC}
  writeln('Forcetaskmode to: ' + intToStr(ToMode));
  {$endif}
  if ToMode = emcState.TaskMode then Exit;
  with emcState do
    if SpindleDirection <> 0 then
      begin
        sendSpindleOff;
        sleep(20);
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
    wait_complete;
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
  Result:= (i <> emcState.TaskMode);
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
  taskActiveCodes;  // update active G,MCodes, FWords, SWords;
end;

procedure TEmc.TaskRun;
begin
  if emcState.TaskMode <> TASKMODEAUTO then
    Exit;
  emcVars.StartFromLine:= 0;
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
      wait_complete;
    end;
end;

function TEMC.HandleCommand(Cmd: integer): boolean;
begin
  {$ifdef DEBUG_EMC}
  writeln('emc.handlecommand: ' + intToStr(cmd));
  {$endif}
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
    cmREFACT:
      sendHome(emcVars.ActiveAxis);
    cmREFALL:
      sendHome(-1);
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

