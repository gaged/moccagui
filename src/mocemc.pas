unit mocemc;

{$I mocca.inc}

interface

uses
  Classes, SysUtils;
  
type
  TEMC = class
    function  ToLinearUnits(Value: Double): double;
    function  UpdateState: Boolean;
    function  HandleCommand(Cmd: integer): Boolean;
    function  ForceTaskMode(ToMode: integer): Boolean;
    function  ForceMachineOff: Boolean;
    procedure SetFeedORide(Feed: integer);
    procedure SetMaxVel(NewVel: integer);
    procedure ORideLimits;
    procedure SetDisplayUnits(UseMetric: Boolean);
    procedure SetCoordsZero;
    procedure TouchOffAxis(Axis: Char; iCoord: integer; Value: double);
    procedure TaskStop;
    procedure TaskPauseResume;
    procedure TaskResume;
    procedure TaskPause;
    procedure TaskStep;
    procedure TaskRun;
    procedure Execute(cmd: string);
    procedure ExecuteSilent(cmd: string);
    function  GetActiveCoordSys: integer;
    function  GetActiveIsInch: Boolean;
    function  GetMaxVelText: string;  // returns MaxVel in value[units]/min;
    function  WaitDone: integer;
    procedure LoadTools;
  end;

var
  Emc: TEMC;                    // the base class of the emc interface

implementation

uses
  mocglb,emc2pas,mocjoints,glcanon,RunClient,offsetdlg,tooleditdlg,touchoff;

procedure TEmc.LoadTools;
var
  FileName: PChar;
begin
  if Length(Vars.ToolFile) < 1 then Exit;
  FileName:= PChar(Vars.ToolFile);
  sendLoadToolTable(FileName);
  if WaitDone = 0 then
    begin
      if not ToolsInitialized then
        begin
          InitToolTable;
          ToolsInitialized:= True;
        end;
      LoadToolTable(FileName);
    end;
end;

function TEmc.ToLinearUnits(Value: double): double;
begin
  Result:= convertLinearUnits(Value);
end;

function TEmc.WaitDone: integer;
var
  i: integer;
begin
  i:= emcCommandWaitDone(emcCommandSerialNumber);
  if i <> 0 then
    LastError:= 'wait command done failed';
  Result:= i;
end;

function TEmc.GetMaxVelText: string;
begin
  Result:= FloatToStr(ConvertLinearUnits(State.ActVel)) + Vars.UnitVelStr
end;

procedure TEmc.ORideLimits;
begin
  sendOverrideLimits(0)
end;

procedure TEmc.Execute(cmd: string);
var
  i: integer;
begin
  i:= sendMDICmd(PCHar(cmd));
  if i <> 0 then
    LastError:= 'call to mdi returned ' + inttostr(i);
end;

procedure TEmc.ExecuteSilent(cmd: string);
var
  OldMode: integer;
begin
  UpdateLock:= True;
  Sleep(10);
  try
    OldMode:= State.taskMode;
    if OldMode <> TASKMODEMDI then
      begin
        sendMDI;
        WaitDone;
      end;
    Execute(cmd);
    WaitDone;
    if OldMode <> TASKMODEMDI then
      begin
        if OldMode = TASKMODEMANUAL then
          sendManual
        else
          sendAuto;
        WaitDone;
      end;
  finally
    UpdateLock:= False;
  end;
end;

function TEmc.GetActiveCoordSys: integer;
var
  i: integer;
begin
  Result:= -1;
  for i:= 0 to CoordSysMax do
    if Pos(CoordSys[i],ActiveGCodes) > 0 then
      begin
        Result:= i;
        Exit;
      end;
end;

function TEmc.GetActiveIsInch: Boolean;
begin
  Result:= Pos('G20',ActiveGCodes) > 0;
end;

procedure TEmc.SetCoordsZero;
var
  i: integer;
  S: string;
  PosX,PosY,PosZ: Double;
  Scale: double;
  IsInch: Boolean;
begin
  i:= GetActiveCoordSys;
  IsInch:= GetActiveIsInch;
  if IsInch then
    Scale:= 25.4
  else
    Scale:= 1;

  if i < 0 then
    begin
      LastError:= 'invalid call too coord system';
      Exit;
    end;
  PosX:= GetAbsPos(Joints.AxisByChar('X')) / Scale;
  PosY:= GetAbsPos(Joints.AxisByChar('Y')) / scale;
  PosZ:= GetAbsPos(Joints.AxisByChar('Z')) / scale;
  if taskTloIsAlongW then
    begin
      // fixme
    end;
  S:= Format('%s%d%s%.5f%s%.5f%s%.5f',['G10L2P',i+1,'X',PosX,'Y',PosY,'Z',PosZ]);
  ExecuteSilent(S);
  UpdateError;
  LastError:= S;
  if Assigned(clRun) then
    clRun.UpdatePreview;
end;

procedure TEmc.TouchOffAxis(Axis: Char; iCoord: integer; Value: double);
var
  s: string;
  IsInch: Boolean;
  V: Double;
begin
  IsInch:= GetActiveIsInch;
  if IsInch then
    V:= Value / 25.4
  else
    V:= Value;
  S:= Format('%s%d%s%.5f',['G10L2P',iCoord,Axis,V]);
  ExecuteSilent(s);
  LastError:= S;
end;

function TEmc.ForceTaskMode(ToMode: integer): Boolean;
begin
  Result:= False;
  if ToMode = State.TaskMode then Exit;
  if Assigned(Joints) then
    Joints.CheckJogExit;
  with State do
    if SpindleDirection <> 0 then
      begin
        sendSpindleOff;
        WaitDone;
      end;
  case ToMode of
    TASKMODEMANUAL: sendManual;
    TASKMODEAUTO: sendAuto;
    TASKMODEMDI: sendMDI;
  end; //case
  Result:= True;
end;

function TEmc.ForceMachineOff: Boolean;
begin
  Result:= False;
  sendMachineOff;
  WaitDone;
  Result:= True;
end;

procedure TEmc.SetDisplayUnits(UseMetric: Boolean);
begin
  State.UnitsChanged:= UseMetric <> Vars.Metric;
  if State.UnitsChanged then
    begin
      if UseMetric then
        begin
          Vars.UnitStr:= 'mm';
          LinearUnitConversion:= LINEAR_UNITS_MM;
        end
      else
        begin
          Vars.UnitStr:= 'inch';
          LinearUnitConversion:= LINEAR_UNITS_INCH;
        end;
      Vars.UnitVelStr:= Vars.UnitStr + '/min';
      Vars.Metric:= UseMetric;
    end;
end;

procedure TEmc.SetFeedORide(Feed: integer);
begin
  if Feed <> State.ActFeed then
    begin
      sendFeedOverride(Feed / 100);
      State.ActFeed:= Feed;
    end;
end;

procedure TEmc.SetMaxVel(NewVel: integer);
begin
  if NewVel <> State.ActVel then
    begin
      sendMaxVelocity(State.ActVel / 60);
      State.ActVel:= NewVel;
    end;
end;

function TEmc.UpdateState: Boolean;
var
  i: integer;
begin
  // Result:= False;
  if UpdateLock then Exit;

  if UpdateStatus <> 0 then Exit;
  if UpdateError <> 0 then Exit;

  i:= taskMode;
  Result:= (i <> State.TaskMode);
  with State do
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
      LubeLevel:= emc2pas.lubeLevel;
      Dtg:= trajDtg;
      Vel:= trajVel;

      Acc:= trajAcceleration;
      Probing:= trajProbing;
      ORideLimits:= AxisOverrideLimits(0);

      CurrentTool:= toolInSpindle;
      ToolPrepared:= toolPrepped <> 0;
      ToolOffset:= toolLengthOffset;

      ORideLimits:= AxisOverrideLimits(0);

      TloAlongW:=  taskTloIsAlongW;

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
  if State.TaskMode <> TASKMODEAUTO then Exit;
  sendProgramRun(Vars.StartLine);
end;

procedure TEmc.TaskStep;
begin
  if (State.TaskMode <> TASKMODEAUTO) or
    (State.InterpState <> INTERP_IDLE) then
      Exit;
  sendProgramStep;
end;

procedure TEmc.TaskPause;
begin
  if (State.TaskMode <> TASKMODEAUTO) or
    not (State.InterpState in [INTERP_READING,INTERP_WAITING]) then
      Exit;
  sendProgramPause;
end;

procedure TEmc.TaskResume;
begin
  UpdateState;
  if not State.InterpState = INTERP_PAUSED then
    Exit;
  if not (State.TaskMode in [TASKMODEAUTO,TASKMODEMDI]) then
    Exit;
  sendProgramResume;
end;

procedure TEmc.TaskPauseResume;
begin
  if not (State.TaskMode in [TASKMODEAUTO,TASKMODEMDI]) then
   Exit;
  UpdateState;
  if State.InterpState = INTERP_PAUSED then
    sendProgramResume
  else
    if State.InterpState <> INTERP_IDLE then
      sendProgramPause;
end;

procedure TEmc.TaskStop;
begin
  if State.TaskMode = TASKMODEAUTO then
    begin
      sendAbort;
      WaitDone;
    end;
end;

function TEMC.HandleCommand(Cmd: integer): boolean;
begin
  case Cmd of
    cmESTOP:
      if not State.EStop then
        begin
          sendAbort;
          sendEStop;
        end
      else
        SendEStopReset;
    cmMACHINE:
      if State.Machine then
        SendMachineOff
      else
        SendMachineOn;
    cmJOG: ForceTaskMode(TASKMODEMANUAL);
    cmAUTO: ForceTaskMode(TASKMODEAUTO);
    cmMDI: ForceTaskMode(TASKMODEMDI);
    cmSPCW:
      if State.SpDir <> 0 then
        sendSpindleOff
      else
        sendSpindleForward;  // assuming that "forward" is CW
    cmSPCCW:
      if State.SpDir <> 0 then
        sendSpindleOff
      else
        sendSpindleReverse; // assuming that "reverse" is CCW
    cmSPPLUS:
      if State.SpDir <> 0 then
        sendSpindleIncrease;
    cmSPMINUS:
      if State.SpDir <> 0 then
        sendSpindleDecrease;
    cmSPBRAKE:
      if State.SpBrake then
        sendBrakeEngage
      else
        sendBrakeRelease;
    cmFLOOD:
      if State.Flood then
        SendFloodOff
      else
        SendFloodOn;
    cmMIST:
      if State.Mist then
        sendMistOff
      else
        sendMistOn;
    cmREFACT: Joints.HomeActive;
    cmREFALL: Joints.HomeAll;
    cmZEROALL: SetCoordsZero;
    cmZEROACT:
      begin
        DoTouchOff;
        if Assigned(clRun) then
          clRun.UpdatePreview;
      end;
    cmLIMITS: Emc.ORideLimits;
    cmOFFSDLG:
      begin
        EditOffsets;
        if Assigned(clRun) then
          clRun.UpdatePreview;
      end;
    cmTOOLS: EditTools;
    cmUNITS: SetDisplayUnits(not Vars.Metric);
  else
    begin
      Result:= False;
      Exit; // we do exit here cause the command is not handled...
    end;
  end; //case
  Result:= true;
end;
  

end.

