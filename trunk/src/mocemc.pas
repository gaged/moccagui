unit mocemc;

{$I mocca.inc}

interface

uses
  Classes, SysUtils;


type
  TEMC = class

    constructor Create;

    function CreateAxisDef: boolean;
    procedure SetupAxes;

    procedure InitSelf;

    procedure Execute(cmd: string);
    procedure ExecuteSilent(cmd: string);
    procedure ExecuteHalCmd;

    function  SetTaskMode(ToMode: integer): Boolean;
    function  SetMachineOff: Boolean;

    function  GetActiveCoordSys: integer;

    function  HandleCommand(Cmd: integer): Boolean;
    function  ToDisplayUnits(Value: double): double;
    function  UpdateState: Boolean;
    procedure SetDisplayUnits(UseMetric: Boolean);
    procedure SetORideLimits(ORide: Boolean);

    procedure TouchOffAxis(Axis: Char; iCoord: integer; Value: double);
    procedure TouchOff(Axis: Char);
    procedure TouchOffWiz;
    procedure SetCoordsZero;

    procedure Stop;
    procedure Resume;
    procedure Pause;
    procedure Step;
    procedure Run(Line: integer);
    procedure Reentry;

    procedure SaveState;

    procedure ResetInterpreter;
    function  WaitDone: integer;

    procedure LoadTools;
    procedure ChangeTool;

    procedure EditFile;
    procedure OpenFile(AFileName: string);
    procedure ReloadFile;

    procedure ShowUserErrorMsg(i: integer);

  private
    FFeedOverride: integer;
    FSpindleOverride: integer;
    FMaxVelocity: integer;
    FHalCmd: integer;
    FHalError: integer;

    procedure SetFeedOverride(Value: integer);
    procedure SetMaxVelocity(Value: integer);
    procedure SetSpindleOverride(Value: integer);
  public
    property FeedOverride: integer read FFeedOverride write SetFeedOverride;
    property SpindleOverride: integer read FSpindleOverride write SetSpindleOverride;
    property MaxVelocity: integer read FMaxVelocity write SetMaxVelocity;
  end;

var
  Emc: TEMC;                    // the base class of the emc interface

implementation

uses
  Forms,
  mocglb,emc2pas,hal,mocjoints,
  runclient,
  offsetdlg,
  tooleditdlg,toolchange,
  touchoff, touchoffwiz,
  coordrotate,
  glcanon,simclient;

var
  UpdateCounter: integer;

const
  ToolsInitialized: Boolean = False;

type
  TReentryState = record
    MotionLn: integer;
    GCodes: string;
    MCodes: string;
    PosX,PosY,PosZ: double;
    OriginX,OriginY,OriginZ: double;
  end;

var
  LastState: TReentryState;

procedure TEmc.OpenFile(AFileName: string);
begin
  Vars.ProgramFile:= AFileName;
  sendProgramOpen(PChar(Vars.ProgramFile));
  WaitDone;
end;

procedure TEmc.ReloadFile;
begin
  if Vars.ProgramFile <> '' then
    begin
      SetTaskMode(TASKMODEAUTO);
      sendAbort;
      WaitDone;
      sendProgramOpen(PChar(Vars.ProgramFile));
      WaitDone;
    end;
end;

constructor TEmc.Create;
begin
  inherited Create;
  if not CreateAxisDef then
    raise Exception.Create('Error creating Axes');
  State.LinearUnits:= trajlinearUnits;
  State.AngularUnits:= trajangularUnits;
  SetupAxes;
  InitSelf;
end;

procedure TEmc.InitSelf;
const
  MSG1 = 'No Jog-Increments set in config-file';
  MSG2 = 'Using default configuration';
begin
  if Vars.JogIncMax < 1 then
    begin
      writeln(MSG1);
      writeln(MSG2);
      with Vars do
        begin
          JogIncrements[0].Text:= 'Durchgehend';
          JogIncrements[0].Value:= 0;
          JogIncrements[1].Text:= '1.00 mm';
          JogIncrements[1].Value:= 1;
        end;
      Vars.JogIncMax:= 1;
    end;
  FFeedOverride:= 100;
  FSpindleOverride:= 100;
  if DefaultSpindleSpeed > 0 then
    emcSpindleDefaultSpeed:= DefaultSpindleSpeed;
end;

procedure TEmc.ResetInterpreter;
begin
  sendTaskPlanInit;
end;

procedure TEmc.ShowUserErrorMsg(i: integer);
var
  Ln: integer;
  s: string;
begin
  FHalError:= i;
  if not Assigned(UserErrors) then Exit;
  if i > 1000 then
    begin
      Ln:= i - 1000;
      HandleCommand(cmESTOP);
    end
  else
    Ln:= i;
  s:= '';
  if Ln < UserErrors.Count then
    s:= UserErrors[Ln]
  else
    s:= 'Unknow error: ' + IntToStr(Ln);
  LastError:= s;
  // if Assigned(GlobalErrors) then
  //  GlobalErrors.Add(S);
end;

procedure TEmc.SetupAxes;
var
  AMin,AMax: Double;
  i,Axis: integer;
  s: string;
  c: char;
begin
  Vars.MLimits:= SetExtents(0,1,0,1,0,1);
  Writeln('Mask: ',CoordMask);
  writeln('Map : ',Vars.CoordMap);
  for i:= 0 to Length(CoordMask) - 1 do
    begin
      c:= CoordMask[i+1];
      Axis:= Pos(c,Vars.CoordNames) - 1;
      if (Axis >= 0) and (Pos(c,Vars.CoordMap) > 0) then
        begin
          writeln('Achse: ',c);
          AMin:= AxisMinPositionLimit(i);
          AMax:= AxisMaxPositionLimit(i);
          if (State.LinearUnits = 1) and (Vars.Axis[Axis].IsLinear) then
            begin
              AMin:= AMin / 25.4;
              AMax:= AMax / 25.4;
            end;
        case c of
          'X':
            begin
              Vars.MLimits.MinX:= AMin;
              Vars.MLimits.MaxX:= AMax;
            end;
          'Y':
            if Vars.IsLathe then
              begin
                Vars.MLimits.MinY:= 0;
                Vars.MLimits.MaxY:= 0;
              end
            else
              begin
                Vars.MLimits.MinY:= AMin;
                Vars.MLimits.MaxY:= AMax;
              end;
          'Z':
            begin
              Vars.MLimits.MinZ:= AMin;
              Vars.MLimits.MaxZ:= AMax;
            end;
        end;
        if Verbose > 0 then
          begin
            if Vars.Axis[Axis].IsLinear then
              s:= 'Linear axis: '
            else
              s:= 'Angular axis: ';
            s:= s + Vars.Axis[Axis].AxisChar;
            writeln(s);
          end;
      end;
    end;
end;

procedure AddAxisGeometry(C: Char; var Sign: integer);
var
  i: integer;
begin
  if not (C in ['U'..'Z','A'..'C']) then
    Exit;
  for i:= 0 to Vars.NumAxes - 1 do
    if Vars.Axis[i].AxisChar = C then
      begin
        Vars.Axis[i].Geometry:= Sign;
        Sign:= 1;
        if Verbose > 0 then
          Writeln('Added Geometry for Axis ' + inttostr(i) + ' :' + C);
        Exit;
      end;
end;

procedure SetupGeometry;
var
  i,Sign: integer;
  s: string;
  c: char;
begin
  for i:= 0 to MaxAxes - 1 do
    Vars.Axis[i].Geometry:= 0;
  if Vars.Geometry = '' then
    begin
      writeln('no geometry used.');
      Exit;
    end;
  s:= UpperCase(Vars.Geometry);
  writeln('using geometry: ',s);
  Sign:= 1;
  for i:= 1 to Length(s) do
    begin
      c:= s[i];
      if c <> #32 then
        begin
          if c = '-' then Sign:= -1;
          AddAxisGeometry(C,Sign);
        end;
    end;
end;

function TEmc.CreateAxisDef: boolean;
var
  FailCount: integer;
  i,Axes,AxCount: integer;
begin
  Result:= False;
  FailCount:= 0;
  Axes:= 0;
  AxCount:= 0;
  Vars.CoordNames:= '';
  Vars.CoordMap:= '';
  Vars.NumAxes:= 0;
  updateStatus;
  Axes:= trajAxes;
  for i:= 0 to MaxAxes - 1 do
    Vars.Axis[i].AxisChar:= #0;
  while Axes = 0 do
    begin
      writeln('waiting for traj.axes...');
      sleep(200);
      inc(FailCount);
      if FailCount > 10 then Exit;
      UpdateStatus;
      Axes:= trajAxes;
    end;
  if Verbose > 0 then
    writeln('Traj Axes: ',Axes);
  Vars.AxisMask:= Word(trajAxisMask);
  if Vars.AxisMask = 0 then
    begin
      writeln('Traj. returned a Axis Mask of 0!');
      Exit;
    end;
  if Verbose > 0 then
    writeln('Traj Axismask: ',Vars.AxisMask);
  for i:= 0 to Length(CoordMask) - 1 do
    begin
      if (Vars.AxisMask and (1 shl i) > 0) then
        begin
          Vars.Axis[AxCount].AxisChar:= CoordMask[i+1];
          Vars.Axis[AxCount].IsLinear:= AxisAxisType(i) = 1;
          Vars.CoordNames:= Vars.CoordNames + CoordMask[i+1];
          Vars.CoordMap:= Vars.CoordMap + CoordMask[i+1];
          inc(AxCount);
        end
      else
        Vars.CoordMap:= Vars.CoordMap + #32;
    end;
  if AxCount < 1 then
    begin
      writeln('Number of Axes is zero!');
      Exit;
    end;
  Vars.NumAxes:= AxCount;
  //writeln('Axisdefs: ',Vars.NumAxes);
  SetupGeometry;
  Result:= True;
end;


procedure TEmc.SetFeedOverride(Value: integer);
var
  HalFeed: integer;
begin
  if (Value < 1) or (Value > Vars.MaxFeedOverride) then
    Exit;
  if (Value <> FFeedOverride) then
    begin
      FFeedOverride:= Value;
      if UseHalFeed then
        begin
          HalFeed:= GetHalFeed;
          if HalFeed <> FFeedOverride then
            SetHalFeed(FFeedOverride);
        end;
    end;
end;

procedure TEmc.SetSpindleOverride(Value: integer);
var
  HalSpindle: integer;
begin
  if (Value < 1) or (Value > Vars.MaxSpORide) then
    Exit;
  if (Value <> FSpindleOverride) then
    begin
      FSpindleOverride:= Value;
      HalSpindle:= GetHalSpindle;
      if HalSpindle <> FSpindleOverride then
        SetHalSpindle(FSpindleOverride);
    end;
end;

procedure TEmc.SetMaxVelocity(Value: integer);
var
  HalVel: integer;
begin
  if (Value < 1) or (Value > State.MaxVel) then
    Exit;
  if (Value <> FMaxVelocity) then
    begin
      FMaxVelocity:= Value;
      HalVel:= GetHalVelocity;
      if HalVel <> FMaxVelocity then
        SetHalVelocity(FMaxVelocity);
    end;
end;

procedure TEmc.EditFile;
begin
  ExecEditor;
end;

procedure TEmc.LoadTools;
var
  FileName: PChar;
begin
  if Length(Vars.ToolFile) < 1 then
    begin
      writeln('Keine Werkzeugdatei vorhanden.');
      Exit;
    end;
  FileName:= PChar(Vars.ToolFile);
  sendLoadToolTable(FileName);
  if WaitDone = 0 then
    begin
      if not ToolsInitialized then
        begin
          {$ifdef VER_24}
          RandomToolChanger:= 0;
          {$endif}
          InitToolTable;
        end;
      ToolsInitialized:= True;
      LoadToolTable(FileName);
    end; 
end;

function ExecToolChange(Cmd: string): boolean;
begin
  {$IFDEF DEBUG_EMC}
  writeln('ExecToolChange: ',Cmd);
  {$ENDIF}
  Result:= False;
  ScriptRunning:= True;
  try
    sendMDI;
    Emc.WaitDone;
    if sendMDICmd(PChar(Cmd)) <> 0 then
    begin
      LastError:= 'Error executing toolchange- command 1';
      ScriptRunning:= False;
      Exit;
    end;
    if emcCommandWaitReceived(emcCommandSerialNumber) <> 0 then
      begin
        LastError:= 'Error executing toolchange- command 2';
        ScriptRunning:= False;
        Exit;
      end;
    while (emcPollStatus = RCS_EXEC) and (ScriptRunning) do
      begin
        Application.ProcessMessages;
      end;
    Result:= True;
  finally
      SendManual;
      ScriptRunning:= False;
  end;
end;

procedure TEmc.ChangeTool;
var
  s: string;
  i: integer;
begin
  i:= DoChangeTool;
  {$IFDEF DEBUG_EMC}
  writeln('ChangeTool: ' + IntToStr(i));
  {$ENDIF}
  if (i < 1) or (i > CANON_TOOL_MAX - 1) then
    Exit;
  if i <> State.CurrentTool then
    begin
      s:= 'T' + IntToStr(i) + ' M6';
      ExecToolChange(s);
      clRun.UpdatePreview(True);
    end;
end;

function TEmc.ToDisplayUnits(Value: double): double;
var
  mm: double;
begin
  mm:= Value / State.LinearUnits;
  if Vars.ShowMetric then
    Result:= mm
  else
    Result:= mm / 25.4;
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

procedure TEmc.SetORideLimits(ORide: Boolean);
begin
  if ORide then
    sendOverrideLimits(0)
  else
    sendOverrideLimits(-1);
end;

procedure TEmc.Execute(cmd: string);
var
  i: integer;
begin
  {$IFDEF DEBUG_EMC}
  writeln('Execute: ' + cmd);
  {$ENDIF}
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
    OldMode:= State.Mode;
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

function CheckInMotion: Boolean;
var
  i,j: integer;
begin
  Result:= False;
  UpdateStatus;
  for i:= 0 to Vars.NumAxes - 1 do
    begin
      j:= Pos(Vars.Axis[i].AxisChar,CoordMask) - 1;
      if j < 0 then Break;
      if (AxisInPos(j) <> 1) or AxisHoming(j) then
        begin
          Result:= True;
          Exit;
        end;
    end;
end;

procedure CheckModeManual;
begin
  if taskMode = TASKMODEMANUAL then Exit;
  sendManual;
  Emc.WaitDone;
  Sleep(20);
  UpdateStatus;
  UpdateError;
  if (taskMode <> TASKMODEMANUAL) then
    raise Exception.Create('Error: Cannot change Taskmode to "Manual"');
end;

procedure CheckModeMDI;
begin
  if taskMode = TASKMODEMDI then Exit;
  sendMDI;
  Emc.WaitDone;
  Sleep(20);
  UpdateStatus;
  UpdateError;
  if (taskMode <> TASKMODEMDI) then
    raise Exception.Create('Error: Cannot change Taskmode to "MDI"');
end;

procedure CheckModeAuto;
begin
  if taskMode = TASKMODEAUTO then Exit;
  sendAuto;
  Emc.WaitDone;
  Sleep(20);
  UpdateStatus;
  UpdateError;
  Sleep(20);
  if ErrorStr[0] <> #0 then
    raise Exception.Create(PChar(ErrorStr));
  //if (taskMode <> TASKMODEAUTO) then
  //  raise Exception.Create('Error: Cannot change Taskmode to "Auto"');
end;

// Stops execution and returns last line of code);
function CheckAutoStop: integer;
var
  i: integer;
  LastLn: integer;
begin
  Result:= -1;
  writeln('Interp.-State: ',taskInterpstate);
  LastLn:= taskMotionLine;
  if LastLn < 1 then
    LastLn:= taskCurrentLine + 1;
  sendAbort;
  Emc.WaitDone;
  Sleep(20);
  UpdateStatus;
  UpdateError;
  if ErrorStr[0] <> #0 then
    raise Exception.Create(PChar(ErrorStr));
  Result:= LastLn;
end;

procedure TEmc.ExecuteHalCmd;
var
  Err: integer;
  LastLn: integer;
begin
  if FHalCmd < 1 then Exit;
  if Verbose > 0 then
    writeln('Executing HalCmd: ',FHalCmd);
  if (State.Mode <> TASKMODEAUTO)  then
    begin
      Emc.HandleCommand(FHalCmd);
      FHalCmd:= 0;
      Exit;
    end;
  { disabled, future release 
  UpdateLock:= True;
  writeln('Updatelock: True');
  Sleep(10);
  try
    LastLn:= CheckAutoStop;
    writeln('Stopped at line: ',LastLn);
    CheckModeManual;
    writeln('Switched to mode manual');
    HandleCommand(FHalCmd);
    FHalCmd:= 0;
    while CheckInMotion do
      begin
        Application.ProcessMessages;
      end;
    // WaitDone;
    Sleep(20);
    UpdateStatus;
    UpdateError;
     if ErrorStr[0] <> #0 then
        raise Exception.Create(PChar(ErrorStr));
    writeln('executed hal cmd: ',FHalCmd);
    Sleep(100);
    CheckModeAuto;
    writeln('switched back to mode auto.');
    Vars.StartLine:= LastLn;
    sendProgramRun(LastLn);
    //WaitDone;
    writeln('restarted program at line: ',LastLn);
  except
    on E:Exception do
      begin
        writeln(E.Message);
      end;
  end;
  writeln('UpdateLock: False');
  UpdateLock:= False; }
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

procedure TEmc.SetCoordsZero;
var
  i,G: integer;
  s: string;
  d: double;
  IsMetric: Boolean;
  C:Char;
  Axis: integer;
  Count: integer;
begin
  Count:= 0;
  G:= GetActiveCoordSys + 1;
  if G < 1 then
    raise Exception.Create('Invalid Coord System Index:' + IntToStr(G));
  IsMetric:= State.ProgramUnits = CANON_UNITS_MM;
  s:= 'G10L2P' + IntToStr(G);
  for i:= 1 to Length(Vars.CoordNames) do
    begin
      C:= Vars.CoordNames[i];
      Axis:= Pos(C,CoordMask) - 1;
      if Axis < 0 then
        raise Exception.Create('set coords zero: invalid axis: ' + C);
      // Get Position in mm
      d:= GetAbsPos(Axis) / State.LinearUnits;
      if not IsMetric then
        d:= d / 25.4;
      if Vars.Axis[i-1].IsLinear then
        begin
          S:= S + Format('%s%.5f',[c,d]);
          inc(Count);
        end;
    end;
  if Count < 1 then Exit;
  ExecuteSilent(S);
  UpdateError;
  clRun.UpdatePreview(True);
end;

procedure TEmc.TouchOffAxis(Axis: Char; iCoord: integer; Value: double);
var
  s: string;
  IsMetric: Boolean;
  AbsMM,OffsetPos,V: Double;
  i: integer;
begin
  IsMetric:= State.ProgramUnits = CANON_UNITS_MM;
  if IsMetric then Writeln('metric') else writeln('inch');
  i:= Pos(Axis,Vars.CoordMap) - 1;
  if i < 0 then
    raise Exception.Create('Touchoff: invalid Axis: ' + Axis);
  // Get Position in mm
  AbsMM:= GetAbsPos(i) / State.LinearUnits;
  writeln('AbsMM: ' + FloatToStrF(AbsMM,ffFixed,8,2));
  v:= Value;
  if not Vars.ShowMetric then
    v:= v * 25.4;
  writeln('Value: ' + FloatToStrF(v,ffFixed,8,2));
  OffsetPos:= AbsMM - v;

  if not IsMetric then
    OffsetPos:= OffsetPos / 25.4;
  writeln('Offsetpos: ' + FloatToStrF(OffsetPos,ffFixed,8,2));
  S:= Format('%s%d%s%.5f',['G10L2P',iCoord,Axis,OffsetPos]);
  ExecuteSilent(s);
  clRun.UpdatePreview(True);
end;

procedure TEmc.TouchOffWiz;
var
  s: string;
begin
  s:= '';
  s:= DoTouchOffWiz;
  if s <> '' then
    begin
      ExecuteSilent(s);
      clRun.UpdatePreview(True);
    end;
end;

function TEmc.SetTaskMode(ToMode: integer): Boolean;
begin
  {$IFDEF DEBUG_EMC}
  writeln('ForceTaskMode',ToMode);
  {$ENDIF}
  Result:= False;
  if ToMode = State.Mode then Exit;
  if State.Mode = TaskModeManual then
    if Assigned(Joints) then
      Joints.CheckJogExit;
{  if ToMode <> TASKMODEAUTO then
    if not State.EStop then
      if State.SpindleDirection <> 0 then
        begin
          sendSpindleOff;
          WaitDone;
        end;  }
  case ToMode of
    TASKMODEMANUAL: sendManual;
    TASKMODEAUTO: sendAuto;
    TASKMODEMDI: sendMDI;
  end; //case
  Result:= True;
end;

function TEmc.SetMachineOff: Boolean;
begin
  Result:= False;
  sendMachineOff;
  WaitDone;
  Result:= True;
end;

procedure TEmc.SetDisplayUnits(UseMetric: Boolean);
begin
  State.UnitsChanged:= UseMetric <> Vars.ShowMetric;
  if State.UnitsChanged then
    begin
      if UseMetric then
        begin
          Vars.UnitStr:= 'mm';
          //LinearUnitConversion:= LINEAR_UNITS_MM;
        end
      else
        begin
          Vars.UnitStr:= 'inch';
          //LinearUnitConversion:= LINEAR_UNITS_INCH;
        end;
      Vars.UnitVelStr:= Vars.UnitStr + '/min';
      Vars.ShowMetric:= UseMetric;
    end;
end;

function TEmc.UpdateState: Boolean;
var
  i: integer;
begin
  Result:= False;

  if UpdateLock then Exit;

  if FHalCmd <> 0 then
    begin
      ExecuteHalCmd;
      FHalCmd:= 0;
      Exit;
    end;

  if UpdateStatus <> 0 then Exit;
  if UpdateError <> 0 then Exit;

  i:= taskMode;
  Result:= (i <> State.Mode);
  State.Mode:= i;

  with State do
    begin
      State:= taskState;
      SpDir:= SpindleDirection;
      SpInc:= SpindleIncreasing;
      SpSpeed:= SpindleSpeed;
      SpEnabled:= SpindleEnabled <> 0;
      SpBrake:= SpindleBrake <> 0;
      SpIncreasing:= SpindleIncreasing;
      Flood:= coolantFlood;
      Mist:= coolantMist;
      Lube:= lubeOn;
      LubeLevel:= emc2pas.lubeLevel;
      Dtg:= trajDtg;
      Vel:= trajVel;
      CurrentVel:= trajCurrentVel;
      Acc:= trajAcceleration;
      Probing:= trajProbing;
      CurrentTool:= toolInSpindle;
      ToolOffset:= toolLengthOffset;
      ORideLimits:= AxisOverrideLimits(0);
      {$ifdef VER_23}
      TloAlongW:=  taskTloIsAlongW;
      ToolPrepared:= toolPrepped <> 0;
      {$endif}
      FeedORideEnabled:= trajFeedORideEnabled;
      SpindleORideEnabled:= trajSpindleORideEnabled;
      ProgramUnits:= taskProgramUnits;
      JointsMode:= trajMode <> 1;
      if TaskMode = TASKMODEAUTO then
        begin
          InterpState:= taskInterpState;
          CurrentLn:= taskCurrentLine;
          ReadLn:= taskReadLine;
          ProgUnits:= taskProgramUnits;
          OptStop:= taskOptStop;
          BlockDel:= taskBlockDelete;
          ExecState:= taskExecState;
        end;
     end;
  taskActiveCodes;  // update active G,MCodes, FWords, SWords;

  Inc(UpdateCounter);

  if UpdateCounter > 10 then
    begin
      UpdateCounter:= 1;
      // Update Feed, Velocity etc

      i:= GetHalError;
      if i > 0 then
        if i <> FHalError then
          begin
            ShowUserErrorMsg(i);
          end;

      if UseHalFeed then
        begin
          i:= GetHalFeed;  // Check if Hal-Feed changed
          if (i <> FFeedOverride) then FeedOverride:= i;
          if FFeedOverride <> State.ActFeed then
          begin
            SendFeedOverride(FFeedOverride / 100);
            State.ActFeed:= FFeedOVerride;
          end;
        end;

      i:= GetHalSpindle;
      if (i <> FSpindleOverride) then
        SpindleOverride:= i;

      if FSpindleOverride <> State.ActSpORide then
        begin
          // if FSpindleOverride < 1 then FSpindleOverride:= 1;
          sendSpindleOverride(FSpindleOverride / 100);
          State.ActSpORide:= FSpindleOverride;
        end;

      i:= GetHalVelocity;
      if (i <> FMaxVelocity) then
        MaxVelocity:= i;

      if FMaxVelocity <> State.ActVel then
        begin
          State.ActVel:= FMaxVelocity;
          if State.ActVel < 1 then State.ActVel:= 1;
          sendMaxVelocity(State.ActVel / 60);
        end;

      GetHalCommand(FHalCmd);
      // HalCommand will be executed on next update cycle
   end;
end;

procedure TEmc.Run(Line: integer);
begin
  if Vars.StartLine <> Line then
    Vars.StartLine:= Line;
  if State.Mode = TASKMODEAUTO then
    sendProgramRun(Vars.StartLine);
end;

procedure TEmc.Step;
begin
  if (State.Mode = TASKMODEAUTO) then
    sendProgramStep;
end;

procedure TEmc.SaveState;
begin
  UpdateStatus;
  taskActiveCodes;
  with LastState do
    begin
      MotionLn := taskMotionline - 1;
      PosX:= getRelPos(0);
      PosY:= getRelPos(1);
      PosZ:= getRelPos(2);
      GCodes:= PChar(ActiveGCodes);
      MCodes:= PChar(ActiveMCodes);
    end;
  if Verbose > 0 then
    writeln('paused at: ',LastState.MotionLn);
end;

procedure TEmc.Reentry;
var
  i: integer;
  Metric: Boolean;
  Scale: Double;
begin
  if State.Mode <> TASKMODEAUTO then Exit;
  if LastState.MotionLn < 1 then
    begin
      LastError:= 'Cannot do a re-entry if the program was not stopped.';
      Exit;
    end;
  with LastState do
    begin
      writeln('Programline: ',MotionLn);
      writeln('Gcodes: ',gcodes);
      writeln('MCodes: ',mcodes);
      Scale:= 1;
      writeln('X: ' + PosToString(PosX * Scale));
      writeln('Y: ' + PosToString(PosY * Scale));
      writeln('Z: ' + PosToString(PosZ * Scale));
    end;
  Run(LastState.MotionLn);
end;

procedure TEmc.Pause;
begin
  if (State.Mode <> TASKMODEAUTO) then Exit;
  if not (State.InterpState in [INTERP_READING,INTERP_WAITING]) then
    Exit;
  sendProgramPause;
  Sleep(20);
  // WaitDone;
  // SaveState;
end;

procedure TEmc.Resume;
begin
  UpdateState;
  if (State.Mode <> TASKMODEAUTO) then Exit;
  if State.InterpState = INTERP_PAUSED then
    sendProgramResume;
end;

procedure TEmc.Stop;
begin
  if State.Mode = TASKMODEAUTO then
    begin
      // this is a test for the reentry functionality of mocca
      // we ll have a short delay on a Stop command but we need
      // to pause the interpreter first to get the correct motion line
      // Disabled in this release
      // Pause;
      // now execute the normal code for a stop command
      sendAbort;
      WaitDone;
    end;
end;

procedure TEmc.TouchOff(Axis: Char);
begin
  if State.Mode <> TASKMODEMANUAL then
    Exit;
  DoTouchOff(Axis);
  clRun.UpdatePreview(True);
end;

function TEMC.HandleCommand(Cmd: integer): boolean;
begin
  case Cmd of
    cmCLOSE:
      begin
        Application.MainForm.Close;
      end;
    cmABORT: sendAbort;
    cmESTOP:
      begin
        sendAbort;
        WaitDone;
        if State.State <> STATE_ESTOP then
          begin
            SendEStop;
            WaitDone;
          end
        else
          SendEStopReset;
      end;
    cmMACHINE:
      if State.State = STATE_ON then
        SendMachineOff
      else
        if State.State <> STATE_ESTOP then
          SendMachineOn;
    cmJOG: SetTaskMode(TASKMODEMANUAL);
    cmAUTO: SetTaskMode(TASKMODEAUTO);
    cmMDI: SetTaskMode(TASKMODEMDI);
    cmFEEDRESET: FeedOverride:= 100;
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
    cmREFX: Joints.HomeAxis('X');
    cmREFY: Joints.HomeAxis('Y');
    cmREFZ: Joints.HomeAxis('Z');
    cmREFA: Joints.HomeAxis('A');
    cmREFB: Joints.HomeAxis('B');
    cmREFC: Joints.HomeAxis('C');
    cmREFALL: Joints.HomeAll;
    cmUNREF: Joints.UnHomeActive;
    cmZEROALL: SetCoordsZero;
    cmTOUCHX: TouchOff('X');
    cmTOUCHY: TouchOff('Y');
    cmTOUCHZ: TouchOff('Z');
    cmTOUCHA: TouchOff('A');
    cmTOUCHB: TouchOff('B');
    cmTOUCHC: TouchOff('C');
    cmTOUCHWIZ: TouchOffWiz;
    cmLIMITS: SetORideLimits(not State.ORideLimits);
    cmOFFSDLG:
      begin
        EditOffsets;
        clRun.UpdatePreview(True);
      end;
    cmTOOLEDT: 
       begin
         EditTools;
         LoadTools;
         clRun.UpdatePreview(True);
       end;
    cmTOOLCHG: ChangeTool;
    cmEDITOR: EditFile;
    cmCOORDROT: DoCoordRotate;
    cmONANDREF:
      if State.State = STATE_ESTOP then
        begin
          SendEStopReset;
          Sleep(20);
          SendMachineOn;
          Sleep(20);
          Joints.HomeAll;
        end;
    cmUNITS: SetDisplayUnits(not Vars.ShowMetric);
    cmVIEWREL: Dro.Relative:= not Dro.Relative;
    cmVIEWDTG: Dro.Dtg:= not Dro.Dtg;


  else
    begin
      Result:= False;
      Exit; // we do exit here cause the command is not handled...
    end;
  end; //case
  Result:= true;

end;
  
end.

