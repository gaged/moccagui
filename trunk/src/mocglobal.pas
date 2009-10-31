unit mocglobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Forms,ExtCtrls,StdCtrls;
  
const
  MaxAxes = 5;                  // maimum number of axes, default 5 XYZCB

type
  EVars = record

    ProgramPrefix: string;        // program_prefix from ini
    Machine: string;              // the machine as defined in inifile
    PostGuiHalfile: string;
    ToolTblFile: string;
    ParamFile: string;
    Extensions: string;
    Geometry: string;

    NumAxes: Integer;             // number of axes in ini
    CoordNames: string;           // the given coordinates (XYZ...)

    CycleTime: Double;            // as defined in [DISPLAY], in seconds
    CycleDelay: Longint;          // cycletime/1000 in msec

    MaxFeedOverride: integer;     // maximum feed override
    MaxSpindleOverride: integer;  // maximum spindle override

    LinearJogSpeed: double;
    AngularJogSpeed: double;
    
    MaxLinearVel: double;
    MaxAngularVel: double;
    
    IsMetric: Boolean;            // metric, default true **FIXME

    UnitVelStr: string;           // linear velocitystr i.e. "inch/min"
    UnitStr: string;              // linear units i.e. inch "in","mm"

    // position display
    ShowActual: Boolean;          // actual or cmd position
    ShowRelative: Boolean;        // relative or absolute position

    HomingOrderDefined: Boolean;  // check if can do a "home all"
  end;
  
var

  E: EVars;
  EStopped: Boolean;
  MachineOn,
  FloodOn,
  MistOn,
  BrakeOn: Boolean;

  LastError: string[255];       // updated by checkerror
  UpdateLock: Boolean;          // used to prevent controls to be updated

  ActTaskMode: Integer;         // current taskmode, used in (update)

  MaxMaxVelocity: Integer;
  ActMaxVelocity: Integer;
  OldMaxVelocity: integer;

  ActFeedOride: Integer;
  OldFeedOride: integer;
  ActSpindleOride: Integer;
  OldSpindleORide: integer;

  ActJogSpeed: integer;
  OldJogSpeed: integer;

  ActAngJogSpeed: integer;
  OldAngJogSpeed: integer;
  
  MaxJogSpeed: Integer;         // linear max jogspeed
  MinJogSpeed: integer;
  MaxAngJogSpeed: Integer;      // angular max jogspeed
  MinAngJogSpeed: integer;

  ProgramFile: PChar;           // a long string;

type
  TJointDef = record
    Id: Integer;                // the joint id
    Designator: Char;           // the joints axis designator
    Inverted: Boolean;          // invert jog- direction
    Jogging: Boolean;           // is jogging called by "dojog"
    JogTimer: Longint;          // timer for smootjog- feature
    JogSmoothStart: Integer;    // Initial velocity for smooth-jog
    JogAccel: integer;          // acceleration for smoothjog
    Homed: Boolean;
  end;
  
type
  TJoints = Array[0..MaxAxes-1] of TJointDef;
  
type
  TJogIncs = record
    Txt: string;
    Val: Double;
  end;
  
var
  Joints: TJoints;

  JogInc: Array[0..6] of TJogIncs;  // fixme!! hardcoded
  JogIncCount: Integer;
  JogActiveInc: Integer;

function LoadSetupFromIni: Boolean;
function CheckError: Boolean;
function PosToString(Value: Double): string;
function AxisId(Axis: Char): integer;
function AxisChar(Axis: Integer): Char;

implementation

uses
  emc2pas;
  
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
  
function PosToString(Value: Double): string;
var
  s: string;
begin
  if Value >= 0 then S:= '+';
  S:= S + FloatToStrF(Value, ffFixed, 8, 3);
  Result:= S;
end;

function AxisId(Axis: Char): integer;
var
  i: integer;
begin
  i:= Pos(Axis,E.CoordNames);
  if i > 0 then
    Result:= i-1
  else
    Result:= -1;
end;

function AxisChar(Axis: Integer): Char;
begin
  if (Axis < 0) or (Axis > Length(E.CoordNames) - 1) then
    Result:= #0
  else
    Result:= E.CoordNames[Axis-1];
end;

function StripBlank(var S: string): Boolean;
var
  i: integer;
  Tmp: string;
begin
  tmp:= '';
  Result:= False;
  if Length(S) < 1 then Exit;
  for i:= 1 to Length(S) do
    if S[i] <> #32 then
      Tmp:= Tmp + S[i];
  S:= Tmp;
  Result:= Length(S) > 0;
end;

function GetIniStr(secstr,varstr: string; var s: string): Boolean;
var
  Buffer: Array[0..LINELEN-1] of Char;
begin
  if (iniGet(PChar(secstr),PChar(varstr),PChar(Buffer)) = 0) then
    begin
      s:= PChar(Buffer);
      Result:= True;
    end
  else
    begin
      s:= '';
      Result:= False;
    end;
end;

function GetIniDouble(secstr,varstr: string; var d: double): Boolean;
var
  s: string;
begin
  Result:= False;
  if not GetIniStr(secstr,varstr,s) then
    Exit;
  if s <> '' then
    begin
      d:= StrToFloat(s);
      Result:= True;
    end
  else
    d:= 0;
end;

function GetIniInt(secstr,varstr: string; var i: integer): Boolean;
var
  s: string;
begin
  Result:= False;
  if not GetIniStr(secstr,varstr,s) then
    Exit;
  if s <> '' then
    begin
      i:= StrToInt(s);
      Result:= true;
    end;
end;

procedure GetJogIncrements;
var
  tmp: string;
begin
  GetIniStr('DISPLAY','INCREMENTS',tmp);
  // Fixme
  JogInc[0].Txt:= 'continous';
  JogInc[0].Val:= 0;
  JogInc[1].Txt:= '0.001';
  JogInc[1].Val:= 0.001;
  JogInc[2].Txt:= '0.01';
  JogInc[2].Val:= 0.01;
  JogInc[3].Txt:= '0.1';
  JogInc[3].Val:= 0.1;
  JogInc[4].Txt:= '1';
  JogInc[4].Val:= 1;
  JogInc[5].Txt:= '10';
  JogInc[5].Val:= 10;
  JogIncCount:= 6;
end;

function LoadSetupFromIni: Boolean;
var
  d: Double;
  tmp: string;
  i: integer;
begin
  Result:= false;
  
  E.ToolTblFile:= 'emc.tbl';
  E.ParamFile:= 'emc.var';

  GetIniStr('DISPLAY','PROGRAM_PREXIX',E.ProgramPrefix);
  GetIniStr('EMC','MACHINE',E.Machine);
  GetIniStr('FILTER','PROGRAM_EXTENSION', E.Extensions);
  GetIniStr('HAL','POSTGUI_HALFILE',E.PostguiHalfile);
  
  if not GetIniDouble('DISPLAY','MAX_FEED_OVERRIDE',d) then
    d:= 1;
  E.MaxFeedOverride:= Round(d * 100);

  if not GetIniDouble('DISPLAY','MAX_SPINDLE_OVERRIDE',d) then
    d:= 1;
  E.MaxSpindleOverride:= Round(d * 100);

  if not GetIniStr('DISPLAY','GEOMETRY',tmp) then
    tmp:= 'XYZBC';
  if Length(tmp) < 1 then Exit;  // no geometry
  E.Geometry:= tmp;

  if not GetIniDouble('DISPLAY','DEFAULT_LINEAR_VELOCITY',d) then
    if not GetIniDouble('TRAJ', 'DEFAULT_LINEAR_VELOCITY',d) then
     if not GetIniDouble('TRAJ', 'DEFAULT_VELOCITY',d) then
       d:= 1.0; // Default 1.0, no Settings found

  E.LinearJogSpeed:= d * 60;

  if not GetIniDouble('DISPLAY','DEFAULT_ANGULAR_VELOCITY',d) then
    if not GetIniDouble('TRAJ', 'DEFAULT_ANGULAR_VELOCITY',d) then
      if not GetIniDouble('TRAJ', 'DEFAULT_VELOCITY',d) then
        d:= 1.0;
        
  E.AngularJogSpeed:= d * 60;
     
  if not GetIniDouble('DISPLAY','MAX_LINEAR_VELOCITY',d) then
    if not GetIniDouble('TRAJ','MAX_LINEAR_VELOCITY',d) then
      if not GetIniDouble('TRAJ','MAX_VELOCITY',d) then
        d:= 1.0;
  E.MaxLinearVel:= d * 60;

  if not GetIniDouble('DISPLAY','MAX_ANGULAR_VELOCITY',d) then
    if not GetIniDouble('TRAJ','MAX_ANGULAR_VELOCITY',d) then
      if not GetIniDouble('TRAJ','MAX_VELOCITY',d) then
        d:= 1.0;
  E.MaxAngularVel:= d * 60;


  GetIniStr('TRAJ','LINEAR_UNITS',tmp);
   if Tmp = '' then
     E.IsMetric:= True
   else
       // Fixme
     E.IsMetric:= True;
  if E.IsMetric then
    begin
      E.UnitVelStr:= 'mm/min';
      E.UnitStr:= 'mm';
    end
  else
    begin
      E.UnitVelStr:= 'in/min';
      E.UnitStr:= 'in';
    end;
    
  if not GetIniInt('TRAJ','AXES',i) then
    i:= 0;
  if (i < 1) or (i > 5) then
    begin
      writeln('Number of Axes not defined or zero.');
      Exit;
    end;
  E.NumAxes:= i;

  if GetIniStr('TRAJ','COORDINATES',tmp) then
    begin
      if not StripBlank(tmp) then tmp:= '';
      if Length(tmp) <> E.NumAxes then
        begin
          writeln('missmatch in number of joints and coord names');
          Exit;
        end;
    end;
  E.CoordNames:= tmp;

  if not GetIniDouble('DISPLAY','CYCLE_TIME',d) then
    d:= 0.2; // default cycletime
  i:= Round(d/1000);
  if (i > 500) then i:= 500;
  if (i < 50) then i := 50;
  E.CycleDelay:= i;
  
  GetIniStr('EMCIO','TOOL_TABLE',E.ToolTblFile);
  GetIniStr('RS274NGC','PARAMETER_FILE',E.ParamFile);

  GetJogIncrements;

  E.HomingOrderDefined:= GetIniStr('AXIS_0','HOME_SEQUENCE',tmp);

  if E.LinearJogSpeed > E.MaxLinearVel then
    E.LinearJogSpeed:= E.MaxLinearVel;

  MaxJogSpeed:= Round(E.MaxLinearVel);
  MinJogSpeed:= Round(0);  // Fixme, should be minlinearvelocity
  ActJogSpeed:= Round(E.LinearJogSpeed);
  OldJogSpeed:= ActJogSpeed;

  MaxMaxVelocity:= Round(E.MaxLinearVel);
  ActMaxVelocity:= MaxMaxVelocity;
  
  Result:= True;
end;

begin
  E.NumAxes:= 0;
  E.CoordNames:= '';
  E.CycleTime:= 0.02;
  E.CycleDelay:= 100;
  E.ToolTblFile:= '';
  E.ParamFile:= '';
  ActTaskMode:= -1;
end.

