unit mocglobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Forms,ExtCtrls,StdCtrls;
  
const
  MaxAxes = 5;                  // maimum number of axes, default 5 XYZCB

   TaskModeManual = 1;
   TaskModeAuto   = 2;
   TaskModeMDI    = 3;

var
  NumAxes: Integer;             // number of axes in ini
  CoordNames: string;           // the given coordinates (XYZ...)

  CycleTime: Double;            // as defined in [DISPLAY], in seconds
  CycleDelay: Longint;          // cycletime/1000 in msec

  TaskMode: Integer;            // current taskmode
  LastError: string[255];
  UpdateLock: Boolean;          // used to prevent controls to be updated
  Machine: string;              // the machine as defined in inifile

  PostGuiHalfile: string;
  ToolTblFile: string;
  ParamFile: string;

  MaxSpindleOverride,
  SpindleOverride: Integer;

  MaxFeedOverride,
  FeedOverride: Integer;

  MaxJogSpeed: Integer;         // linear max jogspeed
  JogSpeed,                     // linear actual jogspeed
  AngularJogSpeed: Integer;     // angular actal jogspeed

  IsMetric: Boolean;            // metric, default true **FIXME

  UnitVelStr: string;           // linear velocitystr i.e. "inch/min"
  UnitStr: string;              // linear units i.e. inch "in","mm"

  // position display
  ShowActual: Boolean;          // actual or cmd position
  ShowRelative: Boolean;        // relative or absolute position

  EStopped: Boolean;
  MachineOn,
  FloodOn,
  MistOn,
  BrakeOn: Boolean;
  
type
  TJointDef = record
    Id: Integer;
    Designator: Char;
    Jogging: Boolean;
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
  i:= Pos(Axis,CoordNames);
  if i > 0 then
    Result:= i-1
  else
    Result:= -1;
end;

function AxisChar(Axis: Integer): Char;
begin
  if (Axis < 0) or (Axis > Length(CoordNames) - 1) then
    Result:= #0
  else
    Result:= CoordNames[Axis-1];
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

function GetCoordNames: Boolean;
begin
  Result:= False;
  if (NumAxes < 1) or (NumAxes > 9) then
    begin
      writeln('wrong Value for defined numer of joints.');
      writeln('there are ' + IntToStr(NumAxes) + ' defined.');
      exit;
    end;
  if StripBlank(CoordNames) then
    if Length(CoordNames) <> NumAxes then
    begin
      writeln('missmatch in number of joints and coord names');
      Exit;
    end;
  Result:= True;
end;

function CheckError: Boolean;
var
  Buffer: Array[0..LINELEN-1] of Char;
begin
  Result:= geterror(PChar(Buffer));
  if Result then
    LastError:= PChar(Buffer)
end;
  
function GetIniStr(secstr,varstr: string): string;
var
  Buffer: Array[0..LINELEN-1] of Char;
begin
  if (iniGet(PChar(secstr),PChar(varstr),PChar(Buffer)) = 0) then
    Result:= PChar(Buffer)
  else
    Result:= '';
end;

function GetIniDouble(secstr,varstr: string): double;
var
  s: string;
begin
  Result:= 0;
  s:= GetIniStr(secstr,varstr);
  if s <> '' then
    result:= StrToFloat(s);
end;

function GetIniInt(secstr,varstr: string): longint;
var
  s: string;
begin
  Result:= 0;
  s:= GetIniStr(secstr,varstr);
  if s <> '' then
    result:= StrToInt(s);
end;

procedure GetJogIncrements;
var
  tmp: string;
begin
  tmp:= GetIniStr('DISPLAY','INCREMENTS');
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
begin
  Result:= false;
  ToolTblFile:= 'emc.tbl';
  ParamFile:= 'emc.var';
  Machine:= GetIniStr('EMC','MACHINE');
  PostguiHalfile:= GetIniStr('HAL','POSTGUI_HALFILE');
  d:= GetIniDouble('DISPLAY', 'MAX_FEED_OVERRIDE') * 100;
  if d < 100 then
    d:= 100;  // Default 100%
  MaxFeedOverride:= Round(d);
  d:= GetIniDouble('DISPLAY','MAX_SPINDLE_OVERRIDE') * 100;
  if d < 100 then
    d:= 100;  // Default 100%
  MaxSpindleOverride:= Round(d);

  d:= GetIniDouble('DISPLAY','DEFAULT_LINEAR_VELOCITY');
  if d = 0 then
    d:= GetIniDouble('TRAJ', 'DEFAULT_LINEAR_VELOCITY');
  if d = 0 then
    d:= GetIniDouble('TRAJ', 'DEFAULT_VELOCITY');
  if d = 0 then
    d:= 1.0; // Default 1.0, no Settings found

  jogSpeed:= Round(d);
  MaxJogSpeed:= jogSpeed * 2;   // Fixme!

  d:= GetIniDouble('DISPLAY', 'DEFAULT_ANGULAR_VELOCITY');
  if d = 0 then
    d:= GetIniDouble('TRAJ', 'DEFAULT_ANGULAR_VELOCITY');
  if d = 0 then
    d:= GetIniDouble('TRAJ', 'DEFAULT_VELOCITY');

   if d < 1 then
     AngularJogSpeed:= JogSpeed // Fixme!
   else
     AngularJogSpeed:= Round(d);
     
  Tmp:= GetIniStr('TRAJ','LINEAR_UNITS');
   if Tmp = '' then
     IsMetric:= True
   else
     begin
       // Fixme
       IsMetric:= True;
     end;
  if IsMetric then
    begin
      UnitVelStr:= 'mm/min';
      UnitStr:= 'mm';
    end
  else
    begin
      UnitVelStr:= 'in/min';
      UnitStr:= 'in';
    end;
    
  NumAxes:= GetIniInt('TRAJ','AXES');
  if NumAxes < 1 then
    begin
      writeln('Number of Axes not defined or zero.');
      Exit;
    end;
  if NumAxes > 5 then
    begin
      writeln('More than 5 Axes are not allowed');
      Exit;
    end;
  CoordNames:= GetIniStr('TRAJ','COORDINATES');
  if Length(CoordNames) < 1 then
    begin
      writeln('no given coordinates in "TRAJ"- Section.');
      Exit;
    end;
  CycleTime:= GetIniDouble('DISPLAY','CYCLE_TIME');
  if CycleTime = 0 then
    CycleTime:= 0.2;
  CycleDelay:= Round(CycleTime/1000);
  if (CycleDelay > 500) then CycleDelay:= 500;
  if (CycleDelay < 50) then CycleDelay:= 50;
  ToolTblFile:= GetIniStr('EMCIO','TOOL_TABLE');
  ParamFile:= GetIniStr('RS274NGC','PARAMETER_FILE');

  if not GetCoordNames then
    Exit;

  GetJogIncrements;
    
  Result:= True;
end;

begin
  NumAxes:= 0;
  CoordNames:= '';
  CycleTime:= 0.02;
  CycleDelay:= 100;
  ToolTblFile:= '';
  ParamFile:= '';
  TaskMode:= -1;
end.

