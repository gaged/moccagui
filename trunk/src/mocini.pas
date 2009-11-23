unit mocini;

{$mode objfpc}
{$I mocca.inc}


interface

uses
  Classes, SysUtils; 

function IniRead(FileName: string): Boolean;

implementation

uses
  mocglb,emc2pas;
  
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

function GetIniStr(secstr,varstr: string; var s: string; defval: string): Boolean;
var
  Buffer: Array[0..LINELEN-1] of Char;
begin
  if iniGet(PChar(secstr),PChar(varstr),PChar(Buffer)) then
    begin
      s:= PChar(Buffer);
      Result:= True;
    end
  else
    begin
      s:= defval;
      Result:= False;
    end;
end;

function GetIniDouble(secstr,varstr: string;var d: double; defval: Double): Boolean;
var
  s: string;
begin
  Result:= False;
  if GetIniStr(secstr,varstr,s,'') then
    if s <> '' then
      begin
        d:= StrToFloat(s);
        Result:= true;
      end;
  if Result = False then
    d:= defval;
end;

function GetIniInt(secstr,varstr: string; var i: integer; defval: integer): Boolean;
var
  s: string;
begin
  Result:= False;
  if GetIniStr(secstr,varstr,s,'') then
    if s <> '' then
      begin
        i:= StrToInt(s);
        Result:= true;
      end;
  if Result = false then
    i:= defval;
end;

procedure SetJogIncrements(s: string);
var
  tmp: string;
begin
  tmp:= '';
  if tmp = '' then
    begin
      with emcVars do
        begin
          JogIncrements[0].Text:= 'Durchgehend';
          JogIncrements[0].Value:=  0;
          JogIncrements[1].Text:= '0.001';
          JogIncrements[1].Value:=  0.001;
          JogIncrements[2].Text:= '0.01';
          JogIncrements[2].Value:=  0.01;
          JogIncrements[3].Text:= '0.1';
          JogIncrements[3].Value:=  0.1;
          JogIncrements[4].Text:= '1';
          JogIncrements[4].Value:=  1;
          JogIncrements[5].Text:= '10';
          JogIncrements[5].Value:=  10;
          JogIncMax:= 5;
        end;
    end
  else
    begin
      // Fixme
    end;
end;

function IniRead(FileName: string): Boolean;
var
  d: Double;
  tmp: string;
  i: integer;
begin
  Result:= false;
  {$ifdef DEBUG_INI}
    writeln('Ini: Inifile: ' + Filename);
  {$endif}
  // try to open the ini-file, filename is passed by paramstr(2)
  if not IniOpen(PChar(FileName)) then
    begin
      writeln('Cannot open inifile: "' + FileName + '"');
      Exit;
    end;

  // first we try to find the nml-file
  if GetIniStr('EMC','NML_FILE',tmp,'') then
    begin
      EMC_NMLFILE:= PChar(tmp);
    end
  else
    begin
      Writeln('Cannot find NMLFILE');
      Exit;
    end;

  {$ifdef DEBUG_INI}
    writeln('Ini: NMLFile: ' + PChar(EMC_NMLFILE));
  {$endif}

  emcVars.ToolTblFile:= 'emc.tbl';
  emcVars.ParamFile:= 'emc.var';

  GetIniStr('DISPLAY','PROGRAM_PREXIX',emcVars.ProgramPrefix,'');

  GetIniStr('EMC','MACHINE',emcVars.Machine,'');

  GetIniStr('FILTER','PROGRAM_EXTENSION', emcVars.Extensions,'');

  GetIniStr('HAL','POSTGUI_HALFILE',emcVars.PostguiHalfile,'');

  GetIniDouble('DISPLAY','MAX_FEED_OVERRIDE',d,1);
  emcVars.MaxFeedOverride:= Round(d * 100);

  GetIniDouble('DISPLAY','MAX_SPINDLE_OVERRIDE',d,1);
  emcVars.MaxSpindleOverride:= Round(d * 100);

  if not GetIniStr('DISPLAY','GEOMETRY',tmp,'XYZBC') then
  if Length(tmp) < 1 then Exit;  // no geometry
  emcVars.Geometry:= tmp;

  if not GetIniDouble('DISPLAY','DEFAULT_LINEAR_VELOCITY',d,1) then
    if not GetIniDouble('TRAJ', 'DEFAULT_LINEAR_VELOCITY',d,1) then
     GetIniDouble('TRAJ', 'DEFAULT_VELOCITY',d,1);
  emcVars.LinearJogSpeed:= d * 60;

  if not GetIniDouble('DISPLAY','DEFAULT_ANGULAR_VELOCITY',d,1) then
    if not GetIniDouble('TRAJ', 'DEFAULT_ANGULAR_VELOCITY',d,1) then
      GetIniDouble('TRAJ', 'DEFAULT_VELOCITY',d,1);
  emcVars.AngularJogSpeed:= d * 60;

  if not GetIniDouble('DISPLAY','MAX_LINEAR_VELOCITY',d,1) then
    if not GetIniDouble('TRAJ','MAX_LINEAR_VELOCITY',d,1) then
      GetIniDouble('TRAJ','MAX_VELOCITY',d,1);
  emcVars.MaxLinearVel:= d * 60;

  if not GetIniDouble('DISPLAY','MAX_ANGULAR_VELOCITY',d,1) then
    if not GetIniDouble('TRAJ','MAX_ANGULAR_VELOCITY',d,1) then
      GetIniDouble('TRAJ','MAX_VELOCITY',d,1);
  emcVars.MaxAngularVel:= d * 60;

  LinearUnitConversion:= 0;
  GetIniStr('TRAJ','LINEAR_UNITS',tmp,'');
  if Tmp = '' then
    begin
      emcVars.IsMetric:= True;
      linearUnitConversion:= LINEAR_UNITS_MM;
    end
  else
    begin
      StripBlank(tmp);
      tmp:= UpperCase(tmp);
      if tmp = 'INCH' then LinearUnitConversion:= LINEAR_UNITS_INCH;
      if tmp = 'MM' then LinearUnitConversion:= LINEAR_UNITS_MM;
      if tmp = 'CM' then LinearUnitConversion:= LINEAR_UNITS_CM;
    end;
  if linearUnitConversion = 0 then
    begin
      writeln('Linear Units not defined.');
      Exit;
    end;

  // *todo: angularUnitsConversion

  GetIniInt('TRAJ','AXES',i,0);
  if (i < 1) or (i > 9) then
    begin
      writeln('Number of Axes not defined or zero.');
      Exit;
    end;
  emcVars.NumAxes:= i;

  if GetIniStr('TRAJ','COORDINATES',tmp,'') then
    begin
      if not StripBlank(tmp) then tmp:= '';
      if Length(tmp) <> emcVars.NumAxes then
        begin
          writeln('missmatch in number of joints and coord names');
          Exit;
        end;
    end;
  emcVars.CoordNames:= tmp;
  
  {$ifdef DEBUG_INI}
    writeln('Ini: Coordinates ' + emcVars.CoordNames);
  {$endif}


  GetIniDouble('DISPLAY','CYCLE_TIME',d,0.2);
  i:= Round(d/1000);
  if (i > 500) then i:= 500;
  if (i < 50) then i := 50;
  emcVars.CycleDelay:= i;

  GetIniStr('EMCIO','TOOL_TABLE',emcVars.ToolTblFile,'tool.tbl');
  GetIniStr('RS274NGC','PARAMETER_FILE',emcVars.ParamFile,'');

  {$ifdef DEBUG_INI}
    writeln('Ini: Tooltablefile: ' + emcVars.ToolTblFile);
    writeln('Ini: Paramfile: ' + emcVars.ParamFile);
  {$endif}


  emcVars.HomingOrderDefined:= GetIniStr('AXIS_0','HOME_SEQUENCE',tmp,'');

  if emcVars.LinearJogSpeed > emcVars.MaxLinearVel then
    emcVars.LinearJogSpeed:= emcVars.MaxLinearVel;

  emcState.MaxJogVel:= Round(emcVars.MaxLinearVel);
  emcState.MinJogVel:= Round(0);  // Fixme, should be minlinearvelocity
  emcState.ActJogVel:= emcState.MaxJogVel;

  emcState.MaxVel:= Round(emcVars.MaxLinearVel);
  emcState.ActVel:= emcState.MaxVel;

  emcVars.UnitStr:= '?';

  if linearUnitConversion = 0 then
    linearUnitConversion:= LINEAR_UNITS_MM;

  case linearUnitConversion of
    LINEAR_UNITS_MM: emcVars.UnitStr:= 'mm';
    LINEAR_UNITS_INCH: emcVars.UnitStr:= 'in';
    LINEAR_UNITS_CM: emcVars.UnitStr:= 'cm';
  end;

  emcVars.UnitVelStr:= emcVars.UnitStr + '/min';
  
  for i:= 0 to emcVars.NumAxes - 1 do
    begin
      tmp:= 'AXIS_' + IntToStr(i);
      GetIniInt(tmp,'JOGGING_POLARITY',emcVars.jogPolarity[i],0);
    end;
    
  GetIniStr('DISPLAY','INCREMENTS',tmp,'');
  SetJogIncrements(tmp);
  
  Result:= True;
end;


end.




