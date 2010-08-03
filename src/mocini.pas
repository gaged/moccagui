unit mocini;

{$I mocca.inc}

interface

uses
  Classes, SysUtils; 

function IniRead(FileName: string): Boolean;

implementation

uses
  mocglb,emc2pas,emcint;

const
  ERR_INI_OPEN = 'Cannot open inifile:';
  MSG_INI_READ_FROM = 'mocca reads from :';
  {$ifdef VER_24}
  MSG_NMLFILE = 'Default NMLFile = ';
  {$endif}
  {$ifdef VER_23}
  ERR_NO_NMLFILE = 'Cannot find NMLFILE';
  {$endif}
  ERR_SPINDLEOVERRIDE = 'Error in inifile: MIN_SPINDLE_OVERRIDE > MAX_SPINDLE_OVERRIDE';
  ERR_MAXSPINDLEORIDE = 'Error in inifile: MAX_SPINDLE_OVERRIDE < 1.0';
  ERR_NOUNITS = 'Linear units not defined in inifile, using mm as default units';
  MSG_INIMETRIC = 'Setup is metric';
  MSG_INIINCHES = 'Setup is inches';
  ERR_ININOCONFIG1 = 'Error: There is no entry "CONFIG" in the Emc2 inifile.';
  ERR_ININOCONFIG2 = 'Mocca needs the file "config.xml" to start.';
  ERR_ININOCONFIG3 = 'Please check your installation of mocca.';
  MSG_CONFIGDIR = 'Config path is: ';

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
  Result:= False;
  if iniGet(PChar(secstr),PChar(varstr),PChar(Buffer)) then
    begin
      s:= PChar(Buffer);
      Result:= True;
    end
  else
    s:= defval;
end;

function GetIniDouble(secstr,varstr: string;var d: double; defval: Double): Boolean;
var
  s: string;
begin
  Result:= False;
  s:= '';
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
  S:= '';
  if GetIniStr(secstr,varstr,s,'') then
    if s <> '' then
      begin
        i:= StrToInt(s);
        Result:= true;
      end;
  if Result = false then
    i:= defval;
end;

function IniRead(FileName: string): Boolean;
var
  d: Double;
  tmp: string;
  i: integer;
begin
  Result:= false;
  tmp:= ''; d:= 0; i:= 0;

  {$ifdef DEBUG_INI}
    writeln('Ini: Inifile: ' + Filename);
  {$endif}

  // try to open the ini-file, filename is passed by paramstr(2)
  if not IniOpen(PChar(FileName)) then
    begin
      writeln(ERR_INI_OPEN + '"' + FileName + '"');
      Exit;
    end;

  Vars.IniFile:= FileName;
  Vars.IniPath:= ExtractFilePath(Vars.IniFile);

  writeln(MSG_INI_READ_FROM,Vars.IniPath);

  // first we try to find the nml-file
  // emc2-2.4 uses the default NML_FILE;
  // emc2-2.3 needs the NML_FILE set up in the ini-file
  {$ifdef VER_24}
  writeln(MSG_NMLFILE, Emc2NmlFile);
  EMC_NMLFILE:= PChar(Emc2NmlFile);
  {$endif}
  if GetIniStr('EMC','NML_FILE',tmp,'') then
    EMC_NMLFILE:= PChar(tmp)
  else
    begin
      {$ifdef VER_23}
      writeln(ERR_NO_NMLFILE);
      Exit;
      {$endif}
    end;
  {$ifdef DEBUG_INI}
  writeln('Ini: NMLFile: ' + PChar(EMC_NMLFILE));
  {$endif}

  Vars.ToolFile:= '';
  Vars.ParamFile:= '';

  GetIniStr('DISPLAY','PROGRAM_PREFIX',Vars.ProgramPrefix,'');
  {$ifdef DEBUG_INI}
  writeln('PROGRAM_PREFIX: ' + Vars.ProgramPrefix);
  {$endif}

  GetIniStr('EMC','MACHINE',Vars.Machine,'');
  GetIniStr('FILTER','PROGRAM_EXTENSION', Vars.Extensions,'');
  GetIniStr('HAL','POSTGUI_HALFILE',Vars.PostGuiHalfile,'');

  GetIniDouble('DISPLAY','MAX_FEED_OVERRIDE',d,1);
  Vars.MaxFeedOverride:= Round(d * 100);

  // Max-Spindle
  GetIniDouble('DISPLAY','MAX_SPINDLE_OVERRIDE',d,1);
  Vars.MaxSpORide:= Round(d * 100);

  GetIniDouble('DISPLAY','MIN_SPINDLE_OVERRIDE',d,0);
  Vars.MinSpORide:= Round(d * 100);
  if (Vars.MaxSpORide <> 0) and (Vars.MinSpORide <> 0) then
    begin
      if Vars.MinSpORide >= Vars.MaxSpORide then
        begin
          writeln(ERR_SPINDLEOVERRIDE);
          Exit;
        end;
      if Vars.MaxSpORide < 100 then
        begin
          writeln(ERR_MAXSPINDLEORIDE);
          Exit;
        end;
    end;
  State.ActSpORide:= 100;

  GetIniStr('DISPLAY','GEOMETRY',tmp,'');
  Vars.Geometry:= tmp;

  if not GetIniDouble('DISPLAY','DEFAULT_LINEAR_VELOCITY',d,1) then
    if not GetIniDouble('TRAJ', 'DEFAULT_LINEAR_VELOCITY',d,1) then
     GetIniDouble('TRAJ', 'DEFAULT_VELOCITY',d,0);
  Vars.LinearJogSpeed:= d * 60;

  if not GetIniDouble('DISPLAY','DEFAULT_ANGULAR_VELOCITY',d,1) then
    if not GetIniDouble('TRAJ', 'DEFAULT_ANGULAR_VELOCITY',d,1) then
      GetIniDouble('TRAJ', 'DEFAULT_VELOCITY',d,1);
  Vars.AngularJogSpeed:= d * 60;

  if not GetIniDouble('DISPLAY','MAX_LINEAR_VELOCITY',d,1) then
    if not GetIniDouble('TRAJ','MAX_LINEAR_VELOCITY',d,1) then
      GetIniDouble('TRAJ','MAX_VELOCITY',d,1);
  Vars.MaxLinearVel:= d * 60;

  {$ifdef DEBUG_INI}
  writeln('Ini: MaxLinearVel ' + FloatToStr(Vars.MaxLinearVel));
  {$endif}

  if Vars.LinearJogSpeed < 0.0001 then
    Vars.LinearJogSpeed:= Vars.MaxLinearVel;
  if Vars.LinearJogSpeed > Vars.MaxLinearVel then
    Vars.LinearJogSpeed:= Vars.MaxLinearVel;

  State.MaxJogVel:= Round(Vars.MaxLinearVel);
  State.ActJogVel:= Round(Vars.LinearJogSpeed);
  State.MaxVel:= Round(Vars.MaxLinearVel);
  State.ActVel:= State.MaxVel;

  if not GetIniDouble('DISPLAY','MAX_ANGULAR_VELOCITY',d,1) then
    if not GetIniDouble('TRAJ','MAX_ANGULAR_VELOCITY',d,1) then
      GetIniDouble('TRAJ','MAX_VELOCITY',d,1);
  Vars.MaxAngularVel:= d * 60;

  Vars.ShowMetric:= True;

  GetIniStr('TRAJ','LINEAR_UNITS',tmp,'');
  if Tmp <> '' then
    begin
      StripBlank(tmp);
      tmp:= UpperCase(tmp);
      if (tmp = 'INCH') or (tmp = 'IN') then
        Vars.ShowMetric:= False;
    end
  else
    writeln(ERR_NOUNITS);

  if Vars.ShowMetric then
    Vars.UnitStr:= 'mm'
  else
    Vars.UnitStr:= 'in';

  Vars.UnitVelStr:= Vars.UnitStr + '/min';

  if Vars.ShowMetric then
    writeln(MSG_INIMETRIC)
  else
    writeln(MSG_INIINCHES);

  if Vars.ShowMetric then
     EdgeFinderDia:= 5
  else
    EdgeFinderDia:= 0.1;

  GetIniDouble('DISPLAY','CYCLE_TIME',d,0.2);
  i:= Round(d/1000);
  if (i > 500) then i:= 500;
  if (i < 50) then i := 50;
  Vars.CycleDelay:= i;

  GetIniStr('EMCIO','TOOL_TABLE',tmp,'');
  if Length(tmp) > 0 then
    Vars.ToolFile:= Vars.IniPath + tmp;

  GetIniStr('RS274NGC','PARAMETER_FILE',tmp,'');
  if Length(tmp) > 0 then
    Vars.ParamFile:= Vars.IniPath + tmp;

  GetIniStr('RS274NGC','RS274NGC_STARTUP_CODE',tmp,'');
  if Length(tmp) > 0 then
    Vars.InitCode:= tmp;

  {$ifdef DEBUG_INI}
  writeln('Ini: Toolfile: ',Vars.ToolFile);
  writeln('Ini: Paramfile: ',Vars.ParamFile);
  {$endif}

  Vars.HomingOrderDefined:= GetIniStr('AXIS_0','HOME_SEQUENCE',tmp,'');

  Vars.NoForceHoming:= False;
  if GetIniInt('TRAJ','NO_FORCE_HOMING',i,0) then
    Vars.NoForceHoming:= i <> 0;

  if GetIniStr('DISPLAY','POSITION_OFFSET',tmp,'') then
    begin
      tmp:= UpperCase(Trim(tmp));
      Vars.ShowRelative:= (tmp = 'RELATIVE') or (tmp = 'REL');
    end
  else
    Vars.ShowRelative:= True;

  if GetIniStr('DISPLAY','POSITION_FEEDBACK',tmp,'') then
    begin
      tmp:= UpperCase(Trim(tmp));
      Vars.ShowActual:= (tmp = 'ACTUAL') or (tmp = 'ACT');
     end
  else
    Vars.ShowActual:= True;

  for i:= 0 to Vars.NumAxes - 1 do
    begin
      tmp:= 'AXIS_' + IntToStr(i);
      GetIniInt(tmp,'JOGGING_POLARITY',Vars.jogPolarity[i],0);
    end;

  // GetIniStr('DISPLAY','INCREMENTS',tmp,'');
  // SetJogIncrements(tmp);

  GetIniStr('DISPLAY','EDITOR',tmp,'');
  Vars.Editor:= Trim(tmp);

  Vars.IsLathe:= False;
  if GetIniInt('DISPLAY','LATHE',i,0) then
    if i = 1 then Vars.IsLathe:= True;

  GetIniStr('MOCCA','CONFIG',tmp,'');
  ConfigDir:= Trim(tmp);

  if ConfigDir = '' then
    begin
      writeln(ERR_ININOCONFIG1);
      writeln(ERR_ININOCONFIG2);    
      writeln(ERR_ININOCONFIG3);
      Exit;
    end;

  i:= Length(ConfigDir);
  if i > 0 then
    begin
      if ConfigDir[i] <> '/' then
        ConfigDir:= ConfigDir + '/';
    end;

  writeln(MSG_CONFIGDIR + ConfigDir);

  GetIniInt('MOCCA','DEFAULT_LAYOUT',i,0);
  if i > 0 then
    UseDefaultLayout:= True;

  IniClose;
  Result:= True;
end;


end.




