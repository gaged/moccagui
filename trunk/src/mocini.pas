unit mocini;


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
  if GetIniStr(secstr,varstr,s,'') then
    if s <> '' then
      begin
        i:= StrToInt(s);
        Result:= true;
      end;
  if Result = false then
    i:= defval;
end;

(* modified for issue #13, Gx *)
function SetJogIncrements(s: string) : Boolean;
var
  i: integer; ii: integer; l : integer;
  s1 : string;
begin
  Result:= False;
  s1 := s;
  ii := 1;
  with Vars do
    begin
      JogIncrements[0].Text:= 'Durchgehend'; // continous
      repeat
         TrimLeft (s1);
         TrimRight (s1);
         i := Pos (' ', s1);	// blank is delimiter
         l := Length (s1);
         if i > 0 then begin
            JogIncrements[ii].Text := LeftStr (s1, i-1);     // item to store
            s1 := RightStr (s1, l-i);    // remaining part
         end
         else begin
            JogIncrements[ii].Text := s1;     // last item
         end;
         ii := ii+1;
      until (ii>MAXJOGINCS) or (i=0);     
      JogIncrements[0].Value:=  0;
      for i:=1 to (ii-1) do
        begin
          s1 := JogIncrements[i].Text;
          l := Pos('mm',s1) - 1;       // other units to add
          try
            if l > 0 then begin
             JogIncrements[i].Value:= StrToFloat(LeftStr(s1,l));
            end
            else begin
             JogIncrements[i].Value:= StrToFloat(s1);  // no known unit found
            end
          except // unknown unit
             writeln('unknown unit in INCREMENTS');
             Exit;
          end;
        end;
      JogIncMax:= ii-1;
      Result:= True;
    end
end;

function IniReadAxes: Boolean;
var
  Min,Max: Double;
  i: integer;
  Sec,tmp: string;
begin
  Result:= False;
  if (Vars.NumAxes < 1) or (Length(Vars.CoordNames) < 1) then
    begin
      writeln('No Axes defined or no coords given.');
      Exit;
    end;
  if Length(Vars.CoordNames) <> Vars.NumAxes then
    begin
      writeln('mismatch in geometry & number of axes.');
      Exit;
    end;
  for i:= 0 to Vars.NumAxes - 1 do
    begin
      Sec:= 'AXIS_' + IntToStr(i);

      GetIniStr(Sec,'TYPE',tmp,'');
      tmp:= UpCase(Trim(tmp));
      Vars.Axis[i].IsLinear:= True;
      Vars.Axis[i].AxisChar:= Vars.CoordNames[i+1];
      if Length(tmp) > 0 then
        if (tmp = 'ANGULAR') or (tmp = 'ANG') then
          Vars.Axis[i].IsLinear:= False;

      GetIniDouble(Sec,'MIN_LIMIT',Min,0);
      GetIniDouble(Sec,'MAX_LIMIT',Max,0);
      if Vars.Metric then
        begin
          Min:= Min / 25.4;
          Max:= Max / 25.4;
        end;

      case Vars.CoordNames[i+1] of
        'X': begin
               Vars.MLimits.MinX:= Min;
               Vars.MLimits.MaxX:= Max;
             end;
        'Y': begin
               Vars.MLimits.MinY:= Min;
               Vars.MLimits.MaxY:= Max;
             end;
        'Z': begin
               Vars.MLimits.MinZ:= Min;
               Vars.MLimits.MaxZ:= Max;
             end;
      end;
    end;
  Result:= True;
end;

function IniReadScripts: Boolean;
var
  s,n,sec: string;
  i: integer;
  NumScripts: integer;
begin
  NumScripts:= 0;
  Result:= False;
  with BtnDefScripts[0] do
    begin
      T:= cmBACK;
      G:= -1;
      S:= '<';
    end;
  for i:= 1 to NumSButtons - 1 do
    begin
      MocScripts[i].Name:= '';
      MocScripts[i].Script:= '';
      with BtnDefScripts[i] do
        begin
          T:= -1;
          G:= -1;
          S:= '';
        end;
      Sec:= 'SCRIPT_' + IntToStr(i-1);
      GetIniStr(Sec,'NAME',n,'');
      GetIniStr(Sec,'SCRIPT',s,'');
      if (s <> '') and (n <> '') then
          begin
            writeln(s,n);
            n:= Trim(n);
            s:= Trim(s);
            if (Length(s) > 0) and (LEngth(n) > 0) then
              begin
                MocScripts[i].Name:= n;
                MocScripts[i].Script:= s;
                BtnDefScripts[i].T:= cmSCRIPTBASE + i;
                BtnDefScripts[i].G:= -1;
                BtnDefScripts[i].S:= n;
                Inc(NumScripts);
              end;
          end;
    end;
  Result:= (NumScripts > 0);
end;

function IniRead(FileName: string): Boolean;
var
  d: Double;
  tmp: string;
  i: integer;
begin
  Result:= false;
  tmp:= ''; d:= 0;

  {$ifdef DEBUG_INI}
    writeln('Ini: Inifile: ' + Filename);
  {$endif}

  // try to open the ini-file, filename is passed by paramstr(2)
  if not IniOpen(PChar(FileName)) then
    begin
      writeln('Cannot open inifile: "' + FileName + '"');
      Exit;
    end;

  Vars.IniFile:= FileName;
  Vars.IniPath:= ExtractFilePath(Vars.IniFile);

  writeln('mocca reads from :',Vars.IniPath);

  // first we try to find the nml-file
  if GetIniStr('EMC','NML_FILE',tmp,'') then
    EMC_NMLFILE:= PChar(tmp)
  else
    begin
      Writeln('Cannot find NMLFILE');
      Exit;
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

  GetIniDouble('DISPLAY','MAX_SPINDLE_OVERRIDE',d,0);
  Vars.MaxSpORide:= Round(d * 100);

  GetIniDouble('DISPLAY','MIN_SPINDLE_OVERRIDE',d,0);
  Vars.MinSpORide:= Round(d * 100);
  if (Vars.MaxSpORide <> 0) and (Vars.MinSpORide <> 0) then
    begin
      if Vars.MinSpORide >= Vars.MaxSpORide then
        begin
          writeln('Error: MIN_SPINDLE_OVERRIDE > MAX_SPINDLE_OVERRIDE');
          Exit;
        end;
      if Vars.MaxSpORide < 100 then
        begin
          writeln('MAX_SPINDLE_OVERRIDE is less than 100%');
          Exit;
        end;
    end;
  State.ActSpORide:= 100;

  if not GetIniStr('DISPLAY','GEOMETRY',tmp,'XYZBC') then
  if Length(tmp) < 1 then Exit;  // no geometry
  Vars.Geometry:= tmp;

  if not GetIniDouble('DISPLAY','DEFAULT_LINEAR_VELOCITY',d,1) then
    if not GetIniDouble('TRAJ', 'DEFAULT_LINEAR_VELOCITY',d,1) then
     GetIniDouble('TRAJ', 'DEFAULT_VELOCITY',d,0);
  Vars.LinearJogSpeed:= d * 60;

  if not GetIniDouble('DISPLAY','DEFAULT_ANGULAR_VELOCITY',d,1) then
    if not GetIniDouble('TRAJ', 'DEFAULT_ANGULAR_VELOCITY',d,1) then
      GetIniDouble('TRAJ', 'DEFAULT_VELOCITY',d,1);
  Vars.AngularJogSpeed:= d * 60;

(* some modification from Gerhard Gleixner
   to seperate between max jog and max speed
   added member MaxLinearJogVel to Vars record
   issue #17
*)
  if not GetIniDouble('TRAJ','MAX_LINEAR_VELOCITY',d,1) then
    if not GetIniDouble('TRAJ','MAX_VELOCITY',d,1) then
      GetIniDouble('DISPLAY','MAX_LINEAR_VELOCITY',d,1); (* not final sol., eval AXIS *)
  Vars.MaxLinearVel:= d * 60;

  if not GetIniDouble('DISPLAY','MAX_LINEAR_VELOCITY',d,1) then
    if not GetIniDouble('DISPLAY','MAX_VELOCITY',d,1) then
     if not GetIniDouble('TRAJ','MAX_LINEAR_VELOCITY',d,1) then
      GetIniDouble('TRAJ','MAX_VELOCITY',d,1);
  Vars.MaxLinearJogVel:= d * 60;
(* end mod *)

  {$ifdef DEBUG_INI}
  writeln('Ini: MaxLinearVel ' + FloatToStr(Vars.MaxLinearVel));
  {$endif}

  if Vars.LinearJogSpeed < 0.0001 then
    Vars.LinearJogSpeed:= Vars.MaxLinearVel;
  if Vars.LinearJogSpeed > Vars.MaxLinearVel then
    Vars.LinearJogSpeed:= Vars.MaxLinearVel;

  State.MaxJogVel:= Round(Vars.MaxLinearJogVel); // Gx, issue #17
  State.ActJogVel:= Round(Vars.LinearJogSpeed);
  State.MaxVel:= Round(Vars.MaxLinearVel);
  State.ActVel:= State.MaxVel;

  if not GetIniDouble('DISPLAY','MAX_ANGULAR_VELOCITY',d,1) then
    if not GetIniDouble('TRAJ','MAX_ANGULAR_VELOCITY',d,1) then
      GetIniDouble('TRAJ','MAX_VELOCITY',d,1);
  Vars.MaxAngularVel:= d * 60;

  LinearUnitConversion:= 0;

  GetIniStr('TRAJ','LINEAR_UNITS',tmp,'');
  if Tmp = '' then
    linearUnitConversion:= LINEAR_UNITS_MM
  else
    begin
      StripBlank(tmp);
      tmp:= UpperCase(tmp);
      if tmp = 'INCH' then LinearUnitConversion:= LINEAR_UNITS_INCH else
      if tmp = 'MM' then LinearUnitConversion:= LINEAR_UNITS_MM;
    end;
  if linearUnitConversion = 0 then
    begin
      writeln('Linear Units not defined.');
      writeln('Setting Units to "mm"');
      LinearUnitConversion:= LINEAR_UNITS_MM;
    end;
  case linearUnitConversion of
    LINEAR_UNITS_MM: Vars.UnitStr:= 'mm';
    LINEAR_UNITS_INCH: Vars.UnitStr:= 'in';
  end;
  Vars.Metric:= LinearUnitConversion = LINEAR_UNITS_MM;
  Vars.UnitVelStr:= Vars.UnitStr + '/min';
  if Vars.Metric then
    writeln('Setup is metric')
  else
    writeln('Setup are Inches');

  // *todo: angularUnitsConversion
  GetIniInt('TRAJ','AXES',i,0);
  if (i < 1) or (i > 9) then
    begin
      writeln('Number of Axes not defined or zero.');
      Exit;
    end;
  Vars.NumAxes:= i;
  if GetIniStr('TRAJ','COORDINATES',tmp,'') then
    begin
      if not StripBlank(tmp) then tmp:= '';
      if Length(tmp) <> Vars.NumAxes then
        begin
          writeln('missmatch in number of joints and coord names');
          Exit;
        end;
    end;
  Vars.CoordNames:= tmp;
  {$ifdef DEBUG_INI}
    writeln('Ini: Coordinates ' + Vars.CoordNames);
  {$endif}

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

  {$ifdef DEBUG_INI}
  writeln('Ini: Toolfile: ',Vars.ToolFile);
  writeln('Ini: Paramfile: ',Vars.ParamFile);
  {$endif}

  Vars.HomingOrderDefined:= GetIniStr('AXIS_0','HOME_SEQUENCE',tmp,'');

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

  GetIniStr('DISPLAY','INCREMENTS',tmp,'');
  if not SetJogIncrements(tmp) then 
    Exit;

  GetIniStr('DISPLAY','EDITOR',tmp,'');
  Vars.Editor:= Trim(tmp);

  if not IniReadAxes then
    Exit;

  // mocca related ini stuff
  GetIniInt('DISPLAY','GLDIRECT',i,1);
  GlSettings.UseDirect:= (i = 1);
  if GlSettings.UseDirect then
    writeln('direct OpenGl rendering activated')
  else
    writeln('direct OpenGl rendering decativated');

  GetIniInt('DISPLAY','GLRGBA',i,1);
  GlSettings.UseRGBA:=  (i = 1);

  GetIniInt('DISPLAY','GLDOUBLEBUFFERED',i,1);
  GlSettings.UseDoubleBuffered:= (i = 1);
  if GlSettings.UseDoubleBuffered then
    writeln('OpenGl doublebuffer activated')
  else
    writeln('OpenGl doublebuffer decativated');

  GetIniInt('DISPLAY','DRO_FONT_SIZE',i,DroFontSize);
  if (i <> DroFontSize) and (i > 10) and (i < 73) then
    DroFontSize:= i;

  GetIniInt('DISPLAY','FONT_SIZE',i,0);
  if (i <> MainFontSize) and (i > 7) and (i < 16) then
    MainFontSize:= i;

  GetIniInt('DISPLAY','FONT_BOLD',i,0);
  MainFontBold:= (i = 1);

  GetIniStr('DISPLAY','BUTTON_SIZE',tmp,'');
  tmp:= UpCase(Trim(tmp));
  if Length(tmp) > 0 then
   begin
     if (tmp = 'MEDIUM') then
       GlobalButtonSize:= ButtonSizeMed
     else
     if (tmp = 'SMALL') then
       GlobalButtonSize:= ButtonSizeSmall;
   end;

  GetIniInt('DISPLAY','WINDOW_SIZE',Vars.WindowSize,0);
  GetIniStr('DISPLAY','SCRIPT_DIR',Vars.ScriptDir,'');

  HasScripts:= IniReadScripts;
  if (Vars.ScriptDir = '') and HasScripts then
    writeln('warning: scripts defined but scriptdir not set!');

  GetIniStr('MOCCA','EDITOR_BEGIN_FILE',EditorBeginFile,'');
  GetIniStr('MOCCA','EDIOTR_END_FILE',EditorEndFile,'');

  GetIniInt('DISPLAY','GL',i,1);
  ShowGlPreview:= Boolean(i = 1);

  Result:= True;
end;


end.




