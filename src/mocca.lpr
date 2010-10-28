program mocca;

{$I mocca.inc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  Dialogs,
  {$IFNDEF OWNGL}
  OpenGlContext,
  {$ENDIF}
  moccapkg, emc2pas, SysUtils, mocmain, mocglb, mocini, jogclient, runclient,
  mdiclient, simclient, offsetdlg, tooleditdlg, touchoff, toolchange, hal,
  emcint, scripts, emcmsgbox, configreader, touchoffwiz, coordrotate, simulator,
  mocstat, gltools, startctrl, tssupport;

const
  __LC_NUMERIC  = 1;

const
  clib = 'c';

const
  ERR_INIT_PARAMS : string = 'Error starting mocca, check params.';
  ERR_EMCCONNECT = 'Error: Can not connect to EMC';

function setlocale(category: integer; locale: pchar): pchar; cdecl; external clib name 'setlocale';

function InitEmc: Boolean;
var
  s: string;
begin
  Result:= False;
  SetLocale(__LC_NUMERIC,PChar('C'));
  decimalseparator:='.';
  if not InitEmcEnvironment then Halt(1);
  if (ParamCount < 2) then
    begin
      writeln(ERR_INIT_PARAMS);
      Exit;
    end;
  s:= ParamStr(2); // the paramstr(2) is the ini-file
  if not IniRead(S) then  // iniread: see mocini.pas
    Exit;
  Vars.IniFile:= s;
  if not ReadConfig(ConfigDir + 'config.xml') then
    Exit;
  if (emcNmlInit <> 0) then
    begin
      writeln(ERR_EMCCONNECT);
      Exit;
    end;
  if not InitHal then
    Exit;
  Result:= True;
end;

function QuitEmc: integer;
begin
  Result:= 0;
  DoneHal;
  emcNmlQuit; // free NML buffers
  // iniClose;   // close inifile if open
  DoneEmcEnvironment;
end;

begin
  if not InitEmc then Halt(1);
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TKeyboardForm, KeyboardForm);
  Application.Run;
  QuitEMC;
end.

