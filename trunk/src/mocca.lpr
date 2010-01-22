program mocca;

{$I mocca.inc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  Dialogs, LResources,
  {$IFNDEF OWNGL}
  OpenGlContext,
  {$ENDIF}
  emc2pas,
  mocbtn, SysUtils,
  mocmain,
  mocglb, mocini, jogclient,
  runclient, mdiclient,
  {$IFDEF USEGL}
  simclient,
  {$ENDIF}
  editordlg, offsetdlg, tooleditdlg, touchoff, toolchange, hal, emcint,
partaligndlg, scripts;

const
  __LC_CTYPE    = 0;
  __LC_NUMERIC  = 1;
  __LC_TIME     = 2;
  __LC_COLLATE  = 3;
  __LC_MONETARY = 4;
  __LC_MESSAGES = 5;
  __LC_ALL      = 6;

Const
  clib = 'c';

function setlocale(category: integer; locale: pchar): pchar; cdecl; external clib name 'setlocale';
// function nl_langinfo(__item: cint):Pchar; cdecl; external clib name 'nl_langinfo';

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
      writeln('Error starting mocca, check params.');
      Exit;
    end;
  s:= ParamStr(2); // the paramstr(2) is the ini-file
  if not IniRead(S) then  // iniread: see mocini.pas
    begin
      writeln('Cannot open inifile: ' + '"' + s + '"');
      Exit;
    end;
  Vars.IniFile:= s;
  if (emcNmlInit <> 0) then
    begin
      writeln('Error: Cannot connect to emc');
      Exit;
    end;
  if not InitHal('mocca') then
    Exit;
  Result:= True;
end;

function QuitEmc: integer;
begin
  result:= 0;
  DoneHal;
  emcNmlQuit; // free NML buffers
  iniClose;   // close inifile if open
  DoneEmcEnvironment;
end;

{$IFDEF WINDOWS}{$R mocca.rc}{$ENDIF}

begin
  // {$I mocca.lrs}
  writeln('starting mocca...');
  if not InitEmc then Halt(1);
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
  QuitEMC;
end.

