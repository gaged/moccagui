program mocca;

{$mode delphi}{$H+}

{$I mocca.inc}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  Dialogs,
  emc2pas,
  SysUtils,
  mocmain,
  mocemc,
  mocjoints, mocglb, mocini, jogclient,
  runclient, mdiclient, simclient, gllist, glview, glcanon, editordlg, 
  offsetdlg;
  
function InitEmc: Boolean;
var
  s: string;
begin
  Result:= False;
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
  emcVars.IniFile:= s;
  if (emcNmlInit <> 0) then
    begin
      writeln('Error: Cannot connect to emc');
      Exit;
    end;
  Result:= True;
end;

function QuitEmc: integer;
begin
  result:= 0;
  emcNmlQuit; // free NML buffers
  iniClose;   // close inifile if open
end;

begin
  decimalseparator:='.';
  if not InitEmc then
    Halt(1);
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
  QuitEMC;
end.

