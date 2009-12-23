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
  mocglb, mocini, jogclient,
  runclient, mdiclient, simclient, editordlg,
  offsetdlg, tooleditdlg;
  
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
  Vars.IniFile:= s;
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
  write('Setting LANG: en_EN ...');
  if setenv(PChar('LANG'),PChar('en_us.utf-8'),1) = 0 then
    writeln('Ok') else writeln('Failed');
  Writeln ('LANG=',getenvironmentvariable('LANG'));
  decimalseparator:='.';
  if not InitEmc then
    Halt(1);
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
  QuitEMC;
end.

