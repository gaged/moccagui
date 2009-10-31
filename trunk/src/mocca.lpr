program mocca;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces,
  Forms,
  Dialogs,
  mocca_main,
  emc2pas,
  SysUtils,
  mocglobal, StatusDlg, HomeDlg;
  

function InitEmc: integer;
var
  s: string;
  i: integer;
begin
  Result:= -1;
  if ParamCount < 2 then
    begin
      writeln('Error connecting to emc');
      writeln('Mocca needs at least 2 params.');
      writeln(IntToStr(ParamCount) + ' params passed to mocca');
      if (ParamCount > 0) then
        for i:= 0 to PAramCount - 1 do
         writeln('Param: "' + ParamStr(i) + '"');
      writeln('press a key');
      readln;
      Exit;
    end;
  s:= ParamStr(2);
  if iniOpen(PChar(S)) <> 0 then
    begin
      writeln('Cannot open inifile: ');
      writeln('"' + s + '"');
      writeln('press a key');
      readln;
      Exit;
    end;
  if LoadSetupFromIni then
    Result:= emcNmlInit;
end;

function QuitEmc: integer;
begin
  result:= 0;
  emcNmlQuit; // free NML buffers
  iniClose;   // close inifile if open
end;


begin
  if (InitEmc <> 0) then
    Halt(1);
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TStatusDlgForm, StatusDlgForm);
  Application.CreateForm(THomeDlgForm, HomeDlgForm);
  Application.Run;
  if QuitEmc <> 0 then
    Halt(2);
end.

