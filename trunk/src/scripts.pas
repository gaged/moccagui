unit scripts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  mocglb,mocemc;

procedure RunScript(Cmd: integer);

implementation

uses
  Forms,emc2pas;

function GetScriptFile(C: integer): string;
var
  i,b: integer;
begin
  Result:= '';
  if not HasScripts then Exit;
  for i:= 0 to NumButtons - 1 do
    if BtnDefScripts[i].T = C then
      begin
        b:= C - cmSCRIPTBASE;
        if (b >= 0) and (b < NumButtons) then
          begin
            Result:= MocScripts[b].Script;
            Exit;
          end;
      end;
end;

procedure ExecuteLine(s: string);
begin
  if sendMDICmd(PChar(s)) <> 0 then
    begin
      writeln('error executing:' ,s);
      Exit;
    end;
  if emcCommandWaitReceived(emcCommandSerialNumber) <> 0 then
    Exit;
  while emcPollStatus = RCS_EXEC do
    begin
      Application.ProcessMessages;
      if (ErrorStr[0] <> #0) or (State.EStop) or
        (not State.Machine) then Break;
    end;
end;

procedure ExecuteScript;
var
  s: string;
  i: integer;
begin
  ScriptRunning:= True;
  try
    sendMDI;
    Emc.WaitDone;
    for i:= 0 to StrListScript.Count - 1 do
      begin
        s:= StrListScript[i];
        if (ErrorStr[0] <> #0) or (State.EStop) or
        (not State.Machine) then Break;
        if not ScriptRunning then Break;
        if s <> '' then
          ExecuteLine(s);
      end;
  finally
    SendManual;
    ScriptRunning:= False;
  end;
end;

procedure RunScript(Cmd: integer);
var
  s: string;
  i,OldMode: integer;
begin
  if State.TaskMode <> TASKMODEMANUAL then
    begin
      LastError:= 'Cannot run a script when not in mode manual';
      Exit;
    end;
  s:= GetScriptFile(Cmd);
  if s = '' then
    begin
      writeln('No Script available for ',Cmd);
      Exit;
    end;
  if not Assigned(StrListScript) then
    StrListScript:= TStringList.Create;
  if StrListScript = nil then Exit;
  try
    StrListScript.LoadFromFile(s);
  except
    LastError:='Error loading script: ' + s;
    Exit;
  end;
  ExecuteScript;
end;

finalization

if Assigned(StrListScript) then
  FreeAndNil(StrListScript);

end.

