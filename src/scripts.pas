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
  if not HasScripts then
    begin
      writeln('Called "GetScriptFile" but no Scripts available');
      Exit;
    end;
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
      if (ErrorStr[0] <> #0) or (State.State <> STATE_ON) then
        break;
    end;
end;

procedure ExecuteScript;
var
  s: string;
  i: integer;
begin
  writeln('executing script');
  ScriptRunning:= True;
  try
    sendMDI;
    Emc.WaitDone;
    for i:= 0 to StrListScript.Count - 1 do
      begin
        s:= StrListScript[i];
        if (ErrorStr[0] <> #0) or (State.State <> STATE_ON) then
          begin
            writeln('Script aborted.');
            Break;
          end;
        if not ScriptRunning then
          begin
            writeln('Script halted.');
            Break;
          end;
        writeln('script: ' + S);
        if s <> '' then
          ExecuteLine(s);
      end;
  finally
    SendManual;
    Emc.WaitDone;
    ScriptRunning:= False;
  end;
end;

procedure RunScript(Cmd: integer);
var
  s,e: string;
begin
  e:= '';
  if State.Mode <> TASKMODEMANUAL then
    e:= 'Cannot run a script when not in mode manual';
  if State.State <> STATE_ON then
    e:= 'Cannot execute a script when machine is turned off or EStop is activated';
  if e <> '' then
    begin
      writeln(e);
      LastError:= e;
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
  if StrListScript = nil then
    begin
      writeln('error creating stringlist for scripts');
      Exit;
    end;
  try
    StrListScript.LoadFromFile(ConfigDir + s);
  except
    LastError:='Error loading script: ' + s;
    writeln(LastError);
    Exit;
  end;
  if StrListScript.Count < 1 then
    writeln('Script ' + s + ' is empty!')
  else
    ExecuteScript;
end;

finalization

if Assigned(StrListScript) then
  FreeAndNil(StrListScript);

end.

