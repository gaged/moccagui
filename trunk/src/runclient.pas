unit runclient;

{$mode objfpc}{$H+}
{$I mocca.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls;

type

  { TRunClientForm }
  TRunClientForm = class(TForm)
    LabelCaption: TLabel;
    LabelInterpState: TLabel;
    LB: TListBox;
    OpenDialog: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure Click(Sender: TObject);
  private
    OldInterpState: integer;
    OldActiveLn: integer;
    OldRunning: Boolean;
    OldBlockDel: Boolean;
    OldOptStop: Boolean;
    OldIsOpen: Boolean;
    IsOpen: Boolean;
    function  HandleCommand(Cmd: integer): Boolean;
  public
    procedure ActivateSelf;
    procedure EditorMode(ShowEditor: Boolean);
    procedure UpdateSelf;
    procedure InitControls;
    procedure MapButtons;
    procedure UpdatePreview(Reload: Boolean);
    procedure OpenFile;
    procedure UpdateLine;
    procedure SetCodes;
    procedure GotoLine;
  end;
  
var
  clRun: TRunClientForm;

implementation

uses
  strutils,mocbtn,
  mocglb,mocemc,
  emc2pas,
  {$IFDEF USEGL}
  simclient,
  {$ENDIF}
  glcanon,gllist,
  editorclient;

procedure TRunClientForm.EditorMode(ShowEditor: Boolean);
begin
  if ShowEditor then
    begin
      if clEditor.Visible then Exit;
      UpdateLock:= True;
      Self.Visible:= False;
      if IsOpen and (Vars.ProgramFile <> '') then
        clEditor.LoadInternalFile(LB.Items,Vars.ProgramFile);
      clEditor.ActivateSelf;
    end
  else
    begin
      UpdateLock:= False;
      clEditor.Visible:= False;
      ActivateSelf;
    end;
end;

procedure TRunClientForm.SetCodes;
var
  C,S: string;
  i: integer;

  procedure SetGCode;
  var n: integer;
  begin
    S:= '';
    for n:= 0 to CoordSysMax do
      if C = CoordSys[n] then
        S:= S + C;
    if C[2] = '2' then
      S:= S + C;
    if C[2] = '9' then
      S:= S + C;
    Emc.Execute(S);
    Emc.WaitDone;
    S:= '';
  end;

begin
  for i:= 1 to ACTIVE_G_CODES_MAX - 1 do
    begin
      C:= GCodeToStr(i);
      if C <> '' then SetGCode;
    end;
end;

procedure TRunClientForm.FormCreate(Sender: TObject);
begin
  if MainFontSize > 0 then
    Self.Font.Size:= MainFontSize;
  OpenDialog.InitialDir:= Vars.ProgramPrefix;
  {$ifdef DEBUG_INI}
  writeln(OpenDialog.InitialDir);
  {$endif}
  Self.Tag:= TASKMODEAUTO;
  OldInterpState:= 0;
  OldActiveLn:= -1;
  OldRunning:= True;
  IsOpen:= False;
  LB.Enabled:= True;
  LB.MultiSelect:= False;
  LB.ClearSelection;
end;

procedure TRunClientForm.GotoLine;
var
  CurrentLine: integer;
  StartLine,EndLine: integer;
  InterpResult: integer;
  Cmd: string;
  SaveGlList: TGlList;
begin
  if (LB.Items.Count > 0) and (LB.ItemIndex > 0) then
    begin
      StartLine:= 0;
      CurrentLine:= StartLine;
      EndLine:= LB.ItemIndex;
      InterpResult:= interpreter_exec(PChar('M2'));
      InterpResult:= 0;
      SaveGlList:= MyGlList;
      try
        MyGlList:= nil;
        while (CurrentLine < EndLine) and (InterpResult = 0) do
          begin
            Cmd:= LB.Items[CurrentLine];
            if Cmd <> '' then
              InterpResult:= interpreter_exec(PChar(Cmd));
            Inc(CurrentLine);
          end;
        if InterpResult <> 0 then
          LastError:= 'Fehler im Satzlauf, GCode unzulässig'
        else
          begin
            interpreter_codes;  // update the gcode,mcodes & settings
            Vars.StartLine:= CurrentLine;
          end;
      finally
        MyGlList:= SaveGlList;
      end;
    SetCodes;
  end;
end;

procedure TRunClientForm.UpdatePreview(Reload: Boolean);
var
  UnitCode,InitCode: string;
  Metric: Boolean;
begin
  if Reload then
    begin
      {$IFDEF USEGL}
      if Assigned(clSim) then
        clSim.ReloadFile;
      {$ENDIF}
      Exit;
    end;
  if (Length(Vars.ProgramFile) < 1) or (not IsOpen) then Exit;
  //Metric:= Pos('G21',ActiveGCodes) > 0;
  Metric:= Vars.Metric;
  if Metric then
    UnitCode:= 'G21' else UnitCode:= 'G20';
  InitCode:= '';
  {$IFDEF USEGL}
  if Assigned(clSim) then
    begin
      clSim.ClearFile;
      clSim.LoadFile(Vars.ProgramFile,UnitCode,InitCode);
    end;
  {$ENDIF}
end;

function TRunClientForm.HandleCommand(Cmd: integer): Boolean;
begin
  Result:= True;
  case Cmd of
    cmEDITOR: if State.InterpState = INTERP_IDLE then
      EditorMode(True);
    cmOPEN:
      OpenFile;

    cmPAUSE:
      if State.InterpState = INTERP_PAUSED then
        Emc.TaskResume
      else
        Emc.TaskPause;

    cmRUN: Emc.TaskRun;

    cmRUNLINE :
      if LB.ItemIndex < 0 then
        LastError:= 'Need a selected line'
      else
        begin
          Vars.StartLine:= LB.ItemIndex + 2;
          Emc.TaskRun;
        end;

    cmRELOAD: begin
      LB.ClearSelection;
      LB.ItemIndex:= -1;
    end;
    cmSTOP:
      Emc.TaskStop;

    cmSTEP:
      begin
        Emc.TaskStep;
        UpdateLine;
        write('ML:',taskMotionline);
        write('CL:',taskCurrentLine);
        writeln('RL:',taskReadLine);
      end;
    cmOPTSTOP:
      sendSetOptionalStop(not State.OptStop);

    cmBLOCKDEL:
      sendSetBlockDelete(not State.BlockDel);

  else
    Result:= False;
  end; // case;
end;

procedure TRunClientForm.ActivateSelf;
begin
  if State.TaskMode <> TASKMODEAUTO then Exit;
  if not Visible then
    Visible:= true;
  MapButtons;
  InitControls;
end;

procedure TRunClientForm.UpdateSelf;
var
  Running: Boolean;
begin

  if State.TaskMode <> TASKMODEAUTO then
    begin
      writeln('Invalid call to runclient.updateself');
      Exit;
    end;

  UpdateLine;

  if OldIsOpen <> IsOpen then
    begin
      SetButtonEnabled(cmSTOP,IsOpen);
      SetButtonEnabled(cmSTEP,IsOpen);
      SetButtonEnabled(cmRUN,IsOpen);
      SetButtonEnabled(cmPAUSE,IsOpen);
      SetButtonEnabled(cmRUN,IsOpen);
      SetButtonEnabled(cmRUNLINE,IsOpen);
      OldIsOpen:= IsOpen;
    end;

  with State do
    begin
      if OldBlockDel <> BlockDel then
        begin
          SetButtonDown(cmBLOCKDEL,BlockDel);
          OldBlockDel:= BlockDel;
        end;

      if OldOptStop <> OptStop then
        begin
          SetButtonDown(cmOPTSTOP,OptStop);
          OldOptStop:= OptStop;
        end;

      if OldInterpState <> InterpState then
        begin

          Running:= InterpState <> INTERP_IDLE;

          SetButtonDown(cmSTOP,not Running);
          SetButtonDown(cmPAUSE,InterpState = INTERP_PAUSED);
          SetButtonDown(cmRUN,Running);

          // disable the taskmode- buttons if not idle
          if OldRunning <> Running then
            begin
              SetButtonEnabled(cmMDI,not Running);
              SetButtonEnabled(cmJOG,not Running);
              SetButtonEnabled(cmOPEN,not Running);
              SetButtonEnabled(cmEDITOR,not Running);
              {$IFDEF LCLGTK2}  //looks better in Gtk2 :)
              // LB.Enabled:= not Running;
              if not Running then
                LB.ClearSelection;
              {$ENDIF}
              OldRunning:= Running;
            end;

          if InterpState = INTERP_IDLE then
            LabelInterpState.Caption:= 'Idle'
          else
          if InterpState = INTERP_PAUSED then
            LabelInterpState.Caption:= 'Paused'
          else
          if InterpState = INTERP_READING then
            LabelInterpState.Caption:= 'Reading'
          else
            LabelInterpState.Caption:= 'Waiting';
        OldInterpState:= InterpState;
      end;
    end;
end;

procedure TRunClientForm.MapButtons;
begin
  SetButtonMap(@BtnDefRun,@BtnDefRun1,@Self.Click);
end;

procedure TRunClientForm.InitControls;
begin
  OldBlockDel:= State.BlockDel;
  OldRunning:= False;
  OldIsOpen:= not IsOpen;
  SetButtonDown(cmAUTO,True);
  SetButtonDown(cmStop,True);
  SetButtonDown(cmOPTSTOP,State.OptStop);
  SetButtonDown(cmBLOCKDEL,State.BlockDel);
end;

procedure TRunClientForm.OpenFile;
var
  FileName: string;
begin
  writeln('OpenFile');
  OpenDialog.InitialDir:= Vars.ProgramPrefix;
  {$ifdef DEBUG_INI}
  writeln('Initial directory: ',OpenDialog.InitialDir);
  {$endif}

  if OpenDialog.Execute then
    begin
      sendAuto;
      sendAbort;
      Vars.ProgramFile:= '';
      IsOpen:= False;
      {$IFDEF USEGL}
      if Assigned(clSim) then
        clSim.ClearFile;
      {$ENDIF}
      FileName:= OpenDialog.FileName;
      if Length(FileName) > 0 then
        begin
          LB.Items.LoadFromFile(FileName);
          if sendProgramOpen(PChar(FileName)) = 0 then
            begin
              LB.ClearSelection;
              Emc.WaitDone;
              Vars.ProgramFile:= FileName;
              if State.Machine then
                begin
                  Vars.StartLine:= -1; // verify
                  Emc.TaskRun;
                end;
              Vars.StartLine:= 1;
              IsOpen:= True;
            end
          else
            begin
              Vars.ProgramFile:= '';
              LastError:= 'Error opening file: ' + FileName;
              IsOpen:= False;
            end;
          if IsOpen then
            UpdatePreview(False);
        end;
    end;
end;

procedure TRunClientForm.UpdateLine;
var
  ActiveLn: integer;
begin
  with State do
  if InterpState <> INTERP_IDLE then
    begin
      ActiveLn:= taskMotionLine - 1;
      if ActiveLn < 0 then
        ActiveLn:= taskCurrentLine + 1;
      if ActiveLn < 0 then
        Exit;
      if ActiveLn <> OldActiveLn then
        begin
          if ActiveLn < LB.Items.Count then
            begin
              LB.ItemIndex:= ActiveLn;
              if (ActiveLn > 0) then
                LB.MakeCurrentVisible;
            end;
          OldActiveLn:= ActiveLn;
        end;
    end;
end;

procedure TRunClientForm.Click(Sender: TObject);
begin
  if Assigned(Sender) then
    with Sender as TMocButton do
      begin
        if not Self.HandleCommand(Tag) then
          Emc.HandleCommand(Tag);
      end;
end;

initialization
  {$I runclient.lrs}

end.

