unit runclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

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

    OldDryRunSet: Boolean;
    DryRunSet: Boolean;

    function  HandleCommand(Cmd: integer): Boolean;
  public
    procedure ActivateSelf;
    procedure UpdateSelf;
    procedure InitControls;
    procedure MapButtons;

    procedure OpenFile;
    procedure UpdateLine;
    // vorlauf
    procedure SetCodes;
    procedure DryRunToLine;
    procedure GotoLine;
  end;
  
var
  clRun: TRunClientForm;

implementation

uses
  buttons,
  mocglb,mocjoints,
  emc2pas,
  simclient,
  glcanon,gllist;

procedure TRunClientForm.SetCodes();
begin


end;

procedure TRunClientForm.FormCreate(Sender: TObject);
begin
  Self.Tag:= TASKMODEAUTO;
  OldInterpState:= 0;
  OldActiveLn:= -1;
  OldRunning:= True;
  OldDryRunSet:= True;
  DryRunSet:= False;
end;

procedure TRunClientForm.GotoLine;
begin
  emcVars.StartFromLine:= LB.ItemIndex;
end;

procedure TRunClientForm.DryRunToLine;
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
            emcVars.StartFromLine:= CurrentLine;
          end;
      finally
        MyGlList:= SaveGlList;
      end;
      DryRunSet:= True;
    end;
end;

function TRunClientForm.HandleCommand(Cmd: integer): Boolean;
begin
  Result:= True;
  case Cmd of
    cmOPEN:
      OpenFile;

    cmPAUSE:
      if emcState.InterpState = INTERP_PAUSED then
        Emc.TaskResume
      else
        Emc.TaskPause;

    cmRUN:
      begin
        // emcVars.StartFromLine:= 0;
        Emc.TaskRun;
      end;

    cmDRYRUN:
      if emcState.InterpState = INTERP_IDLE then
        if DryRunSet then    // clear the dry run
          begin
            emcVars.StartFromLine:= 0;
            DryRunSet:= False;
          end
        else
          if LB.ItemIndex > 0 then
            DryRunToLine;

    cmSTOP:
      Emc.TaskStop;

    cmOPTSTOP:
      sendSetOptionalStop(not emcState.OptStop);

    cmBLOCKDEL:
      sendSetBlockDelete(not emcState.BlockDel);

  else
    Result:= False;
  end; // case;
end;

procedure TRunClientForm.ActivateSelf;
begin
  if emcState.TaskMode <> TASKMODEAUTO then Exit;
  if not Visible then
    Visible:= true;
  MapButtons;
  InitControls;
end;

procedure TRunClientForm.UpdateSelf;
var
  Running: Boolean;
begin
  UpdateLine;
  with emcState do
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

      if OldDryRunSet <> DryRunSet then
        begin
          SetButtonDown(cmDRYRUN,DryRunSet);
          OldDryRunSet:= DryRunSet;
        end;

      if OldInterpState <> emcState.InterpState then
        begin
          Running:= InterpState <> INTERP_IDLE;
         // set buttons according to the interpreter state
          SetButtonDown(cmSTOP,not Running);
          SetButtonDown(cmPAUSE,InterpState = INTERP_PAUSED);
          SetButtonDown(cmRUN,Running);

        // disable the taskmode- buttons if not idle
          if OldRunning <> Running then
            begin
              SetButtonEnabled(cmMDI,not Running);
              SetButtonEnabled(cmJOG,not Running);
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
  SetButtonMap(@BtnDefRun,@Self.Click);
end;

procedure TRunClientForm.InitControls;
begin
  OldBlockDel:= emcState.BlockDel;
  OldRunning:= False;
  SetButtonDown(cmStop,True);
  SetButtonDown(cmOPTSTOP,emcState.OptStop);
  SetButtonDown(cmBLOCKDEL,emcState.BlockDel);
  SetButtonDown(cmAUTO,True);
end;

procedure TRunClientForm.OpenFile;
var
  s: string;
  FileName: string;
  Error: integer;
begin
  if OpenDialog.Execute then
    begin
      sendAuto;
      sendAbort;
      FileName:= OpenDialog.FileName;
      if Length(FileName) > 0 then
        begin
          LB.Items.LoadFromFile(FileName);
          clSim.LoadPreview(FileName);
          if sendProgramOpen(PChar(FileName)) = 0 then
            emcVars.ProgramFile:= FileName
          else
            begin
              emcVars.ProgramFile:= '';
              LastError:= 'Error opening file: ' + FileName;
            end;
        end;
    end;
end;

procedure TRunClientForm.UpdateLine;
var
  CurrentLn,MotionLn,ActiveLn: integer;
begin
  with emcState do
  if InterpState <> INTERP_IDLE then
    begin
      CurrentLn:= taskCurrentLine;
      if (CurrentLn > 0) then
         begin
           MotionLn:= taskMotionLine;
           if (MotionLn > 0) and (MotionLn < CurrentLn) then
             ActiveLn:= MotionLn - 1
           else
             ActiveLn:= CurrentLn - 1;
         end
      else
        ActiveLn:= -1;
      if ActiveLn <> OldActiveLn then
        begin
          if ActiveLn < LB.Items.Count then
            LB.ItemIndex:= ActiveLn;
          if (ActiveLn >= 0) then
            if not LB.ItemFullyVisible(ActiveLn) then
              LB.MakeCurrentVisible;
          OldActiveLn:= ActiveLn;
        end;
    end;
end;

procedure TRunClientForm.Click(Sender: TObject);
begin
  if Assigned(Sender) then
    with Sender as TSpeedButton do
      begin
        Down:= False;
        if not Self.HandleCommand(Tag) then
          Emc.HandleCommand(Tag);
      end;
end;

initialization
  {$I runclient.lrs}

end.

