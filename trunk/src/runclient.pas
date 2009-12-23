unit runclient;

{$mode objfpc}{$H+}

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
    OldHasFile: Boolean;
    HasFile: Boolean;

    function  HandleCommand(Cmd: integer): Boolean;
  public
    procedure ActivateSelf;
    procedure UpdateSelf;
    procedure InitControls;
    procedure MapButtons;
    procedure UpdatePreview(FileName: string);

    procedure OpenFile;
    procedure UpdateLine;
    // vorlauf
    procedure SetCodes;
    procedure GotoLine;
  end;
  
var
  clRun: TRunClientForm;

implementation

uses
  strutils,buttons,
  mocglb,mocemc,
  emc2pas,
  simclient,
  glcanon,gllist;


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
  OpenDialog.InitialDir:= Vars.ProgramPrefix;
  Self.Tag:= TASKMODEAUTO;
  OldInterpState:= 0;
  OldActiveLn:= -1;
  OldRunning:= True;
  HasFile:= False;
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

procedure TRunClientForm.UpdatePreview(FileName: string);
var
  UnitCode,InitCode: string;
  Metric: Boolean;
begin
  if Length(FileName) < 1 then Exit;
  Metric:= Pos('G21',ActiveGCodes) > 0;
  if Metric then
    UnitCode:= 'G21' else UnitCode:= 'G20';
  InitCode:= '';
  clSim.ClearPreview;
  clSim.LoadPreview(FileName,UnitCode,InitCode);
end;

function TRunClientForm.HandleCommand(Cmd: integer): Boolean;
begin
  Result:= True;
  case Cmd of
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

    cmSTOP:
      Emc.TaskStop;

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

    UpdateLine;


  if OldHasFile <> HasFile then
    begin
      SetButtonEnabled(cmSTOP,HasFile);
      SetButtonEnabled(cmSTEP,HasFile);
      SetButtonEnabled(cmRUN,HasFile);
      SetButtonEnabled(cmPAUSE,HasFile);
      SetButtonEnabled(cmRUN,HasFile);
      SetButtonEnabled(cmRUNLINE,HasFile);
      OldHasFile:= HasFile;
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

      if OldInterpState <> State.InterpState then
        begin

          Running:= InterpState <> INTERP_IDLE;
          if not Running then LB.ItemIndex:= -1;

         // set buttons according to the interpreter state
          SetButtonDown(cmSTOP,not Running);
          SetButtonDown(cmPAUSE,InterpState = INTERP_PAUSED);
          SetButtonDown(cmRUN,Running);
          // LB.Enabled:= not Running;  // looks nice in gtk2


        // disable the taskmode- buttons if not idle
          if OldRunning <> Running then
            begin
              SetButtonEnabled(cmMDI,not Running);
              SetButtonEnabled(cmJOG,not Running);
              SetButtonEnabled(cmOPEN,not Running);
              SetButtonEnabled(cmEDITOR,not Running);
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
  OldBlockDel:= State.BlockDel;
  OldRunning:= False;
  OldHasFile:= not HasFile;
  SetButtonDown(cmAUTO,True);
  SetButtonDown(cmStop,True);
  SetButtonDown(cmOPTSTOP,State.OptStop);
  SetButtonDown(cmBLOCKDEL,State.BlockDel);

end;

procedure TRunClientForm.OpenFile;
var
  FileName: string;
begin
  if OpenDialog.Execute then
    begin
      sendAuto;
      sendAbort;
      FileName:= OpenDialog.FileName;
      if Length(FileName) > 0 then
        begin
          Vars.ProgramFile:= '';
          LB.Items.LoadFromFile(FileName);
          if sendProgramOpen(PChar(FileName)) = 0 then
            begin
              Emc.WaitDone;
              Vars.StartLine:= -1; // verify
              Vars.ProgramFile:= FileName;
              Emc.TaskRun;
              Vars.StartLine:= 1;
              HasFile:= true;
            end
          else
            begin
              Vars.ProgramFile:= '';
              LastError:= 'Error opening file: ' + FileName;
            end;
          if HasFile then
            UpdatePreview(FileName);
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
      if ActiveLn < 0 then Exit;
      // Label1.Caption:= Inttostr(ActiveLn) + ',' + inttostr(taskcurrentline);
      if ActiveLn <> OldActiveLn then
        begin
          if ActiveLn < LB.Items.Count then
            begin
              LB.ItemIndex:= ActiveLn;
              if (ActiveLn > 0) then
              //if not LB.ItemFullyVisible(ActiveLn) then
                LB.MakeCurrentVisible;
            end;
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

