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
    function  HandleCommand(Cmd: integer): Boolean;  // internal handlecommand
  public
    procedure ActivateSelf;
    procedure UpdateSelf;
    procedure InitControls;
    procedure MapButtons;
    procedure OpenFile;
    procedure UpdateLine;
  end; 
  
var
  clRun: TRunClientForm;

implementation

uses
  buttons,
  simclient,
  mocglb,mocjoints,emc2pas;

procedure TRunClientForm.FormCreate(Sender: TObject);
begin
  Self.Tag:= TASKMODEAUTO;
  OldInterpState:= 0;
end;

function TRunClientForm.HandleCommand(Cmd: integer): Boolean;
begin
  Result:= True;
  case Cmd of
    cmNCOPEN:  OpenFile;
    cmNCOPTM1: sendSetOptionalStop(not emcState.OptStop);
    cmNCPAUSE: if emcState.InterpState = INTERP_PAUSED then
                 Emc.TaskResume
               else
                 Emc.TaskPause;
    cmNCRUN:   Emc.TaskRun;
    cmNCSTOP:  Emc.TaskStop;
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
  {$ifdef DEBUG_EMC}
  writeln('runclient activateself');
  {$endif}
end;

procedure TRunClientForm.UpdateSelf;
begin
  UpdateLine;
  if OldInterpState <> emcState.InterpState then
    with emcState do
      begin
        SetSoftBtn(cmNCSTOP,InterpState = INTERP_IDLE);
        SetSoftBtn(cmNCPAUSE,InterpState = INTERP_PAUSED);
        SetSoftBtn(cmNCRUN,InterpState <> INTERP_IDLE);
        if InterpState = INTERP_IDLE then
          begin
            // SetSoftBtn(cmNCRUN,False);
            LabelInterpState.Caption:= 'Idle';
          end
        else
        if InterpState = INTERP_PAUSED then
          begin
            // SetSoftBtn(cmNCPAUSED,True);
            LabelInterpState.Caption:= 'Paused';
          end
        else
          if InterpState = INTERP_READING then
            LabelInterpState.Caption:= 'Reading'
          else
            LabelInterpState.Caption:= 'Waiting';
        OldInterpState:= InterpState;
      end;
end;

procedure TRunClientForm.MapButtons;
var
  i,Id,iTag: Integer;
  Tags: Array[0..NumSoftButtons - 1] of integer;
  Glyphs: Array[0..NumSoftButtons - 1] of integer;
begin
  Tags:= cmdRunTags;
  Glyphs:= cmdRunGlyphs;
  for i:= 0 to NumSoftButtons - 1 do
    begin
      Id:= Glyphs[i];
      iTag:= Tags[i];
      if (Id < 0) or (iTag < 0) then
        begin
          SoftBtns[i].Glyph:= nil;
          SoftBtns[i].Enabled:= False;
          SoftBtns[i].Tag:= -1;
          SoftBtns[i].OnClick:= nil;
        end
      else
        begin
          GlobalImageList.GetBitmap(Id,SoftBtns[i].Glyph);
          SoftBtns[i].Tag:= iTag;
          SoftBtns[i].Enabled:= True;
          SoftBtns[i].OnClick:= @Self.Click;
        end;
    end;
end;

procedure TRunClientForm.InitControls;
begin
end;

procedure TRunClientForm.OpenFile;
var
  s: string;
  FileName: string;
begin
  if OpenDialog.Execute then
    begin
      sendAuto;
      sendAbort;
      FileName:= OpenDialog.FileName;
      clSim.Reset;
      clSim.ParseFile(PChar(FileName));
      clSim.Update;
      LB.Items.LoadFromFile(FileName);
      if Length(FileName) > 0 then
        begin
          sendProgramOpen(PChar(FileName));
          Caption:= 'Mocca: ' + FileName;
        end;
    end;
end;

procedure TRunClientForm.UpdateLine;
var
  i: integer;
begin
  with emcState do
  if InterpState <> INTERP_IDLE then
    begin
      i:= taskMotionLine;
      if i < LB.Items.Count then
        LB.ItemIndex:= i;
      if not LB.ItemFullyVisible(i) then
        LB.MakeCurrentVisible;
    end;
end;

procedure TRunClientForm.Click(Sender: TObject);
begin
  if Assigned(Sender) then
    if Sender is TSpeedButton then
      with Sender as TSpeedButton do
        begin
          if Tag > 100 then Down:= False;
          if not Self.HandleCommand(Tag) then
            Emc.HandleCommand(Tag);
        end;
end;

initialization
  {$I runclient.lrs}

end.

