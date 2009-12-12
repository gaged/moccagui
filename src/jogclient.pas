unit jogclient;

{$mode objfpc}{$H+}
{$I mocca.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TJogClientForm }

  TJogClientForm = class(TForm)
    Label1: TLabel;
    LabelCaption: TLabel;
    LabelJogVel: TLabel;
    Label3: TLabel;
    rgJogInc: TRadioGroup;
    sbJogVel: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure rgJogIncClick(Sender: TObject);
    procedure sbJogVelChange(Sender: TObject);
    procedure Click(Sender: TObject);
  private
    OldORideLimits: Boolean;
    OldSpDir: integer;
    procedure EditOffsets;
  public
    procedure ActivateSelf;
    procedure UpdateSelf;
    procedure InitControls;
    procedure MapButtons;
    procedure HandleCommand(Cmd: integer);
    function  HandleJogKeys(var Key: Word; Down: Boolean): Boolean;
  end;

var
  clJog: TJogClientForm;

implementation

{ TJogClientForm }

uses
  buttons,mocemc,
  mocglb,mocjoints,
  emc2pas,offsetdlg;

procedure TJogClientForm.EditOffsets;
var
  Dlg: TOffsetsDlg;
begin
  Application.CreateForm(TOffsetsDlg,Dlg);
  if Assigned(Dlg) then
    begin
      Dlg.ShowModal;
      Dlg.Free;
    end;
end;

procedure TJogClientForm.HandleCommand(Cmd: integer);
begin
  case Cmd of
    cmLIMITS: Emc.OverrideLimits;
    cmOFFSDLG: EditOffsets;
  else
    Emc.HandleCommand(Cmd);
  end;
end;

procedure TJogClientForm.ActivateSelf;
begin
  if emcState.TaskMode <> TASKMODEMANUAL then
    raise Exception.Create('Cannot activate jogwindow when not in mode manual.');
  if not Visible then
    Visible:= true;
  MapButtons;
  InitControls;
end;

procedure TJogClientForm.UpdateSelf;
var
  ORideLimits: Boolean;
begin
  if ORideLimits <> emcState.ORideLimits then
    begin
      SetButtonDown(cmLIMITS,ORideLimits);
      OldORideLimits:= emcState.ORideLimits;
    end;
  if OldSpDir <> emcState.SpDir then
    begin
      SetButtonDown(cmSPCW,emcState.SpDir > 0);
      SetButtonDown(cmSPCCW,emcState.SpDir < 0);
      OldSpDir:= emcState.SpDir;
    end;
end;

procedure TJogClientForm.MapButtons;
begin
  SetButtonMap(@BtnDefJog,@Self.Click);
end;

procedure TJogClientForm.InitControls;
var
  i: integer;
begin
  SetButtonDown(cmJOG,True);
  // SetButtonDown(cmLIMITS,emcState.ORideLimits);
  SetButtonEnabled(cmREFALL,emcVars.HomingOrderDefined);
  with emcState,emcVars do
    begin
      sbJogVel.Min:= MinJogVel;
      sbJogVel.Max:= MaxJogVel;
      sbJogVel.Position:= ActJogVel;
      LabelJogVel.Caption:= IntToStr(ActJogVel) + UnitVelStr;
      rgJogInc.Items.Clear;
      for i:= 0 to JogIncMax do
        rgJogInc.Items.Add(JogIncrements[i].Text);
      rgJogInc.ItemIndex:= 0;
      JogContinous:= True;
      OldORideLimits:= False;
      OldSpDir:= emcState.SpDir + 1;
    end;
end;

function TJogClientForm.HandleJogKeys(var Key: Word; Down: Boolean): Boolean;

procedure Jog(Ch: Char; Dir: Integer);
var
  Speed: Double;
begin
  Key:= 0;
  Speed:= emcState.ActJogVel * Dir;
  if emcVars.JogContinous then
    Joints.JogCont(Ch,Speed)
  else
    Joints.JogIncr(Ch,Speed,emcVars.JogIncrement);
end;

procedure JogStop(Ch: Char);
begin
  if emcVars.JogContinous then
    begin
      Joints.JogStop(Ch);
      Sleep(10);
    end;
  Key:= 0;
end;

begin
  if Down then
    begin
      case Key of
        33: Jog('Z', 1);
        34: Jog('Z',-1);
        37: Jog('X',-1);
        39: Jog('X', 1);
        38: Jog('Y', 1);
        40: Jog('Y',-1);
      end;
    end
  else
    begin
      case Key of
        33,34: JogStop('Z');
        37,39: JogStop('X');
        38,40: JogStop('Y');
      end // case
    end;
  Result:= (Key = 0);
end;

procedure TJogClientForm.Click(Sender: TObject);
begin
  if Assigned(Sender) then
    with Sender as TSpeedButton do
      begin
        Down:= False;
        HandleCommand(Tag)
      end;
end;

procedure TJogClientForm.sbJogVelChange(Sender: TObject);
var
  Vel: integer;
begin
  if UpdateLock then Exit;
  Vel:= sbJogVel.Position;
  if Vel <> emcState.ActJogVel then
    begin
      emcState.ActJogVel:= Vel;
      LabelJogVel.Caption:= IntToStr(emcState.ActJogVel)+emcVars.UnitVelStr;
    end;
end;

procedure TJogClientForm.FormCreate(Sender: TObject);
begin
  Self.Tag:= TASKMODEMANUAL;
end;

procedure TJogClientForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if HandleJogKeys(Key,True) then Key:= 0;
end;

procedure TJogClientForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if HandleJogKeys(Key,False) then Key:= 0;
end;

procedure TJogClientForm.rgJogIncClick(Sender: TObject);
var
  i: integer;
begin
  i:= rgJogInc.ItemIndex;
  if (i < 0) or (i > emcVars.JogIncMax) then Exit;
  emcVars.JogContinous:= (i = 0);
  emcVars.jogIncrement:= emcVars.JogIncrements[i].Value;
end;


initialization
  {$I jogclient.lrs}
  
end.

