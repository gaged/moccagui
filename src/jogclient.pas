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
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    LabelSpORide: TLabel;
    LabelSpVel: TLabel;
    LabelCaption: TLabel;
    LabelJogVel: TLabel;
    Label3: TLabel;
    rgJogInc: TRadioGroup;
    sbJogVel: TScrollBar;
    ScrollBar1: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure rgJogIncClick(Sender: TObject);
    procedure sbJogVelChange(Sender: TObject);
    procedure Click(Sender: TObject);
  private
    OldORideLimits: Boolean;
    OldJogVel: integer;
    OldSpDir: integer;
    OldSpVel: double;
    OldSpORide: integer;
  public
    procedure ActivateSelf;
    procedure UpdateSelf;
    procedure InitControls;
    procedure MapButtons;
    procedure HandleCommand(Cmd: integer);
    function  HandleJogKeys(var Key: Word; Down,Fast: Boolean): Boolean;
  end;

var
  clJog: TJogClientForm;

implementation

{ TJogClientForm }

uses
  buttons,mocemc,
  mocglb,mocjoints,
  emc2pas,offsetdlg,tooleditdlg;


procedure TJogClientForm.HandleCommand(Cmd: integer);
begin
  case Cmd of
    //cmLIMITS: Emc.OverrideLimits;
    cmOFFSDLG: EditOffsets;
    cmTOOLS: EditTools;
  else
    Emc.HandleCommand(Cmd);
  end;
end;

procedure TJogClientForm.ActivateSelf;
begin
  if State.TaskMode <> TASKMODEMANUAL then
    raise Exception.Create('Cannot activate jogwindow when not in mode manual.');
  if not Visible then
    Visible:= true;
  MapButtons;
  InitControls;
end;

procedure TJogClientForm.UpdateSelf;
var
  i: integer;
begin
  {if ORideLimits <> State.ORideLimits then
    begin
      SetButtonDown(cmLIMITS,ORideLimits);
      OldORideLimits:= State.ORideLimits;
    end;}

  if OldSpDir <> State.SpDir then
    begin
      SetButtonDown(cmSPCW,State.SpDir > 0);
      SetButtonDown(cmSPCCW,State.SpDir < 0);
      OldSpDir:= State.SpDir;
    end;
  if (OldJogVel <> State.ActJogVel) or (State.UnitsChanged) then
    begin
      i:= Round(Emc.ToLinearUnits(State.ActJogVel));
      LabelJogVel.Caption:= IntToStr(i) + Vars.UnitVelStr;
      OldJogVel:= State.ActJogVel;
    end;

  if (OldSpVel <> State.SpSpeed) then
    begin
      LabelSpVel.Caption:= FloatToStr(State.SpSpeed) + ' U/min';
      OldSpVel:= State.SpSpeed;
    end;

  if (OldSpORide <> State.SpindleOverride) then
    begin
      LabelSpORide.Caption:= IntToStr(State.SpindleOverride) + '%';
      OldSpORide:= State.SpindleOverride;
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
  SetButtonEnabled(cmREFALL,Vars.HomingOrderDefined);
  { gtk scrollbars need a pagesize of 1!}
  sbJogVel.SetParams(State.ActJogVel,0,State.MaxJogVel,1);
  rgJogInc.Items.Clear;
  for i:= 0 to Vars.JogIncMax do
    rgJogInc.Items.Add(Vars.JogIncrements[i].Text);
  rgJogInc.ItemIndex:= 0;
  Vars.JogContinous:= True;
  OldORideLimits:= False;
  OldSpDir:= State.SpDir + 1;
  OldJogVel:= -1;
  OldSpVel:= -1;
  OldSpORide:= -1;
end;

function TJogClientForm.HandleJogKeys(var Key: Word;
  Down,Fast: Boolean): Boolean;

procedure Jog(Ch: Char; Dir: Integer);
var
  Speed: Double;
begin
  Key:= 0;
  if Fast then
    Speed:= State.MaxJogVel * Dir
  else
    Speed:= State.ActJogVel * Dir;
  if Vars.JogContinous then
    Joints.JogCont(Ch,Speed)
  else
    Joints.JogIncr(Ch,Speed,Vars.JogIncrement);
end;

procedure JogStop(Ch: Char);
begin
  if Vars.JogContinous then
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
  Vel:= sbJogVel.Position + 1;
  if Vel <> State.ActJogVel then
    begin
      State.ActJogVel:= Vel;
      LabelJogVel.Caption:= IntToStr(State.ActJogVel)+ Vars.UnitVelStr;
    end;
end;

procedure TJogClientForm.FormCreate(Sender: TObject);
begin
  Self.Tag:= TASKMODEMANUAL;
end;

procedure TJogClientForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if HandleJogKeys(Key,True,(ssShift in Shift)) then Key:= 0;
end;

procedure TJogClientForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if HandleJogKeys(Key,False,False) then Key:= 0;
end;

procedure TJogClientForm.rgJogIncClick(Sender: TObject);
var
  i: integer;
begin
  i:= rgJogInc.ItemIndex;
  if (i < 0) or (i > Vars.JogIncMax) then Exit;
  Vars.JogContinous:= (i = 0);
  Vars.jogIncrement:= Vars.JogIncrements[i].Value;
end;


initialization
  {$I jogclient.lrs}
  
end.

