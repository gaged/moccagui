unit jogclient;

{$I mocca.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls;

type

  { TJogClientForm }

  TJogClientForm = class(TForm)
    BXMinus: TButton;
    BBPlus: TButton;
    BCMinus: TButton;
    BCPlus: TButton;
    BXPlus: TButton;
    BYPlus: TButton;
    BZPlus: TButton;
    BYMinus: TButton;
    BZMinus: TButton;
    BAMinus: TButton;
    BAPlus: TButton;
    BBMinus: TButton;
    CbInc: TComboBox;
    LabelL1: TLabel;
    LabelCaption: TLabel;
    LabelJogVel: TLabel;
    LabelL2: TLabel;
    sbJogVel: TScrollBar;
    procedure BJogMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BJogMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CbIncClick(Sender: TObject);
    procedure sbJogVelChange(Sender: TObject);
    procedure Click(Sender: TObject);
  private
    OldORideLimits: Boolean;
    OldJogVel: integer;
    OldMachineOn: Boolean;
    FCurrentMap: integer;
    FBtnDown: integer;
  public
    procedure ActivateSelf;
    procedure UpdateBtnState(Down: Boolean);
    procedure UpdateSelf;
    procedure InitControls;
    procedure MapButtons(MapAll: Boolean);
    procedure UpdateButtons;
    procedure ChangeButtons(ToMap: integer);
    procedure ChangeIncrements(i: integer);
    procedure HandleCommand(Cmd: integer);
    function  HandleJogKeys(var Key: Word; Down,Fast: Boolean): Boolean;
  end;

var
  clJog: TJogClientForm;

implementation

{ TJogClientForm }

uses
  mocemc,
  mocglb,mocjoints,
  emc2pas,runclient,
  mocbtn,scripts,
  LCLIntf;

procedure TJogClientForm.HandleCommand(Cmd: integer);
begin
  if (Cmd >= cmSCRIPTBASE) and (Cmd < cmSCRIPTS) then
    begin
      RunScript(Cmd);
      ChangeButtons(0);
      Exit;
    end;
  case cmd of
    cmBACK: ChangeButtons(0);
    cmREF: ChangeButtons(1);
    cmTOUCHOFF: ChangeButtons(2);
    cmTOOLS: ChangeButtons(3);
    cmSCRIPTS: ChangeButtons(4);
    cmINCRUP: ChangeIncrements(1);
    cmINCRDN: ChangeIncrements(-1);
  else
    begin
      Emc.HandleCommand(Cmd);
      ChangeButtons(0);  // Back to default
    end;
  end;
end;

procedure TJogClientForm.UpdateBtnState(Down: Boolean);
const
  CoordMap = 'XYZABC';
  CoordMax = 6;
var
  Ch: Char;
  i,dir: integer;
  Speed: Double;
begin
  if FBtnDown = 0 then Exit;
  i:= Abs(FBtnDown);
  if i > CoordMax then Exit;
  Ch:= CoordMap[i];
  if Down then
    begin
      if FBtnDown < 0 then
        dir:= -1
      else
        dir:= 1;
      Speed:= State.ActJogVel * Dir;
      if Vars.JogContinous then
        begin
          Joints.JogCont(Ch,Speed);
        end
      else
        begin
          Joints.JogIncr(Ch,Speed,Vars.JogIncrement);
          FBtnDown:= 0;
        end;
    end
  else
    begin
      Joints.JogStop(Ch);
      FBtnDown:= 0;
    end;
end;

procedure TJogClientForm.ChangeIncrements(i: integer);
var
  n: integer;
begin
  n:= CbInc.ItemIndex + i;
  if n < 0 then
    n:= CbInc.Items.Count - 1
  else
  if n > CbInc.Items.Count - 1 then
    n:= 0;
  CbInc.ItemIndex:= n;
  CbIncClick(nil);
end;

procedure TJogClientForm.ChangeButtons(ToMap: Integer);
begin
  if (ToMap < 0) or (ToMap > 4) then Exit;
  if ToMap <> FCurrentMap then
    begin
      FCurrentMap:= ToMap;
      MapButtons(False);
      UpdateButtons;
    end;
end;

procedure TJogClientForm.ActivateSelf;
begin
  if State.TaskMode <> TASKMODEMANUAL then
    raise Exception.Create('Cannot activate jogwindow when not in mode manual.');
  if not Visible then
    Visible:= true;
  FCurrentMap:= 0;
  MapButtons(True);
  InitControls;
end;

procedure TJogClientForm.UpdateButtons;
var
  i,ii: integer;
begin
  for i:= 0 to NumSButtons - 1 do
    begin
      ii:= i + NumMButtons;
      if Assigned(MocBtns[ii]) then
        MocBtns[ii].Enabled:= State.Machine;
    end;
  SetButtonEnabled(cmSPMINUS,State.Machine);
  SetButtonEnabled(cmSPCW,State.Machine);
  SetButtonEnabled(cmSPCCW,State.Machine);
  SetButtonEnabled(cmSPPLUS,State.Machine);
  SetButtonEnabled(cmFLOOD,State.Machine);
end;

procedure TJogClientForm.UpdateSelf;
var
  i: integer;
  s: string;
begin

  if State.Machine then
    if Vars.JogContinous then
      if FBtnDown <> 0 then
        UpdateBtnState(True);

  if OldMachineOn <> State.Machine then
    begin
      OldMachineOn:= State.Machine;
      UpdateButtons;
    end;

  if OldORideLimits <> State.ORideLimits then
    begin
      SetButtonDown(cmLIMITS,State.ORideLimits);
      OldORideLimits:= State.ORideLimits;
    end;

  if (OldJogVel <> State.ActJogVel) or (State.UnitsChanged) then
    begin
      i:= Round(Emc.ToLinearUnits(State.ActJogVel));
      LabelJogVel.Caption:= IntToStr(i) + Vars.UnitVelStr;
      OldJogVel:= State.ActJogVel;
    end;

end;

procedure TJogClientForm.MapButtons(MapAll: Boolean);
var
  S: PSButtonArray;
  M: PMButtonArray;
begin
  S:= nil; M:= nil;
  case FCurrentMap of
    0: S:= @BtnDefJogMain;
    1: S:= @BtnDefJogRef;
    2: S:= @BtnDefJogTouch;
    3: S:= @BtnDefJogTool;
    4: S:= @BtnDefScripts;
  end;
  if MapAll then
    M:= @BtnDefJog;
  SetButtonMap(M,S,@Self.Click);
  UpdateButtons;
end;

procedure TJogClientForm.InitControls;
var
  i: integer;
begin
  SetButtonDown(cmJOG,True);
  SetButtonEnabled(cmREFALL,Vars.HomingOrderDefined);
  sbJogVel.SetParams(State.ActJogVel,0,State.MaxJogVel,1);
  CbInc.Items.Clear;
  for i:= 0 to Vars.JogIncMax do
    CbInc.Items.Add(Vars.JogIncrements[i].Text);
  CbInc.ItemIndex:= 0;
  Vars.JogContinous:= True;
  OldJogVel:= -1;
  OldORideLimits:= not State.ORideLimits;
  OldMachineOn:= not State.Machine;
  if Pos('A',Vars.CoordNames) < 1 then
    begin
      BAPlus.Enabled:= False;
      BAMinus.Enabled:= False;
    end;
  if Pos('B',Vars.CoordNames) < 1 then
    begin
      BBPlus.Enabled:= False;
      BBMinus.Enabled:= False;
    end;
  if Pos('C',Vars.CoordNames) < 1 then
    begin
      BCPlus.Enabled:= False;
      BCMinus.Enabled:= False;
    end;
  if Pos('Z',Vars.CoordNames) < 1 then
    begin
      BZPlus.Enabled:= False;
      BZMinus.Enabled:= False;
    end;
  if Pos('Y',Vars.CoordNames) < 1 then
    begin
      BYPlus.Enabled:= False;
      BYMinus.Enabled:= False;
    end;
  if Pos('X',Vars.CoordNames) < 1 then
    begin
      BXPlus.Enabled:= False;
      BXMinus.Enabled:= False;
    end;
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
      {$IFNDEF LCLGTK2}
      Sleep(10);
      {$ENDIF}
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
    with Sender as TMocButton do
    HandleCommand(Tag)
end;

procedure TJogClientForm.sbJogVelChange(Sender: TObject);
var
  Vel: integer;
begin
  if UpdateLock then Exit;
  Vel:= sbJogVel.Position;
  if Vel <> State.ActJogVel then
    begin
      if Vel < 1 then Vel:= 1;
      State.ActJogVel:= Vel;
      LabelJogVel.Caption:= IntToStr(State.ActJogVel)+ Vars.UnitVelStr;
    end;
end;

procedure TJogClientForm.FormCreate(Sender: TObject);
begin
  Self.Tag:= TASKMODEMANUAL;
  FCurrentMap:= 0;
  FBtnDown:= 0;
end;

procedure TJogClientForm.BJogMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  t: integer;
begin
  if Sender = nil then ;
  t:= TButton(Sender).Tag;
  FBtnDown:= t;
  UpdateBtnState(True);
end;

procedure TJogClientForm.BJogMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateBtnState(False);
  FBtnDown:= 0;
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

procedure TJogClientForm.CbIncClick(Sender: TObject);
var
  i: integer;
begin
  i:= CbInc.ItemIndex;
  if (i < 0) or (i > Vars.JogIncMax) then Exit;
  Vars.JogContinous:= (i = 0);
  Vars.jogIncrement:= Vars.JogIncrements[i].Value;
end;

initialization
  {$I jogclient.lrs}
  
end.

