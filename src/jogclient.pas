unit jogclient;

{$I mocca.inc}

interface

uses
  Classes, mocslider, mocbtn, mocled, SysUtils, LResources, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type

  { TJogClientForm }

  TJogClientForm = class(TForm)
    BtnORideLimits: TMocButton;
    BXMinus: TMocButton;
    BBPlus: TMocButton;
    BCMinus: TMocButton;
    BCPlus: TMocButton;
    BXPlus: TMocButton;
    BYPlus: TMocButton;
    BZPlus: TMocButton;
    BYMinus: TMocButton;
    BZMinus: TMocButton;
    BAMinus: TMocButton;
    BAPlus: TMocButton;
    BBMinus: TMocButton;
    LabelJ1: TLabel;
    LabelJ2: TLabel;
    LedORide: TMocLed;
    MocButtonInc0: TMocButton;
    MocButtonInc1: TMocButton;
    MocButtonInc2: TMocButton;
    MocButtonInc3: TMocButton;
    MocButtonInc4: TMocButton;
    MocButtonInc5: TMocButton;
    MocButtonInc6: TMocButton;
    MocButtonInc7: TMocButton;
    SliderJog: TSlider;
    procedure BJogMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BJogMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Click(Sender: TObject);
    procedure MocButtonIncClick(Sender: TObject);
    procedure SliderJogPositionChanged(Sender: TObject; NewPos: integer);
  private
    OldORideLimits: Boolean;
    OldJogVel: integer;
    OldMachineOn: Boolean;
    FCurrentMap: integer;
    FBtnDown: integer;
    FIncrement: integer;
  public
    procedure ActivateSelf;
    procedure UpdateBtnState(Down: Boolean);
    procedure UpdateSelf;
    procedure InitControls;
    procedure MapButtons;
    procedure UpdateButtons;
    procedure SetIncrement(Index: integer);
    procedure SetButtonParams(Index: integer; s: string; ATag: integer);
    procedure SetButtonState(Index: integer; ADown: Boolean);
    procedure ChangeButtons(ToMap: integer);
    procedure ChangeIncrement(i: integer);
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
  scripts;

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
    cmINCRUP: ChangeIncrement(1);
    cmINCRDN: ChangeIncrement(-1);
  else
    begin
      Emc.HandleCommand(Cmd);
      if not (Cmd in [cmREFX..cmREFC,cmTOUCHX..cmTOUCHC]) then
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

procedure TJogClientForm.SetIncrement(Index: integer);
begin
  if (Index < 0) or (Index > Vars.JogIncMax) then
    Exit;
  if Index <> FIncrement then
    begin
      SetButtonState(FIncrement,False);
      FIncrement:= Index;
      SetButtonState(FIncrement,True);
      Vars.JogContinous:= (FIncrement = 0);
      Vars.JogIncrement:= Vars.JogIncrements[FIncrement].Value;
    end;
end;

procedure TJogClientForm.SetButtonParams(Index: integer; s: string; ATag: integer);
begin
  case Index of
    0: begin
         MocButtonInc0.Caption:= s;
       end;
    1: begin
         MocButtonInc1.Caption:= s;
         MocButtonInc1.Tag:= 1;
         MocButtonInc1.Enabled:= true;
       end;
    2: begin
         MocButtonInc2.Caption:= s;
         MocButtonInc2.Tag:= 2;
         MocButtonInc2.Enabled:= true;
       end;
    3: begin
         MocButtonInc3.Caption:= s;
         MocButtonInc3.Tag:= 3;
         MocButtonInc3.Enabled:= true;
       end;
    4: begin
         MocButtonInc4.Caption:= s;
         MocButtonInc4.Tag:= 4;
         MocButtonInc4.Enabled:= true;
       end;
    5: begin
         MocButtonInc5.Caption:= s;
         MocButtonInc5.Tag:= 5;
         MocButtonInc6.Enabled:= true;
       end;
    6: begin
         MocButtonInc6.Caption:= s;
         MocButtonInc6.Tag:= 6;
         MocButtonInc6.Enabled:= true;
       end;
    7: begin
         MocButtonInc7.Caption:= s;
         MocButtonInc7.Tag:= 7;
         MocButtonInc7.Enabled:= true;
       end;
   end;
end;

procedure TJogClientForm.SetButtonState(Index: integer; ADown: Boolean);
begin
  case Index of
    0: MocButtonInc0.Down:= ADown;
    1: MocButtonInc1.Down:= ADown;
    2: MocButtonInc2.Down:= ADown;
    3: MocButtonInc3.Down:= ADown;
    4: MocButtonInc4.Down:= ADown;
    5: MocButtonInc5.Down:= ADown;
    6: MocButtonInc6.Down:= ADown;
    7: MocButtonInc7.Down:= ADown;
   end;
end;

procedure TJogClientForm.ChangeIncrement(i: integer);
var
  n: integer;
begin
  n:= FIncrement + i;
  if n < 0 then
    n:= 0;
  if  n > Vars.JogIncMax then
    n:= Vars.JogIncMax;
  SetIncrement(n);
end;

procedure TJogClientForm.ChangeButtons(ToMap: Integer);
begin
  if (ToMap < 0) or (ToMap > 4) then Exit;
  if ToMap <> FCurrentMap then
    begin
      FCurrentMap:= ToMap;
      MapButtons;
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
  MapButtons;
  InitControls;
end;

procedure TJogClientForm.UpdateButtons;
begin
  {for i:= 0 to NumButtons - 1 do
    begin
      if Assigned(MocBtns[i]) then
        MocBtns[i].Enabled:= State.Machine;
    end;
  SetButtonEnabled(cmSPMINUS,State.Machine);
  SetButtonEnabled(cmSPCW,State.Machine);
  SetButtonEnabled(cmSPCCW,State.Machine);
  SetButtonEnabled(cmSPPLUS,State.Machine);
  SetButtonEnabled(cmFLOOD,State.Machine);}
end;

procedure TJogClientForm.UpdateSelf;
var
  i: integer;
  // s: string;
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
      OldORideLimits:= State.ORideLimits;
      LedOride.IsOn:= OldORideLimits;
    end;

  if (OldJogVel <> State.ActJogVel) or (State.UnitsChanged) then
    begin
      i:= Round(Emc.ToLinearUnits(State.ActJogVel));
      SliderJog.Caption:= IntToStr(i) + Vars.UnitVelStr;
      OldJogVel:= State.ActJogVel;
    end;

end;

procedure TJogClientForm.MapButtons;
var
  V: PButtonArray;
begin
  V:= nil;
  case FCurrentMap of
    0: V:= @BtnDefJog;
    1: V:= @BtnDefJogRef;
    2: V:= @BtnDefJogTouch;
    3: V:= @BtnDefJogTool;
    4: V:= @BtnDefScripts;
  end;
  SetButtonMap(V,@Self.Click);
  UpdateButtons;
end;

procedure TJogClientForm.InitControls;
begin
  SetButtonDown(cmJOG,True);
  SetButtonEnabled(cmREFALL,Vars.HomingOrderDefined);
  SliderJog.SetParams(0,State.MaxJogVel,State.ActJogVel);
  OldJogVel:= -1;
  OldORideLimits:= not State.ORideLimits;
  OldMachineOn:= not State.Machine
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
      Sleep(KeySleepAfterUp);
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

procedure TJogClientForm.MocButtonIncClick(Sender: TObject);
begin
  if Sender = nil then Exit;
  with Sender as TMocButton do
    SetIncrement(Tag);
end;

procedure TJogClientForm.SliderJogPositionChanged(Sender: TObject; NewPos: integer);
var
  Vel: integer;
begin
  if UpdateLock then Exit;
  Vel:= SliderJog.Position;
  if Vel <> State.ActJogVel then
    begin
      if Vel < 1 then Vel:= 1;
      State.ActJogVel:= Vel;
      SliderJog.Caption:= IntToStr(State.ActJogVel)+ Vars.UnitVelStr;
    end;
end;

procedure TJogClientForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  ReadStyle(Self,'jog.xml');
  Self.Tag:= TASKMODEMANUAL;
  FCurrentMap:= 0;
  FBtnDown:= 0;
  if Vars.JogIncMax < 1 then
    begin
      writeln('No Jog-Increments set in config-file');
      writeln('Using default configuration');
      with Vars do
        begin
          JogIncrements[0].Text:= 'Durchgehend';
          JogIncrements[0].Value:= 0;
          JogIncrements[1].Text:= '1.00 mm';
          JogIncrements[1].Value:= 1;
          JogIncrements[2].Text:= '0.1 mm';
          JogIncrements[2].Value:= 0.1;
          JogIncrements[3].Text:= '0.01 mm';
          JogIncrements[3].Value:= 0.01;
          JogIncrements[4].Text:= '0.001 mm';
          JogIncrements[4].Value:= 0.001;
        end;
      Vars.JogIncMax:= 4;
    end;
  for i:= 0 to Vars.JogIncMax do
    SetButtonParams(i,Vars.JogIncrements[i].Text,i);
  Vars.JogContinous:= True;
  FIncrement:= 1;
  SetIncrement(0);

  if MocButtonInc1.Caption = '' then MocButtonInc1.Visible:= False;
  if MocButtonInc2.Caption = '' then MocButtonInc2.Visible:= False;
  if MocButtonInc3.Caption = '' then MocButtonInc3.Visible:= False;
  if MocButtonInc4.Caption = '' then MocButtonInc4.Visible:= False;
  if MocButtonInc5.Caption = '' then MocButtonInc5.Visible:= False;
  if MocButtonInc6.Caption = '' then MocButtonInc6.Visible:= False;
  if MocButtonInc7.Caption = '' then MocButtonInc7.Visible:= False;

   if Pos('A',Vars.CoordNames) < 1 then
    begin
      BAPlus.Visible:= False;
      BAMinus.Visible:= False;
    end;
  if Pos('B',Vars.CoordNames) < 1 then
    begin
      BBPlus.Visible:= False;
      BBMinus.visible:= False;
    end;
  if Pos('C',Vars.CoordNames) < 1 then
    begin
      BCPlus.Visible:= False;
      BCMinus.Visible:= False;
    end;
  if Pos('Z',Vars.CoordNames) < 1 then
    begin
      BZPlus.Visible:= False;
      BZMinus.Visible:= False;
    end;
  if Pos('Y',Vars.CoordNames) < 1 then
    begin
      BYPlus.Visible:= False;
      BYMinus.Visible:= False;
    end;
  if Pos('X',Vars.CoordNames) < 1 then
    begin
      BXPlus.Visible:= False;
      BXMinus.Visible:= False;
    end;
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

procedure TJogClientForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if not Assigned(Joints) then Exit;
  case Key of
    'u'..'z': Joints.SetActiveChar(Key);
    '+': ChangeIncrement(1);
    '-': ChangeIncrement(-1);
  end;
  Key:= #0;
end;

procedure TJogClientForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if HandleJogKeys(Key,False,False) then Key:= 0;
end;


initialization
  {$I jogclient.lrs}
  
end.

