unit jogclient;

{$I mocca.inc}

interface

uses
  Classes, mocslider, mocbtn, mocled, SysUtils, LResources, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type

  { TJogClientForm }

  TJogClientForm = class(TForm)
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
    MocButtonInc0: TMocButton;
    MocButtonInc1: TMocButton;
    MocButtonInc2: TMocButton;
    MocButtonInc3: TMocButton;
    MocButtonInc4: TMocButton;
    MocButtonInc5: TMocButton;
    MocButtonInc6: TMocButton;
    MocButtonInc7: TMocButton;
    BtnORideLimits: TMocButton;
    LedORide: TMocLed;
    SliderJog: TSlider;
    LabelJ1: TLabel;
    LabelJ2: TLabel;
    procedure BJogMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BJogMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BJogMouseLeave(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure UserClick(Sender: TObject);
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
    procedure SetButtonParams(Index: integer; s: string);
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
  lcltype,
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
      writeln('jog command:',Cmd);
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

procedure TJogClientForm.SetButtonParams(Index: integer; s: string);
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
         MocButtonInc5.Enabled:= true;
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
  if State.Mode <> TASKMODEMANUAL then
    raise Exception.Create('Cannot activate jogwindow when not in mode manual.');
  if not Visible then
    Visible:= true;
  FCurrentMap:= 0;
  MapButtons;
  InitControls;
end;

procedure TJogClientForm.UpdateButtons;
begin
end;

procedure TJogClientForm.UpdateSelf;
var
  i: integer;
begin

  if State.State = STATE_ON then
    if Vars.JogContinous then
      if FBtnDown <> 0 then
        UpdateBtnState(True);

  if OldMachineOn <> (State.State = STATE_ON) then
    begin
      OldMachineOn:= (State.State = STATE_ON);
      UpdateButtons;
    end;

  if OldORideLimits <> State.ORideLimits then
    begin
      OldORideLimits:= State.ORideLimits;
      LedOride.IsOn:= OldORideLimits;
    end;

  if (OldJogVel <> State.ActJogVel) or (State.UnitsChanged) then
    begin
      i:= Round(Emc.ToDisplayUnits(State.ActJogVel));
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
  SetButtonMap(V,@Self.UserClick);
  UpdateButtons;
end;

procedure TJogClientForm.InitControls;
begin
  SetButtonDown(cmJOG,True);
  SetButtonEnabled(cmREFALL,Vars.HomingOrderDefined);
  SliderJog.SetParams(0,State.MaxJogVel,State.ActJogVel);
  OldJogVel:= -1;
  OldORideLimits:= not State.ORideLimits;
  OldMachineOn:= not (State.State = STATE_ON)
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
      {$IFDEF LCLGKT2}
      //Sleep(KeySleepAfterUp);
      {$ENDIF}
    end;
  Key:= 0;
end;

begin
  if Down then
    begin
      if Vars.IsLathe then
        begin
          case Key of
            VK_LEFT,VK_NUMPAD4: Jog('Z',-1);
            VK_RIGHT,VK_NUMPAD6: Jog('Z', 1);
            VK_UP: Jog('X',-1);
            VK_DOWN: Jog('X',1);
          end;
        end
      else
        begin
          case Key of
            VK_PRIOR: Jog('Z', 1);
            VK_NEXT: Jog('Z',-1);
            VK_LEFT,VK_NUMPAD4: Jog('X',-1);
            VK_RIGHT,VK_NUMPAD6: Jog('X', 1);
            VK_UP: Jog('Y', 1);
            VK_DOWN: Jog('Y',-1);
          end;
        end;
    end
  else
    begin
      if Vars.IsLathe then
        begin
          case Key of
            VK_LEFT,VK_RIGHT,VK_NUMPAD4,VK_NUMPAD6: JogStop('Z');
            VK_UP,VK_DOWN: JogStop('X');
          end;
        end
      else
        begin
          case Key of
            VK_PRIOR,VK_NEXT: JogStop('Z');
            VK_LEFT,VK_RIGHT,VK_NUMPAD4,VK_NUMPAD6: JogStop('X');
            VK_UP,VK_DOWN: JogStop('Y');
          end;
      end // case
    end;
  Result:= (Key = 0);
end;

procedure TJogClientForm.UserClick(Sender: TObject);
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
  writeln('Slider changed');
  if Sender = nil then ;
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
const
  MSG1 = 'No Jog-Increments set in config-file';
  MSG2 = 'Using default configuration';
begin
  if Sender = nil then ;
  ReadStyle(Self,'jog.xml');
  Self.Tag:= TASKMODEMANUAL;
  FCurrentMap:= 0;
  FBtnDown:= 0;
  if Vars.JogIncMax < 1 then
    raise Exception.Create('Error: Jogincrement, maximum < 1');
  for i:= 0 to Vars.JogIncMax do
    SetButtonParams(i,Vars.JogIncrements[i].Text);
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

  BtnORideLimits.Tag:= cmLIMITS;
end;

procedure TJogClientForm.FormDestroy(Sender: TObject);
begin
  writeln('jog destroy');
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
  if Sender = nil then ;
  UpdateBtnState(False);
  FBtnDown:= 0;
end;

procedure TJogClientForm.BJogMouseLeave(Sender: TObject);
begin
  UpdateBtnState(False);
  FBtnDown:= 0;
end;

procedure TJogClientForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Verbose > 0 then
    writeln('close jog-form');
end;

procedure TJogClientForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Sender = nil then ;
  if HandleJogKeys(Key,True,(ssShift in Shift)) then Key:= 0;
end;

procedure TJogClientForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Sender = nil then ;
  if not Assigned(Joints) then Exit;
  case Key of
    'u'..'z': Joints.SetActiveAxis(Key);
    '+': ChangeIncrement(1);
    '-': ChangeIncrement(-1);
  end;
  Key:= #0;
end;

procedure TJogClientForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Sender = nil then ;
  if HandleJogKeys(Key,False,False) then Key:= 0;
end;


initialization
  {$I jogclient.lrs}
  
end.

