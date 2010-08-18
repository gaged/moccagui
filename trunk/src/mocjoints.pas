unit mocjoints;

{$mode objfpc}
{$I mocca.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls,
  Graphics, Dialogs, ExtCtrls;

const
  MAX_JOINTS = 9;
  ZERO_POS_STRING = '+00000.000';
  ZERO_DES_STRING = '0';

const
  KeyDebounceTime = 100;

type
  TAxis = class
    constructor Create(PosId: integer; APanel: TWinControl; AxisNo: integer; Des: Char);
    destructor Destroy; override;
  private
    FAxisChar: Char;
    FAxisNumber: Integer;
    FDesLabel: TLabel;
    FHomed: Boolean;
    FJogging: Boolean;
    FLinear: Boolean;
    FPanel: TWinControl;
    FPosLabel: TLabel;
    FValue: Double;
    FTopLeft: TPoint;
    FJointMode: Boolean;
    FDiaMode: Boolean;
    FImage: TImage;
    procedure SetHomed(Value: Boolean);
    procedure SetDiaMode(Value: Boolean);
    procedure SetJointMode(Value: Boolean);
    procedure SizeLabels(x,y,w,h: integer);
    procedure Update(Relative,Dtg: Boolean);
    function GetAxisType: integer;
    function GetUnits: double;
    function GetBacklash: double;
    function GetMinPosLimit: double;
    function GetMaxPosLimit: double;
    function GetMinSoftLimit: double;
    function GetMaxSoftLimit: double;
    function GetMinHardLimit: double;
    function GetMaxHardLimit: double;
    function GetEnabled: boolean;
    function GetFault: boolean;
    function GetOverrideLimits: boolean;
    function ConvertUnits(Value: Double): Double;
   public
    property AxisChar: Char read FAxisChar;
    property AxisNumber: Integer read FAxisNumber;
    property Jogging: Boolean read FJogging write FJogging;
    property Linear: Boolean read FLinear;
    property Panel: TWinControl read FPanel write FPanel;
    property Homed: Boolean read FHomed write SetHomed;
    property DiaMode: Boolean read FDiaMode write SetDiaMode;
    property AxisType: integer read GetAxisType;
    property Units: double read GetUnits;
    property Backlash: Double read GetBacklash;
    property MinPosLimit: Double read GetMinPosLimit;
    property MaxPosLimit: double read GetMaxPosLimit;
    property MinSoftLimit: double read GetMinSoftLimit;
    property MaxSoftLimit: Double read GetMaxSoftLimit;
    property MinHardLimit: Double read GetMinHardLimit;
    property MaxHardLimit: Double read GetMaxHardLimit;
    property Enabled: Boolean read GetEnabled;
    property Fault: Boolean read GetFault;
    property OverrideLimits: Boolean read GetOverrideLimits;
    property JointMode: Boolean write SetJointMode;
  end;

type
  TJoints = class
   private
    FOldActiveAxis: integer;
    FBorderWidth: integer;
    FCoords: string;
    FBox: {$IFDEF LCLGTK2}TShape;{$ELSE}TBevel;{$ENDIF}
    FPanel: TPanel;
    FShowDia: Boolean;
    FShowRelative: Boolean;
    FShowDtg: Boolean;
    FNumAxes: Integer;
    FShowBox: Boolean;
    FJointMode: Boolean;
    FAxes: Array[0..MAX_JOINTS-1] of TAxis;
    procedure OnLabelClick(Sender: TObject);
    procedure SetShowBox(Value: Boolean);
    procedure SetShowDia(Value: Boolean);
    procedure SetJointMode(Value: Boolean);
    procedure OnIdle(Sender: TObject; var Handled: Boolean);
   public
    constructor Create(APanel: TPanel);
    destructor Destroy; override;
    function  AddAxis(Des: Char): TAxis;
    procedure CreateJoints;
    function  GetAxis(Index: integer): TAxis;
    function  GetAxisChar(Index: integer): Char;
    function  AxisByChar(Ch: Char): integer;
    procedure SetActiveChar(Ch: Char);
    procedure Update;
    procedure CheckJogExit;
    procedure DoResize(Sender: TObject);
    function  JogCont(Ax: Char; Speed: Double): integer;
    function  JogIncr(Ax: Char; Speed, Incr: Double): integer;
    function  JogStop(Ax: Char): integer;
    procedure JogStopAll;
    procedure HomeAll;
    procedure HomeActive;
    procedure UnHomeActive;
    procedure HomeAxis(Ax: Char);
    property  JointMode: Boolean read FJointMode write SetJointMode;
    property  ShowDia: Boolean read FShowDia write SetShowDia;
    property  ShowRelative: Boolean read FShowRelative write FShowRelative;
    property  ShowDtg: Boolean read FShowDtg write FShowDtg;
    property  BorderWidth: integer read FBorderWidth write FBorderWidth;
    property  ShowBox: Boolean write SetShowBox;
  end;

var
  Joints: TJoints;
  
implementation

uses
  emc2pas,mocglb,mocemc,hal,LCLIntf;

var
  Layout: TDROLayoutStyle;

  AxisTicks: Array[0..MAX_JOINTS - 1] of DWord;
  AxisJogging: Array[0..MAX_JOINTS - 1] of Boolean;
  JgSpeed: double;

function GetTickDiff(var t: DWord): longint;
var
  i: DWord;
begin
  i:= GetTickCount;
  if t = 0 then  // First use..
    Result:= 0
  else
    Result:= i - t;
  t:= i;
end;

constructor TAxis.Create(PosId: integer; APanel: TWinControl; AxisNo: integer; Des: Char);
begin
  FJointMode:= False;
  FJogging:= False;
  FValue:= -1;
  FAxisNumber:= AxisNo;
  FAxisChar:= Des;
  FPanel:= APanel;
  if Assigned(DroHomedBitmap) and Assigned(DroUnHomedBitmap) then
    begin
      FImage:= TImage.Create(FPanel);
      FImage.Parent:= FPanel;
    end
  else
    FImage:= nil;
  FPosLabel:= TLabel.Create(FPanel);
  with FPosLabel do
    begin
      Parent:= FPanel;
      Alignment:= taRightJustify;
      AutoSize:= False;
      Caption:= ZERO_POS_STRING;
      ParentFont:= False;
      Font.Assign(FPanel.Font);
      Layout:= tlCenter;
      Tag:= PosId;
    end;
  FDesLabel:= TLabel.Create(FPanel);
  with FDesLabel do
    begin
      Parent:= FPanel;
      Alignment:= taLeftJustify;
      AutoSize:= True;
      if (Vars.IsLathe) and (FAxisChar = 'X') then
        Caption:= 'RAD'
      else
        Caption:= FAxisChar;
      Layout:= tlCenter;
      ParentFont:= False;
      Font.Assign(FPanel.Font);
      Font.Color:= clRed;
      Tag:= PosId;
    end;
end;

destructor TAxis.Destroy;
begin
  FPosLabel.Free;
  FDesLabel.Free;
  inherited;
end;

function TAxis.ConvertUnits(Value: Double): Double;
var
  mm: double;
begin
  if FLinear then
    begin
      mm:= Value / State.LinearUnits;
      if Vars.ShowMetric then
        Result:= mm
      else
        Result:= mm / 25.4;
    end
  else
    Result:= Value;
end;

procedure TAxis.SizeLabels(x,y,w,h: integer);
var
  LeftPart,iw,ih: integer;
begin
  FDesLabel.Left:= x + 2;
  FDesLabel.Top:= y + ((h - FDesLabel.Height) div 2);
  LeftPart:= FDesLabel.Width + FDesLabel.Left + 2;
  if FImage <> nil then
    begin
      iw:= DroHomedBitmap.Width;
      ih:= DroHomedBitmap.Height;
      FImage.SetBounds(x + LeftPart,(h - ih) div 2,iw,ih);
      LeftPart:= LeftPart + iw + 2;
    end;
  FPosLabel.SetBounds(x+LeftPart,y,w-LeftPart-1,h-1);
  FTopLeft.Y:= y;
  FTopLeft.X:= x;
end;

procedure TAxis.SetHomed(Value: Boolean);
begin
  if FHomed <> Value then
    sendHome(FAxisNumber);
end;

procedure TAxis.SetDiaMode(Value: Boolean);
begin
  if (not Vars.IsLathe) or (FAxisChar <> 'X') then
    begin
      FDiaMode:= False;
      Exit;
    end;
  if Value <> FDiaMode then
    begin
      FDiaMode:= Value;
      if FDiaMode then
        FDesLabel.Caption:= 'DIA'
      else
        FDesLabel.Caption:= 'RAD'
    end;
end;

procedure TAxis.SetJointMode(Value: Boolean);
begin
  if FJointMode <> Value then
    begin
      FJointMode:= Value;
      if FJointMode then
        FDesLabel.Caption:= IntToStr(FAxisNumber)
      else
        FDesLabel.Caption:= FAxisChar;
    end;
end;

procedure TAxis.Update(Relative,Dtg: Boolean);
var
  NewPos: Double;
  IsHomed: Boolean;
begin
  if FJointMode then
    NewPos:= ConvertUnits(getJointPos(FAxisNumber))
  else
    begin
      if Dtg then
        NewPos:= ConvertUnits(getDtgPos(FAxisNumber))
      else
        begin
          if Relative then
            NewPos:= ConvertUnits(getRelPos(FAxisNumber))
          else
            NewPos:= ConvertUnits(getAbsPos(FAxisNumber));
        end;
    end;

  if Vars.IsLathe then
    if FDiaMode then
      NewPos:= NewPos * 2;

  if NewPos <> FValue then
    begin
      FValue:= NewPos;
      FPosLabel.Caption:=  PosToString(FValue);
    end;

  isHomed:= AxisHomed(FAxisNumber);
  if isHomed <> FHomed then
    begin
      FHomed:= isHomed;
      if FImage <> nil then
        begin
          if FHomed then
            FImage.Picture.Assign(DroHomedBitmap)
          else
            FImage.Picture.Assign(DroUnHomedBitmap);
        end
      else
        begin
          if FHomed then
            FDesLabel.Font.Color:= clGreen
          else
            FDesLabel.Font.Color:= clRed;
        end;
    end;
end;

function TAxis.GetAxisType: integer;
begin
  Result:= AxisAxisType(FAxisNumber);
end;

function TAxis.GetUnits: double;
begin
  Result:= AxisUnits(FAxisNumber);
end;

function TAxis.GetBacklash: double;
begin
  Result:= AxisBacklash(FAxisNumber);
end;

function TAxis.GetMinPosLimit: double;
begin
  Result:= AxisMinPositionLimit(FAxisNumber);
end;

function TAxis.GetMaxPosLimit: double;
begin
  Result:= AxisMaxPositionLimit(FAxisNumber);
end;

function TAxis.GetMinSoftLimit: double;
begin
  Result:= AxisMinSoftLimit(FAxisNumber);
end;

function TAxis.GetMaxSoftLimit: double;
begin
  Result:= AxisMaxSoftLimit(FAxisNumber);
end;

function TAxis.GetMinHardLimit: double;
begin
  Result:= AxisMinHardLimit(FAxisNumber);
end;

function TAxis.GetMaxHardLimit: double;
begin
  Result:= AxisMaxHardLimit(FAxisNumber);
end;

function TAxis.GetEnabled: boolean;
begin
   Result:= AxisEnabled(FAxisNumber);
end;

function TAxis.GetFault: boolean;
begin
  Result:= AxisFault(FAxisNumber);
end;

function TAxis.GetOverrideLimits: boolean;
begin
  Result:= AxisOverrideLimits(FAxisNumber);
end;

constructor TJoints.Create(APanel: TPanel);
begin
  Layout:= DROLayoutStyle; // read from global config.
  FBorderWidth:= 4;
  FPanel:= APanel;
  FNumAxes:= 0;
  {$IFDEF LCLGTK2}
  FBox:= TShape.Create(FPanel);
  FBox.Shape:= stRectangle;
  {$ELSE}
  FBox:= TBevel.Create(FPanel);
  FBox.Shape:= bsBox;
  {$ENDIF}
  FBox.Parent:= FPanel;
  FBox.Visible:= False;
  FOldActiveAxis:= -1;
  //FShowActual:= Vars.ShowActual;
  FShowRelative:= Vars.ShowRelative;
  FShowDtg:= False;
end;

destructor TJoints.Destroy;
var
  i: integer;
begin
  Application.OnIdle:= nil;
  if FNumAxes > 0 then
    for i:= 0 to FNumAxes - 1 do
      if Assigned(FAxes[i]) then
        FAxes[i].Free;
  FNumAxes:= 0;
  if Assigned(FBox) then
    FBox.Free;
  inherited;
end;

procedure TJoints.OnIdle(Sender: TObject; var Handled: Boolean);
var
  i: integer;
begin
  if FNumAxes > 0 then
    for i:= 0 to FNumAxes - 1 do
      if AxisJogging[i] <> FAxes[i].Jogging then
        begin
          AxisJogging[i]:= FAxes[i].Jogging;
          if AxisJogging[i] then
            begin
              sleep(10);
              sendJogCont(FAxes[i].AxisNumber,JgSpeed);
            end
          else
            begin
              sleep(10);
              SendJogStop(FAxes[i].AxisNumber);
            end;
        end;
end;

procedure TJoints.SetShowBox(Value: Boolean);
begin
  if FShowBox <> Value then
    begin
      FBox.Visible:= Value;
      FShowBox:= Value;
    end;
end;

procedure TJoints.SetShowDia(Value: Boolean);
var
  i: integer;
begin
  if FNumAxes < 2 then Exit;
  if Vars.IsLathe then
    for i:= 0 to FNumAxes - 1 do
      FAxes[i].DiaMode:= Value;
  FShowDia:= Value;
end;

procedure TJoints.CheckJogExit;
var
  i: integer;
begin
  if FNumAxes > 0 then
    for i:= 0 to FNumAxes - 1 do
      if FAxes[i].Jogging then
        begin
          sendJogStop(FAxes[i].AxisNumber);
          Emc.WaitDone;
          FAxes[i].Jogging:= False;
        end;
end;

procedure TJoints.HomeAll;
begin
  SendHome(-1);
end;

procedure TJoints.HomeActive;
begin
  if (Vars.ActiveAxis >= 0) and (Vars.ActiveAxis < FNumAxes) then
    sendHome(Vars.ActiveAxis);
end;

procedure TJoints.UnHomeActive;
begin
  if (Vars.ActiveAxis >= 0) and (Vars.ActiveAxis < FNumAxes) then
    sendUnHome(Vars.ActiveAxis);
end;

procedure TJoints.HomeAxis(Ax: Char);
var
  i: integer;
begin
  i:= AxisByChar(Ax);
  if i < 0 then Exit;
  sendHome(FAxes[i].AxisNumber);
end;

function TJoints.JogIncr(Ax: Char; Speed,Incr: Double): integer;
var
  d: Dword;
  i: integer;
begin
  Result:= -1;
  i:= AxisByChar(Ax);
  if i < 0 then Exit;
  d:= GetTickDiff(AxisTicks[i]);
  if d < KeyDebounceTime then
    Exit;
  Result:= sendJogIncr(FAxes[i].AxisNumber,Speed,Incr);
  if i <> Vars.ActiveAxis then
    Vars.ActiveAxis:= i;
end;

function TJoints.JogCont(Ax: Char; Speed: Double): integer;
var
  i: Integer;
begin
  Result:= -1;
  i:= AxisByChar(Ax);
  if (i < 0) then
    Exit;
  if FAxes[i].Jogging then
    Exit;
  JgSpeed:= Speed;
  FAxes[i].Jogging:= True;
  if i <> Vars.ActiveAxis then
    Vars.ActiveAxis:= i;
  Result:= i;
end;

function TJoints.JogStop(Ax: Char): integer;
var
  i: Integer;
begin
  Result:= -1;
  i:= AxisByChar(Ax);
  if i < 0 then Exit;
  if not FAxes[i].Jogging then
    Exit;
  FAxes[i].Jogging:= False;
  if i <> Vars.ActiveAxis then
    Vars.ActiveAxis:= i;
end;

procedure TJoints.JogStopAll;
var
  i: integer;
begin
  if FNumAxes > 0 then
    for i:= 0 to FNumAxes - 1 do
      if FAxes[i].Jogging then
        sendJogStop(i);
end;

procedure TJoints.OnLabelClick(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(Sender) then
    with Sender as TLabel do
      begin
        i:= Tag;
        if (i < 0) or (i >= FNumAxes) then
          Exit;
        Vars.ActiveAxis:= i;
    end;
end;

function TJoints.AddAxis(Des: Char): TAxis;
var
  A: TAxis;
  i: integer;
// bugfix
begin
  Result:= nil;
  i:= Pos(Des,Mask);
  if i < 0 then
    begin
      writeln('illegal Axis char: ' + Des);
      Exit;
    end;
  if FNumAxes < MAX_JOINTS - 1 then
    begin
      A:= TAxis.Create(FNumAxes,FPanel,i-1,Des);
      if Assigned(A) then
        begin
          A.FDesLabel.OnClick:= @OnLabelClick;
          A.FPosLabel.OnClick:= @OnLabelClick;
        end
      else
        Exit;
      FAxes[FNumAxes]:= A;
      Inc(FNumAxes);
    end;
  Result:= A;
end;

function TJoints.GetAxis(index: integer): TAxis;
begin
  if (index < 0) or (index > FNumAxes - 1) then
    Result:= nil
  else
    Result:= FAxes[index];
end;

function TJoints.AxisByChar(Ch: Char): integer;
var
  i: integer;
begin
  i:= Pos(Ch,FCoords);
  if i > 0 then
    Result:= i-1
  else
    Result:= -1;
end;

function TJoints.GetAxisChar(Index: integer): Char;
begin
  Result:= #0;
  if (index < 0) or (index > FNumAxes - 1) then
    Exit;
  if Assigned(FAxes[index]) then
    Result:= GetAxis(index).AxisChar;
end;

procedure TJoints.SetActiveChar(Ch: Char);
var
  i: integer;
begin
  i:= AxisByChar(UpCase(Ch));
  if i < 0 then Exit;
  Vars.ActiveAxis:= i;
end;

procedure TJoints.CreateJoints;
var
  i: integer;
  c: Char;
  Ax: TAxis;
begin
  FCoords:= '';
  for i:= 0 to MaxAxes - 1 do
    begin
      if (Vars.Axis[i].AxisChar <> #0) then
        FCoords:= Fcoords + Vars.Axis[i].AxisChar
      else
        FCoords:= FCoords + #32;
    end;
  if Verbose > 0 then writeln('Create Joints: ' + FCoords);
  for i:= 1 to Length(FCoords) do
    if FCoords[i] <> #32 then
      begin
        c:= FCoords[i];
        Ax:= AddAxis(c);
        if Ax <> nil then
          begin
            Ax.FLinear:= Vars.Axis[i-1].IsLinear;
            if Vars.IsLathe and (c = 'X') then
              Ax.DiaMode:= True;
          end;
        if Verbose > 0 then writeln('Created joint: ' + c);
      end;
  Application.OnIdle:= @Self.OnIdle;
end;

procedure TJoints.SetJointMode(Value: Boolean);
var
  i: integer;
begin
  if FNumAxes < 1 then Exit;
  for i:= 0 to FNumAxes - 1 do
    FAxes[i].JointMode:= Value;
  FJointMode:= Value;
end;

procedure TJoints.Update;
var
  i: integer;
begin
  if FNumAxes < 1 then Exit;
  for i:= 0 to FNumAxes - 1 do
    FAxes[i].Update(FShowRelative,FShowDtg);
  if FShowBox then
    if FOldActiveAxis <> Vars.ActiveAxis then
      if (Vars.ActiveAxis >= 0) and (Vars.ActiveAxis < FNumAxes) then
        begin
          FBox.Top := FAxes[Vars.ActiveAxis].FTopLeft.Y;
          FBox.Left := FAxes[Vars.ActiveAxis].FTopLeft.X;
          FOldActiveAxis:= Vars.ActiveAxis;
          SetHalJogAxis(GetAxisChar(Vars.ActiveAxis));
        end;
end;

procedure TJoints.DoResize(Sender: TObject);
var
  w,h,x,y,i: integer;
begin
  if (FNumAxes < 1) or (not Assigned(FPanel)) then
    Exit;
  if Layout = dlsVertical then
    begin
      h:= FPanel.ClientHeight div FNumAxes;
      w:= FPanel.ClientWidth - 2;
      y:= (FPanel.CLientHeight - (h * FNumAxes)) div 2;
      y:= y + 1;
      if y < 0 then y:= 0;
      for i:= 0 to FNumAxes - 1 do
        begin
          FAxes[i].SizeLabels(0,y,w,h);
          y:= y + h + 1;
        end;
      FBox.Left:= 0;
      FBox.Width:= FPanel.ClientWidth;
      FBox.Height:= h-2;
    end
  else
    begin
      h:= FPanel.ClientHeight - 2;
      w:= FPanel.ClientWidth div FNumAxes;
      x:= (FPanel.ClientWidth - (w * FNumAxes)) div 2;
      x:= x + 1;
      if x < 0 then x:= 0;
      for i:= 0 to FNumAxes - 1 do
        begin
          FAxes[i].SizeLabels(x,0,w,h);
          x:= x + w + 1;
        end;
      FBox.Width:= w - 2;
      FBox.Height:= FPanel.ClientHeight;
    end;
end;

end.
