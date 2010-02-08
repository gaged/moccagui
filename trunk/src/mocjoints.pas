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

  {$IFDEF LCLGTK2}
    KeyDelayTime = 20;
  {$ELSE}
    KeyDelayTime = 20;
  {$ENDIF}

type
  TAxis = class
    constructor Create(APanel: TWinControl; Id: integer; Des: Char);
    destructor Destroy;
  private
    //FActive: Boolean;
    FAxisChar: Char;
    FAxisNumber: Integer;
    FDesLabel: TLabel;
    FHomed: Boolean;
    FJogging: Boolean;
    FPanel: TWinControl;
    FPosLabel: TLabel;
    FValue: Double;
    FTop: integer;
    procedure SetHomed(const Value: Boolean);
    procedure SizeLabels(ActTop,FontHeight,BorderWidth: integer);
    procedure Update(ShowActual,ShowRelative: Boolean);
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
   public
    property AxisChar: Char read FAxisChar;
    property AxisNumber: Integer read FAxisNumber;
    property Jogging: Boolean read FJogging write FJogging;
    property Panel: TWinControl read FPanel write FPanel;
    property Top: integer read FTop;
    property Homed: Boolean read FHomed write SetHomed;
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
  end;
  
type
  TJoints = class
   private
    FOldActiveAxis: integer;
    FBorderWidth: integer;
    FCoords: string;
    FBox: {$IFDEF LCLGTK2}TShape;{$ELSE}TBevel;{$ENDIF}
    FPanel: TPanel;
    FShowActual: Boolean;
    FShowRelative: Boolean;
    FNumAxes: Integer;
    FShowBox: Boolean;
    FAxes: Array[0..MAX_JOINTS-1] of TAxis;
    procedure OnLabelClick(Sender: TObject);
    procedure SetShowBox(Value: Boolean);
   public
    constructor Create(APanel: TPanel);
    destructor Destroy;
    procedure AddAxis(Id: Integer; Des: Char);
    procedure CreateJoints(CoordNames: string; NumAxes: Integer);
    function  GetAxis(Index: integer): TAxis;
    function  GetAxisChar(Index: integer): Char;
    function  AxisByChar(Ch: Char): integer;
    procedure SetActiveChar(Ch: Char);
    procedure Update;
    procedure CheckJogExit;
    procedure DoResize(Sender: TObject);
    function  JogCont(Ch: Char; Speed: Double): integer;
    function  JogIncr(Ch: Char; Speed,Incr: Double): integer;
    function  JogStop(Ch: Char): integer;
    procedure JogStopAll;
    procedure HomeAll;
    procedure HomeActive;
    procedure HomeAxis(Ax: Char);
    property  ShowActual: Boolean read FShowActual write FShowActual;
    property  ShowRelative: Boolean read FShowRelative write FShowRelative;
    property  BorderWidth: integer read FBorderWidth write FBorderWidth;
    property  ShowBox: Boolean write SetShowBox;

  end;
  
var
  Joints: TJoints;
  
implementation

uses
  emc2pas,mocglb,mocemc,hal,LCLIntf;

var
  FullWidth: integer;
  AxisTicks: Array[0..MAX_JOINTS - 1] of longint;

function GetTickDiff(var t: longint): longint;
var
  i: longint;
begin
  i:= GetTickCount;
  if t = 0 then  // First use..
    Result:= KeyDelayTime
  else
    Result:= i - t;
  t:= i;
end;

function PosToString(const Value: Double): string;
var
  s: string;
begin
  Result:= '';
  if Value >= 0 then S:= '+';
  if Vars.Metric then
    S:= S + FloatToStrF(Value, ffFixed, 8, 3)
  else
    S:= S + FloatToStrF(Value, ffFixed, 8, 4);
  Result:= S;
end;

constructor TAxis.Create(APanel: TWinControl; Id: integer; Des: Char);
begin
  FValue:= -1;
  FAxisNumber:= Id;
  FAxisChar:= Des;
  FPanel:= APanel;
  FPosLabel:= TLabel.Create(FPanel);
  FDesLabel:= TLabel.Create(FPanel);
  with FPosLabel do
    begin
      Parent:= FPanel;
      Alignment:= taRightJustify;
      AutoSize:= False;
      Caption:= ZERO_POS_STRING;
      ParentFont:= False;
      Font.Assign(FPanel.Font);
      Layout:= tlCenter;
      Tag:= Id;
    end;
  with FDesLabel do
    begin
      Parent:= FPanel;
      Alignment:= taCenter;
      AutoSize:= False;
      Caption:= FAxisChar;
      Layout:= tlCenter;
      ParentFont:= False;
      Font.Assign(FPanel.Font);
      Font.Color:= clRed;
      Tag:= Id;
    end;
end;

destructor TAxis.Destroy;
begin
  FPosLabel.Free;
  FDesLabel.Free;
  inherited;
end;

procedure TAxis.SizeLabels(ActTop,FontHeight,BorderWidth: integer);
var
  X,CellW,W,i: integer;
begin
  CellW:= Round(FontHeight * 0.7);
  X:= (2*CellW) + BorderWidth;
  W:= 9 * CellW;
  FDesLabel.SetBounds(BorderWidth,ActTop,CellW,FontHeight);
  FPosLabel.SetBounds(X,ActTop,W,FontHeight);
  FTop:= ActTop;
  i:= X + W + BorderWidth;
  if FullWidth < i then
    FullWidth:= i;
end;

procedure TAxis.SetHomed(const Value: Boolean);
begin
  if FHomed <> Value then
    sendHome(FAxisNumber);
end;

procedure TAxis.Update(ShowActual,ShowRelative: Boolean);
var
  NewPos: Double;
  IsHomed: Boolean;
begin
  if ShowActual then
    begin
      if ShowRelative then
        NewPos:= getRelPos(FAxisNumber)
      else
        NewPos:= getAbsPos(FAxisNumber);
      if NewPos <> FValue then
        begin
          FValue:= NewPos;
          FPosLabel.Caption:= PosToString(FValue);
        end;
    end
  else
    begin
      if ShowRelative then
        NewPos:= getRelCmdPos(FAxisNumber)
      else
        NewPos:= getAbsCmdPos(FAxisNumber);
      if NewPos <> FValue then
        begin
          FValue:= NewPos;
          FPosLabel.Caption:= PosToString(FValue);
        end;
    end;
    
  isHomed:= AxisHomed(FAxisNumber);
  if isHomed <> FHomed then
    begin
      FHomed:= isHomed;
      if FHomed then
        FDesLabel.Font.Color:= clGreen
      else
        FDesLabel.Font.Color:= clRed;
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
  FShowActual:= Vars.ShowActual;
  FShowRelative:= Vars.ShowRelative;
end;

destructor TJoints.Destroy;
var
  i: integer;
begin
  if FNumAxes > 0 then
    for i:= 0 to FNumAxes - 1 do
      if Assigned(FAxes[i]) then
        FAxes[i].Free;
  FNumAxes:= 0;
  if Assigned(FBox) then
    FBox.Free;
end;

procedure TJoints.SetShowBox(Value: Boolean);
begin
  if FShowBox <> Value then
    begin
      FBox.Visible:= Value;
      FShowBox:= Value;
    end;
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

procedure TJoints.HomeAxis(Ax: Char);
var
  i: integer;
begin
  i:= AxisByChar(Ax);
  if i < 0 then Exit;
  sendHome(i);
end;

function TJoints.JogCont(Ch: Char; Speed: Double): integer;
var
  i: Integer;
  d: longint;
begin
  Result:= -1;
  i:= AxisByChar(Ch);
  if i < 0 then Exit;
  FAxes[i].Jogging:= True;
  {$IFDEF LCLGTK2}
  d:= GetTickDiff(AxisTicks[i]);
  if d < KeyDelayTime then
    begin
      {$IFDEF DEBUG_EMC}
       writeln('key skipped ',d);
      {$ENDIF}
      Exit;
    end;
  {$ENDIF}
  Result:= sendJogCont(i,Speed);
  {$IFNDEF LCLGKT2}
  Sleep(5);
  {$ENDIF}
  if i <> Vars.ActiveAxis then
    Vars.ActiveAxis:= i;
end;

function TJoints.JogIncr(Ch: Char; Speed,Incr: Double): integer;
var
  i: Integer;
begin
  Result:= -1;
  i:= AxisByChar(Ch);
  if i < 0 then Exit;
  FAxes[i].Jogging:= True;
  Result:= sendJogIncr(i,Speed,Incr);
  FAxes[i].Jogging:= False;
  if i <> Vars.ActiveAxis then
    Vars.ActiveAxis:= i;
end;

function TJoints.JogStop(Ch: Char): integer;
var
  i: Integer;
begin
  Result:= -1;
  i:= AxisByChar(Ch);
  if i < 0 then Exit;
  Result:= sendJogStop(i);
  if i <> Vars.ActiveAxis then
    Vars.ActiveAxis:= i;
  FAxes[i].Jogging:= False;
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

procedure TJoints.AddAxis(Id: Integer; Des: Char);
var
  A: TAxis;
begin
  if FNumAxes < MAX_JOINTS then
    begin
      A:= TAxis.Create(FPanel,Id,Des);
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
    Result:= GetAxis(Index).AxisChar;
end;

procedure TJoints.SetActiveChar(Ch: Char);
var
  i: integer;
begin
  i:= AxisByChar(UpCase(Ch));
  if i < 0 then
    Exit;
  Vars.ActiveAxis:= i;
end;

procedure TJoints.CreateJoints(CoordNames: string; NumAxes: integer);
var
  i: integer;
  c: Char;
begin
  if (Length(CoordNames) < 1) or (Length(CoordNames) > MAX_JOINTS) or
   (Length(CoordNames) <> NumAxes) then
     Exit;
  FCoords:= CoordNames;
  for i:= 1 to Length(CoordNames) do
    begin
      c:= CoordNames[i];
      AddAxis(i-1,c);
    end;
end;

procedure TJoints.Update;
var
  i: integer;
begin
  if FNumAxes < 1 then Exit;
  for i:= 0 to FNumAxes - 1 do
    begin
      FAxes[i].Update(FShowActual,FShowRelative);
    end;
  if FShowBox then
    if FOldActiveAxis <> Vars.ActiveAxis then
      if (Vars.ActiveAxis >= 0) and (Vars.ActiveAxis < FNumAxes) then
        begin
          FBox.Top:= FAxes[Vars.ActiveAxis].Top - 2;
          FOldActiveAxis:= Vars.ActiveAxis;
          SetHalJogAxis(GetAxisChar(Vars.ActiveAxis));
        end;
end;

procedure TJoints.DoResize(Sender: TObject);
var
  i,y,h: Integer;
begin
  if (FNumAxes < 1) or (not Assigned(FPanel)) then
    Exit;
  h:= Abs(FPanel.Font.Height);
  y:= FBorderWidth;
  for i:= 0 to FNumAxes - 1 do
    begin
      FAxes[i].SizeLabels(Y,H,FBorderWidth);
      y:= y + BorderWidth + h;
    end;
  FBox.Left:= 0;
  FBox.Width:= FullWidth; // FPanel.ClientWidth;
  FBox.Height:= h + 1;
  //if FPanel.ClientWidth < FullWidth + 1 then
  FPanel.ClientWidth:= FullWidth + 1;
  FPanel.ClientHeight:= y + 1;
end;

end.
