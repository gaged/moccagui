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


type
  TAxis = class
    constructor Create(APanel: TPanel; Id: integer; Des: Char);
    destructor Destroy;
  private
    //FActive: Boolean;
    FAxisChar: Char;
    FAxisNumber: Integer;
    FDesLabel: TLabel;
    FHomed: Boolean;
    FJogging: Boolean;
    FPanel: TPanel;
    FPosLabel: TLabel;
    FValue: Double;
    FTop: integer;
    procedure SetHomed(const Value: Boolean);
    procedure SizeLabels(ActTop,FontHeight,BorderWidth: integer);
    procedure Update(ShowActual,ShowCommanded: Boolean);
   public
    property AxisChar: Char read FAxisChar;
    property AxisNumber: Integer read FAxisNumber;
    property Homed: Boolean read FHomed write SetHomed;
    property Jogging: Boolean read FJogging write FJogging;
    property Panel: TPanel read FPanel write FPanel;
    property Top: integer read FTop;
  end;
  
type
  TJoints = class
   private
    FOldActiveAxis: integer;
    FBorderWidth: integer;
    FCoords: string;
    FBox: TShape;
    FPanel: TPanel;
    FShowActual: Boolean;
    FShowCommanded: Boolean;
    FNumAxes: Integer;
    FAxes: Array[0..MAX_JOINTS-1] of TAxis;
    procedure OnLabelClick(Sender: TObject);
   public
    constructor Create(APanel: TPanel);
    destructor Destroy;
    procedure AddAxis(Id: Integer; Des: Char);
    procedure CreateJoints(CoordNames: string; NumAxes: Integer);
    function  GetAxis(Index: integer): TAxis;
    procedure Update;
    procedure DoResize(Sender: TObject);
    function  AxisByChar(Ch: Char): integer;
    function  JogStart(Ch: Char; Speed: Double): integer;
    function  JogStop(Ch: Char): integer;
    procedure JogStopAll;
    procedure HomeAll;
    procedure HomeActive;
    property  ShowActual: Boolean read FShowActual write FShowActual;
    property  ShowCommanded: Boolean read FShowCommanded write FShowCommanded;
    property  BorderWidth: integer read FBorderWidth write FBorderWidth;
  end;
  
var
  Joints: TJoints;
  
implementation

uses
  emc2pas,mocglb;

function PosToString(const Value: Double): string;
var
  s: string;
begin
  Result:= '';
  if Value >= 0 then S:= '+';
  S:= S + FloatToStrF(Value, ffFixed, 8, 3);
  Result:= S;
end;

constructor TAxis.Create(APanel: TPanel; Id: integer; Des: Char);
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
  X,CellW,W: integer;
begin
  CellW:= Round(FontHeight * 0.7);
  X:= (2*CellW) + BorderWidth;
  W:= 9 * CellW;
  FDesLabel.SetBounds(BorderWidth,ActTop,CellW,FontHeight);
  FPosLabel.SetBounds(X,ActTop,W,FontHeight);
  FTop:= ActTop;
end;

procedure TAxis.SetHomed(const Value: Boolean);
begin
  if FHomed <> Value then
    sendHome(FAxisNumber);
end;

procedure TAxis.Update(ShowActual,ShowCommanded: Boolean);
var
  NewPos: Double;
  IsHomed: Boolean;
begin
  if ShowActual then
    begin
      if ShowCommanded then
        NewPos:= getRelCmdPos(FAxisNumber)
      else
        NewPos:= getRelPos(FAxisNumber);
      if NewPos <> FValue then
        begin
          FValue:= NewPos;
          FPosLabel.Caption:= PosToString(FValue);
        end;
    end
  else
    begin
      if ShowCommanded then
        NewPos:= getAbsCmdPos(FAxisNumber)
      else
        NewPos:= getAbsPos(FAxisNumber);
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

constructor TJoints.Create(APanel: TPanel);
begin
  FBorderWidth:= 4;
  FPanel:= APanel;
  FNumAxes:= 0;
  FBox:= TShape.Create(FPanel);
  FBox.Parent:= FPanel;
  FBox.Shape:= stRectangle;
  FOldActiveAxis:= 0;
  FShowActual:= True;
  FShowCommanded:= False;
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

procedure TJoints.HomeAll;
begin
  SendHome(-1);
end;

procedure TJoints.HomeActive;
begin
  if (emcVars.ActiveAxis >= 0) and (emcVars.ActiveAxis < FNumAxes) then
    sendHome(emcVars.ActiveAxis);
end;

function TJoints.JogStart(Ch: Char; Speed: Double): integer;
var
  i: Integer;
begin
  Result:= -1;
  i:= AxisByChar(Ch);
  if i < 0 then Exit;
  FAxes[i].Jogging:= True;
  Result:= sendJogCont(i,Speed);
  if i <> emcVars.ActiveAxis then
    emcVars.ActiveAxis:= i;
end;

function TJoints.JogStop(Ch: Char): integer;
var
  i: Integer;
begin
  Result:= -1;
  i:= AxisByChar(Ch);
  if i < 0 then Exit;
  Result:= sendJogStop(i);
  if i <> emcVars.ActiveAxis then
    emcVars.ActiveAxis:= i;
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
        emcVars.ActiveAxis:= i;
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
    FAxes[i].Update(FShowActual,FShowCommanded);
  if FOldActiveAxis <> emcVars.ActiveAxis then
    if (emcVars.ActiveAxis >= 0) and (emcVars.ActiveAxis < FNumAxes) then
      begin
        FBox.Top:= FAxes[emcVars.ActiveAxis].Top - 2;
        FOldActiveAxis:= emcVars.ActiveAxis;
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
  FBox.Width:= FPanel.ClientWidth;
  FBox.Height:= h;
end;

end.
