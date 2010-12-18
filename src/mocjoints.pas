unit mocjoints;

{$mode objfpc}
{$I mocca.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, StdCtrls,
  Graphics, Dialogs, ExtCtrls;

const
  MAX_JOINTS = 9;
  ZERO_POS_STRING = '+0.000';
  ZERO_DES_STRING = '0';

const
  KeyDebounceTime = 100;

type
  TAxis = class
    constructor Create(Des: Char);
    destructor Destroy; override;
  private
    FAxisChar: Char;
    FAxisNumber: Integer;
    FHomed: Boolean;
    FJogging: Boolean;
    FLinear: Boolean;
    FPosition: Double;
    procedure SetHomed(Value: Boolean);
    procedure Update(Rel,Dtg,JointMode: Boolean);
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
    property AxisType: integer read GetAxisType;
    property Jogging: Boolean read FJogging write FJogging;
    property Linear: Boolean read FLinear;
    property Homed: Boolean read FHomed write SetHomed;
    property Units: double read GetUnits;
    property Enabled: Boolean read GetEnabled;
    property Fault: Boolean read GetFault;
    property Backlash: Double read GetBacklash;
    property Position: double read FPosition;
    property MinPosLimit: Double read GetMinPosLimit;
    property MaxPosLimit: double read GetMaxPosLimit;
    property MinSoftLimit: double read GetMinSoftLimit;
    property MaxSoftLimit: Double read GetMaxSoftLimit;
    property MinHardLimit: Double read GetMinHardLimit;
    property MaxHardLimit: Double read GetMaxHardLimit;
    property OverrideLimits: Boolean read GetOverrideLimits;
  end;

type
  TDroItem = record
    DesLabel: TLabel;
    PosLabel: TLabel;
    Text: string;
    Image: TImage;
    Position: double;
    Scale: double;
    px,py: integer;
    Axis: TAxis;
    OldHomed: Boolean;
  end;

type
  TDro = class
  private
    FActiveAxis: integer;
    FBorderWidth: integer;
    FRelative: Boolean;
    FDtg: Boolean;
    FJointMode: Boolean;
    FBox: {$IFDEF LCLGTK2}TShape;{$ELSE}TBevel;{$ENDIF}
    FShowBox: Boolean;
    FCount: integer;
    FPanel: TWinControl;
    FItems: Array[0..MAX_JOINTS - 1] of TDroItem;
    procedure SizeItem(i,x,y,w,h: integer);
    procedure SetShowBox(Value: Boolean);
    procedure SetJointMode(Value: Boolean);
    procedure OnLabelClick(Sender: TObject);
  public
    constructor Create(APanel: TWinControl);
    destructor Destroy; override;
    procedure Add(A: TAxis; AScale: Double; AText: string);
    procedure Update;
    procedure Size;
    property Relative: Boolean read FRelative write FRelative;
    property Dtg: Boolean read FDtg write FDtg;
    property ShowBox: Boolean read FShowBox write SetShowBox;
    property JointMode: Boolean read FJointMode write SetJointMode;
  end;

type
  TJoints = class
  private
    FNumAxes: Integer;
    FMap: string;
    FAxes: Array[0..MAX_JOINTS-1] of TAxis;
    procedure OnIdle(Sender: TObject; var Handled: Boolean);
    function  AddAxis(Des: Char): TAxis;
    function  GetAxis(c: Char): integer;
  public
    constructor Create(APanel: TPanel);
    destructor Destroy; override;
    procedure CreateJoints;
    //function  GetAxis(Index: integer): TAxis;
    //function  GetAxisChar(Index: integer): Char;
    procedure SetActiveAxis(Ch: Char);
    procedure Update;
    procedure CheckJogExit;
    function  JogCont(Ax: Char; Speed: Double): integer;
    function  JogIncr(Ax: Char; Speed, Incr: Double): integer;
    function  JogStop(Ax: Char): integer;
    procedure JogStopAll;
    procedure HomeAll;
    procedure HomeActive;
    procedure UnHomeActive;
    procedure HomeAxis(Ax: Char);
  end;

var
  DRO: TDro;
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

constructor TAxis.Create(Des: Char);
begin
  FJogging:= False;
  FAxisNumber:= Pos(Des,Vars.CoordMap) - 1;
  if FAxisNumber < 0 then
    raise Exception.Create('Create axis: invalid axis char: ' + Des);
  FAxisChar:= Des;
  FPosition:= -1;
end;

destructor TAxis.Destroy;
begin
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

procedure TAxis.SetHomed(Value: Boolean);
begin
  if FHomed <> Value then
    sendHome(FAxisNumber);
end;

procedure TAxis.Update(Rel,Dtg,JointMode: Boolean);
begin
  if JointMode then
    FPosition:= ConvertUnits(getJointPos(FAxisNumber))
  else
    begin
      if Dtg then
        FPosition:= ConvertUnits(getDtgPos(FAxisNumber))
      else
        begin
          if Rel then
            FPosition:= ConvertUnits(getRelPos(FAxisNumber))
          else
            FPosition:= ConvertUnits(getAbsPos(FAxisNumber));
        end;
    end;
  FHomed:= AxisHomed(FAxisNumber);
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

constructor TDro.Create(APanel: TWinControl);
begin
  Layout:= DROLayoutStyle; // read from global config.
  FBorderWidth:= 4;
  FCount:= 0;
  FPanel:= APanel;
  {$IFDEF LCLGTK2}
  FBox:= TShape.Create(FPanel);
  FBox.Shape:= stRectangle;
  {$ELSE}
  FBox:= TBevel.Create(FPanel);
  FBox.Shape:= bsBox;
  {$ENDIF}
  FBox.Parent:= FPanel;
  FBox.Visible:= False;
  FRelative:= Vars.ShowRelative;
  FJointMode:= False;
end;

destructor TDro.Destroy;
begin
  if Assigned(FBox) then
    FBox.Free;
  inherited;
end;

procedure TDro.SetShowBox(Value: Boolean);
begin
  if FShowBox <> Value then
    begin
      FBox.Visible:= Value;
      FShowBox:= Value;
    end;
end;

procedure TDro.SetJointMode(Value: Boolean);
var
  i: integer;
begin
  FJointMode:= Value;
  if FCount < 1 then Exit;
  for i:= 0 to FCount -1 do
    with FItems[i] do
      begin
        if not Assigned(Axis) then Exit;
        if FJointMode then
          DesLabel.Caption:= IntToStr(Axis.AxisNumber)
        else
          begin
            if Text = '' then
              DesLabel.Caption:= Axis.AxisChar
            else
              DesLabel.Caption:= Text;
          end;
      end;
end;

procedure TDro.OnLabelClick(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(Sender) then
    with Sender as TLabel do
      begin
        i:= Tag;
        if (i < 0) or (i > FCount - 1) then
          Exit;
        Vars.ActiveAxis:= i;
    end;
end;

procedure TDro.Add(A: TAxis; AScale: Double; AText: string);
var
  UseBmp: Boolean;
begin
  if not Assigned(A) then Exit;
  UseBmp:= Assigned(DroHomedBitmap) and Assigned(DroUnHomedBitmap);
  with FItems[FCount] do
    begin
      Axis:= A;
      PosLabel:= TLabel.Create(FPanel);
      DesLabel:= TLabel.Create(FPanel);
      PosLabel.OnClick:= @Self.OnLabelClick;
      DesLabel.OnClick:= @Self.OnLabelClick;
      Image:= nil;
      Position:= 0;
      Scale:= AScale;
      Text:= AText;
      px:= 0; py:= 0;
      if UseBmp then
        begin
          Image:= TImage.Create(FPanel);
          Image.Parent:= FPanel;
          Image.OnClick:= @Self.OnLabelClick;
          Image.Tag:= A.AxisNumber;
        end;
      with PosLabel do
        begin
          Parent:= FPanel;
          Alignment:= taRightJustify;
          AutoSize:= False;
          Caption:= ZERO_POS_STRING;
          ParentFont:= False;
          Font.Assign(FPanel.Font);
          Layout:= tlCenter;
          Tag:= A.AxisNumber;
        end;
      with DesLabel do
        begin
          Parent:= FPanel;
          Alignment:= taLeftJustify;
          AutoSize:= False;
          if Text <> '' then
            Caption:= Text
          else
            Caption:= A.AxisChar;
          Layout:= tlCenter;
          ParentFont:= False;
          Font.Assign(FPanel.Font);
          Font.Color:= clRed;
          Tag:= A.AxisNumber;
        end;
    end;
  inc(FCount);
end;

procedure TDro.SizeItem(i,x,y,w,h: integer);
var
  LeftPart,iw,ih: integer;
begin
  if (i < 0) or (i > FCount - 1) then Exit;
  with FItems[i] do
    begin
      DesLabel.Left:= x + 2;
      DesLabel.Top:= y;
      DesLabel.Height:= h - 1;
      DesLabel.Width:= Round(w * DroLabelScale);
      LeftPart:= DesLabel.Width + DesLabel.Left + 2;
      if Image <> nil then
        begin
          iw:= DroHomedBitmap.Width;
          ih:= DroHomedBitmap.Height;
          Image.SetBounds(x + LeftPart,(h - ih) div 2,iw,ih);
          LeftPart:= LeftPart + iw + 2;
        end;
      PosLabel.SetBounds(x+LeftPart,y,w-LeftPart-1,h-1);
      px:= x;
      py:= y;
    end;
end;

procedure TDro.Size;
var
  w,h,x,y,i: integer;
begin
  if (FCount < 1) or (not Assigned(FPanel)) then
    Exit;
  if Layout = dlsVertical then
    begin
      h:= FPanel.ClientHeight div FCount;
      w:= FPanel.ClientWidth - 2;
      y:= (FPanel.CLientHeight - (h * FCount)) div 2;
      y:= y + 1;
      if y < 0 then y:= 0;
      for i:= 0 to FCount - 1 do
        begin
          SizeItem(i,0,y,w,h);
          y:= y + h + 1;
        end;
      FBox.Left:= 0;
      FBox.Width:= FPanel.ClientWidth;
      FBox.Height:= h-2;
    end
  else
    begin
      h:= FPanel.ClientHeight - 2;
      w:= FPanel.ClientWidth div FCount;
      x:= (FPanel.ClientWidth - (w * FCount)) div 2;
      x:= x + 1;
      if x < 0 then x:= 0;
      for i:= 0 to FCount - 1 do
        begin
          SizeItem(i,x,0,w,h);
          x:= x + w + 1;
        end;
      FBox.Width:= w - 2;
      FBox.Height:= FPanel.ClientHeight;
    end;
end;

procedure TDro.Update;
var
  i: integer;
  Axis: TAxis;
  //OldHomed: Boolean;
begin
  for i:= 0 to FCount - 1 do
    with FItems[i] do
      begin
        if Axis = nil then Exit;
        // OldHomed:= Axis.Homed;
        Axis.Update(FRelative,FDtg,FJointMode);
        if Position <> Axis.Position then
          begin
            Position:= Axis.Position;
            PosLabel.Caption:= PosToString(Position*Scale);
          end;
        if OldHomed <> Axis.Homed then
          begin
            if Axis.Homed then
              DesLabel.Font.Color:= clGreen
            else
              DesLabel.Font.Color:= clRed;
            OldHomed:= Axis.Homed;
          end;
    end;

  if FShowBox then
    if FActiveAxis <> Vars.ActiveAxis then
      begin
        FActiveAxis:= Vars.ActiveAxis;
        for i:= 0 to FCount - 1 do
          begin
            Axis:= FItems[i].Axis;
            if Axis = nil then Exit;
            if Axis.AxisNumber = FActiveAxis then
              begin
                FBox.Left:= FItems[i].px;
                FBox.Top:= FItems[i].py;
                SetHalJogAxis(Axis.AxisChar);
              end;
          end;
      end;
end;

constructor TJoints.Create(APanel: TPanel);
begin
  FNumAxes:= 0;
  FMap:= '';
  Dro:= TDro.Create(APanel);
end;

destructor TJoints.Destroy;
var
  i: integer;
begin
  Application.OnIdle:= nil;
  if Assigned(Dro) then
    Dro.Free;
  if FNumAxes > 0 then
    for i:= 0 to FNumAxes - 1 do
      if Assigned(FAxes[i]) then
        FAxes[i].Free;
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
  i:= GetAxis(Ax);
  if i < 0 then Exit;
  sendHome(FAxes[i].AxisNumber);
end;

function TJoints.JogIncr(Ax: Char; Speed,Incr: Double): integer;
var
  d: Dword;
  i: integer;
begin
  Result:= -1;
  i:= GetAxis(Ax);
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
  i:= GetAxis(Ax);
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
  i:= GetAxis(Ax);
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

function TJoints.AddAxis(Des: Char): TAxis;
var
  A: TAxis;
begin
  Result:= nil;
  if FNumAxes < MAX_JOINTS - 1 then
    begin
      A:= TAxis.Create(Des);
      if not Assigned(A) then Exit;
      FAxes[FNumAxes]:= A;
      Inc(FNumAxes);
    end;
  Result:= A;
end;

function TJoints.GetAxis(c: Char): integer;
begin
  Result:= Pos(c,FMap) - 1;
end;

procedure TJoints.SetActiveAxis(Ch: Char);
var
  i: integer;
begin
  i:= Pos(Ch,Vars.CoordMap) - 1;
  if i < 0 then Exit;
  Vars.ActiveAxis:= i;
end;

procedure TJoints.CreateJoints;
var
  i: integer;
  c: Char;
  Ax: TAxis;
begin
  FMap:= '';
  for i:= 1 to Length(Vars.CoordMap) do
    if Vars.CoordMap[i] <> #32 then
      begin
        c:= Vars.CoordMap[i];
        Ax:= AddAxis(c);
        if Ax <> nil then
          begin
            FMap:= FMap + c;
            Ax.FLinear:= Vars.Axis[i-1].IsLinear;
            if (Vars.IsLathe) and (c = 'X') then
              begin
                Dro.Add(Ax,2,'D');
                Dro.Add(Ax,1,'R');
              end
            else
              Dro.Add(Ax,1,'');
          end;
        if Verbose > 0 then writeln('Created joint: ' + c);
      end;
  Application.OnIdle:= @Self.OnIdle;
end;

procedure TJoints.Update;
var
  i: integer;
begin
  if FNumAxes < 1 then Exit;
  Dro.Update;
end;

end.
end.
