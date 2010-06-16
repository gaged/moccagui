unit mocslider;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, LCLType, LCLProc, LCLIntf,
  GraphType, Graphics, ActnList, Controls, LMessages, Forms,
  Menus;

type
  TOnPosChangedEvent = procedure(Sender: TObject;
    NewPos: integer) of object;

type
  TSlider = class(TCustomControl)
  private
    FMin: integer;
    FMax: integer;
    FPos: integer;
    FDown: Boolean;
    FBarColor: TColor;
    FVertical: Boolean;
    FSmooth: Boolean;
    FTextStyle: TTextStyle;
    FCaption: string;
    FOnPosChanged: TOnPosChangedEvent;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure SetPosition(Value: integer);
    procedure SetCaption(Value: string);
    procedure SetVertical(Value: Boolean);
    procedure UpdatePosition(X,Y: integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetParams(AMin,AMax,APos: integer);
  published
    property Position: integer read FPos write SetPosition;
    property BarColor: TColor read FBarColor write FBarColor;
    property Vertical: Boolean read FVertical write SetVertical;
    property Smooth: Boolean read FSmooth write FSmooth;
    property PositionChanged: TOnPosChangedEvent read FOnPosChanged write FOnPosChanged;
    property Caption: string read FCaption write SetCaption;
  end;

procedure Register;

implementation

constructor TSlider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csCaptureMouse];
  Color:= clBtnFace;
  BarColor:= clYellow;
  FVertical:= False;
  FTextStyle.Alignment:= taCenter;
  FTextStyle.Layout:= tlCenter;
  FTextStyle.Opaque:= True;
  SetParams(-100,1000,0);
end;

procedure TSlider.SetPosition(Value: integer);
var
  i: integer;
begin
  i:= Value;
  if i < FMin then i:= FMin;
  if i > FMax then i:= FMax;
  if i <> FPos then
    begin
      FPos:= i;
      if HandleAllocated then
        Invalidate;
    end;
end;

procedure TSlider.SetCaption(Value: string);
begin
  if FCaption <> Value then
    begin
      FCaption:= Value;
      if HandleAllocated then
        Invalidate;
    end;
end;

procedure TSlider.UpdatePosition(X,Y: integer);
var
  i,h: integer;
  NewPos: integer;
begin
  if FVertical then
    begin
      h:= ClientHeight;
      i:= h - Y;
    end
  else
    begin
      h:= ClientWidth;
      i:= X;
    end;
  if h < 1 then ;
  NewPos:= Round((i / h) * (FMax - FMin) + FMin);
  SetPosition(NewPos);
  if Assigned(FOnPosChanged) then
    if FSmooth then FOnPosChanged(Self,FPos);
end;

procedure TSlider.SetParams(AMin,AMax,APos: integer);
begin
  FMin:= AMin;
  FMax:= AMax;
  Enabled:= (FMin < FMax);
  if Enabled then
    SetPosition(APos);
end;

procedure TSlider.SetVertical(Value: Boolean);
begin
  if FVertical <> Value then
    begin
      FVertical:= Value;
      if HandleAllocated then
        Invalidate;
    end;
end;

procedure TSlider.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Enabled then Exit;
  FDown:= True;
  UpdatePosition(X,Y);
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if FDown then
    UpdatePosition(X,Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TSlider.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FDown:= False;
  if not FSmooth then
    if Assigned(FOnPosChanged) then
      begin
        FOnPosChanged(Self,FPos);
        Invalidate;
      end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TSlider.Paint;
var
  R: TRect;
  i: integer;
  v: integer;
begin
  R := GetClientRect;
  Canvas.Frame3d(R,2,bvLowered);
  Canvas.Brush.Color:= Color;
  if Enabled then
    begin
      if FVertical then
        begin
          v:= R.Bottom - R.Top;
          i:= v - Round(v * ((FPos - FMin) / (FMax - FMin)));
          Canvas.FillRect(R.Left,R.Top,R.Right,i);
          Canvas.Brush.Color:= BarColor;
          Canvas.FillRect(R.Left,i,R.Right,R.Bottom);
        end
      else
        begin
          v:= R.Right - R.Left;
          i:= Round(v * ((FPos - FMin) / (FMax - FMin)));
          if i < 3 then i:= 3;
          Canvas.FillRect(i,R.Top,R.Right,R.Bottom);
          Canvas.Brush.Color:= BarColor;
          Canvas.FillRect(R.Left,R.Top,i,R.Bottom);
        end;
      if FCaption <> '' then
        begin
          //SetBkMode(Canvas.Handle,TRANSPARENT);
          Canvas.Brush.Color:= Color;
          Canvas.TextStyle:= FTextStyle;
          Canvas.TextRect(R,0,0,FCaption);
        end;
    end
  else
    Canvas.FillRect(R);
  inherited Paint;
end;

procedure TSlider.CMEnabledChanged(var Message: TLMEssage);
begin
  if HandleAllocated then
    Invalidate;
end;

procedure Register;
begin
  RegisterComponents('Mocca', [TSlider]);
end;

initialization

end.
