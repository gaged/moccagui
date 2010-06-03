unit mocled;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, LCLType, LCLProc, LCLIntf,
  GraphType, Graphics, ActnList, Controls, Forms,
  Themes, Buttons, Menus, LResources, mocbtn;

type
  TMocLed = class(TGraphicControl)
  private
    FLedColor: TColor;
    FIsOn: Boolean;
    procedure SetLedColor(Value: TColor);
  protected
    procedure SetOn(Value: Boolean);
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property LedColor: TColor read FLedColor write SetLedColor;
    property IsOn: Boolean read FIsOn write SetOn;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Color default clGray;
    property Enabled;
    property Visible;
  end;

procedure Register;

implementation

constructor TMocLed.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color:= clGray;
  FLedColor:= clRed;
  FIsOn:= False;
end;

procedure TMocLed.SetLedColor(Value : TColor);
begin
  if FLedColor <> Value then
    begin
      FLedColor:= Value;
      Invalidate;
    end;
end;

procedure TMocLed.SetOn(Value: Boolean);
begin
  if FIsOn <> Value then
    begin
      FIsOn:= Value;
      Invalidate;
    end;
end;

procedure TMocLed.Paint;
var
  R: TRect;
begin
  R:= ClientRect;
  Canvas.Frame(R);
  InflateRect(R,-1,-1);
  if FIsOn then
    Canvas.Brush.Color:= FLedColor
  else
     Canvas.Brush.Color:= Color;
  Canvas.FillRect(R);
  inherited Paint;
end;

procedure Register;
begin
  RegisterComponents('Mocca', [TMocLed]);
end;

initialization

end.
