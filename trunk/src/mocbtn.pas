unit mocbtn;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, LCLType, LCLProc, LCLIntf,
  GraphType, Graphics, ActnList, Controls, LMessages, Forms,
  Themes, Buttons, Menus, LResources;

type
  TCustomMocButton = class(TGraphicControl)
  private
    FDetails: TThemedElementDetails;
    FDown: Boolean;
    FShowClicks: Boolean;
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
  private
    procedure DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
    procedure MouseLeave; override;
    procedure WMLButtonDown(Var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TLMLButtonUp); message LM_LBUTTONUP;
  protected
    FState: TButtonState;
    class procedure WSRegisterClass; override;
    procedure Paint; override;
    procedure PaintBackground(var PaintRect: TRect); virtual;
    procedure SetDown(Value: Boolean);
    procedure RealSetText(const Value: TCaption); override;
    procedure UpdateState(InvalidateOnChange: boolean); virtual;
    function GetDrawDetails: TThemedElementDetails; virtual;
    class function GetControlClassDefaultSize: TPoint; override;
  public
    constructor Create(AOwner: TComponent); override;
  public
    property Down: Boolean read FDown write SetDown default false;
    property ShowClicks: boolean read FShowClicks write FShowClicks default false;
  end;

  { TSpeedButton }

  TMocButton = class(TCustomMocButton)
  published
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Color;
    property Down;
    property Enabled;
    property Font;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnChangeBounds;
    property ShowClicks;
    property ShowHint;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
  end;

implementation

uses
  WSButtons;

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

constructor TCustomMocButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetInitialBounds(0,0,GetControlClassDefaultSize.X,GetControlClassDefaultSize.Y);
  ControlStyle := ControlStyle + [csCaptureMouse]-[csSetCaption, csClickEvents, csOpaque];
  Color:= clBtnFace;
  Canvas.TextStyle.Wordbreak:= True;
  Canvas.TextStyle.Alignment:= taCenter;
  Canvas.TextStyle.Layout:= tlCenter;
  Canvas.TextStyle.SingleLine:= False;
end;

procedure TCustomMocButton.SetDown(Value : Boolean);
var
  OldState: TButtonState;
begin
  if FDown <> Value then
    begin
      FDown:= Value;
      OldState:= FState;
      if FDown then
        FState:= bsDown
      else
        FState:= bsUp;
      if (OldState <> FState) then
        Invalidate;
    end;
end;

procedure TCustomMocButton.RealSetText(const Value: TCaption);
begin
  if Caption = Value then Exit;
  inherited RealSetText(Value);
  Invalidate;
end;

procedure TCustomMocButton.UpdateState(InvalidateOnChange: boolean);
var
  OldState: TButtonState;
begin
  OldState:= FState;
  if not Enabled then
    begin
      FState:= bsDisabled;
    end
  else
    begin
      if FState = bsDisabled then
        FState:= bsUp
      else
        if (FDown) or ((csClicked in FControlState) and FShowClicks) then
          FState:= bsDown
        else
          FState:= bsUp;
    end;
  if InvalidateOnChange then
    if ((FState <> OldState) or not
      ThemedElementDetailsEqual(FDetails, GetDrawDetails))
    then
      Invalidate;
end;

function TCustomMocButton.GetDrawDetails: TThemedElementDetails;
var
  Part: TThemedButton;
begin
  if not Enabled then
    Part:= tbPushButtonDisabled
  else
  if FState = bsDown then
    Part := tbPushButtonPressed
  else
  if FState = bsHot then
    Part := tbPushButtonHot
  else
    Part:= tbPushButtonNormal;
  Result:= ThemeServices.GetElementDetails(Part)
end;

class function TCustomMocButton.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=23;
  Result.Y:=22;
end;

procedure TCustomMocButton.Paint;
var
  R: TRect;
  OldColor: TColor;
begin
  UpdateState(False);
  R:= ClientRect;
  FDetails:= GetDrawDetails;
  PaintBackground(R);
  if Caption <> '' then
    begin
      OldColor := Canvas.Font.Color;
      if ThemeServices.IsDisabled(FDetails) then
        begin
          Canvas.Font.Color := clBtnHighlight;
          OffsetRect(R, 1, 1);
          Canvas.TextRect(R,R.Left,R.Top,Caption);
          Canvas.Font.Color := clBtnShadow;
          OffsetRect(R, -1, -1);
        end
      else
        if ThemeServices.IsPushed(FDetails) then
          begin
            Inc(R.Left, 1);
            Inc(R.Top, 1);
          end;
      Canvas.TextRect(R, R.Left, R.Top, Caption);
      Canvas.Font.Color := OldColor;
    end;
  inherited Paint;
end;

procedure TCustomMocButton.PaintBackground(var PaintRect: TRect);
begin
  ThemeServices.DrawElement(Canvas.Handle, FDetails, PaintRect);
  PaintRect:= ThemeServices.ContentRect(Canvas.Handle, FDetails, PaintRect);
end;

procedure TCustomMocButton.DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
begin
  if not (csNoStdEvents in ControlStyle)
  then with Message do
    MouseUp(Button, KeysToShiftState(Keys), XPos, YPos);
end;

procedure TCustomMocButton.MouseLeave;
begin
  if Enabled then
    if csClicked in ControlState then
      begin
        Exclude(FControlState,csClicked);
        if FShowClicks then
          begin
            FState:= bsUp;
            UpdateState(true);
          end;
    end;
  inherited MouseLeave;
end;

procedure TCustomMocButton.WMLButtonDown(var Message: TLMLButtonDown);
begin
  inherited;
  Include(FControlState,csClicked);
  Invalidate;
end;

class procedure TCustomMocButton.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCustomSpeedButton;
end;

procedure TCustomMocButton.WMLButtonUp(var Message: TLMLButtonUp);
begin
  if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
    MouseCapture:= False;
  DoMouseUp(Message, mbLeft);
  if csClicked in ControlState then
  begin
    Exclude(FControlState,csClicked);
    if PtInRect(ClientRect, SmallPointToPoint(Message.Pos)) then
      Click;
  end;
  invalidate;
end;

procedure TCustomMocButton.CMEnabledChanged(var Message: TLMEssage);
Begin
  UpdateState(True);
end;


{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}


initialization

end.
