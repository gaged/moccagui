unit mocbtn;

{$mode objfpc}{$H+}


interface

uses
  Types, Classes, SysUtils, Math, LCLType, LCLProc, LCLIntf, LCLStrConsts,
  GraphType, Graphics, ImgList, ActnList, Controls, StdCtrls, LMessages, Forms,
  Themes, Buttons, Menus, LResources, ImageListCache;

type
  TCustomMocButton = class(TGraphicControl)
  private
    FGlyph: TButtonGlyph;
    FLastDrawDetails: TThemedElementDetails;
    FLayout: TButtonLayout;
    FMargin: integer;
    FSpacing: integer;
    FShowCaption: boolean;
    FDown: Boolean;
    FShowClicks: Boolean;
    function GetGlyph: TBitmap;
    procedure SetShowCaption(const AValue: boolean);
    function  GetTransparent: Boolean;
    procedure SetGlyph(Value: TBitmap);
    procedure SetLayout(const Value: TButtonLayout);
    procedure SetTransparent(const AValue: boolean);
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
  private
    procedure DoBeforeMouseMessage;
    procedure DoMouseUp(var Message: TLMMouse; Button: TMouseButton);
    procedure MouseLeave; override;
    procedure WMLButtonDown(Var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TLMLButtonUp); message LM_LBUTTONUP;
  protected
    FState: TButtonState;
    class procedure WSRegisterClass; override;
    function GetNumGlyphs: Integer;
    procedure GlyphChanged(Sender: TObject);
    procedure Paint; override;
    procedure PaintBackground(var PaintRect: TRect); virtual;
    procedure SetDown(Value: Boolean);
    procedure SetMargin(const Value: integer);
    procedure SetNumGlyphs(Value: integer);
    procedure SetSpacing(const Value: integer);
    procedure RealSetText(const Value: TCaption); override;
    procedure UpdateState(InvalidateOnChange: boolean); virtual;
    function GetDrawDetails: TThemedElementDetails; virtual;
    class function GetControlClassDefaultSize: TPoint; override;
  protected
    function GetGlyphSize(PaintRect: TRect): TSize; virtual;
    function GetTextSize(PaintRect: TRect): TSize; virtual;
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect; const AOffset: TPoint;
      AState: TButtonState; ATransparent: Boolean; BiDiFlags: Longint): TRect; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadGlyphFromLazarusResource(const AName: String);
  public
    property Down: Boolean read FDown write SetDown default false;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property Margin: integer read FMargin write SetMargin default -1;
    property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs default 1;
    property ShowCaption: boolean read FShowCaption write SetShowCaption default false;
    property ShowClicks: boolean read FShowClicks write FShowClicks default false;
    property Spacing: integer read FSpacing write SetSpacing default 4;
    property Transparent: Boolean read GetTransparent write SetTransparent default true;
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
    property Glyph;
    property Layout;
    property Margin;
    property NumGlyphs;
    property Spacing;
    property Transparent;
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
    property ShowCaption;
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
  FGlyph:= TButtonGlyph.Create;
  FGlyph.IsDesigning:= csDesigning in ComponentState;
  FGlyph.ShowMode:= gsmAlways;
  // FGlyph.TransparentMode:= gtmTransparent;
  FGlyph.OnChange:= @GlyphChanged;
  SetInitialBounds(0,0,GetControlClassDefaultSize.X,GetControlClassDefaultSize.Y);
  ControlStyle := ControlStyle + [csCaptureMouse]-[csSetCaption, csClickEvents, csOpaque];
  FLayout:= blGlyphLeft;
  FSpacing:= 4;
  FMargin:= -1;
  Color:= clBtnFace;
  FShowCaption:= true;
end;

destructor TCustomMocButton.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited Destroy;
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

procedure TCustomMocButton.SetGlyph(Value : TBitmap);
begin
  FGlyph.Glyph:= Value;
  Invalidate;
end;

procedure TCustomMocButton.SetMargin(const Value : Integer);
begin
  if FMargin <> Value then begin
    FMargin := Value;
    Invalidate;
  end;
end;

procedure TCustomMocButton.SetNumGlyphs(Value : integer);
Begin
  if Value < 0 then Value := 1;
  if Value > High(TNumGlyphs) then Value:= High(TNumGlyphs);
  if Value <> TButtonGlyph(fGlyph).NumGlyphs then
  Begin
    TButtonGlyph(fGlyph).NumGlyphs := TNumGlyphs(Value);
    Invalidate;
  end;
end;

procedure TCustomMocButton.SetSpacing(const Value : Integer);
begin
  if FSpacing <> Value then begin
    FSpacing := Value;
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
    if ((FState <> OldState) or not ThemedElementDetailsEqual(FLastDrawDetails, GetDrawDetails))
      then
        Invalidate;
end;

function TCustomMocButton.GetDrawDetails: TThemedElementDetails;

  function ButtonPart: TThemedButton;
  begin
    Result:= tbPushButtonNormal;
    if not Enabled then
      Result := tbPushButtonDisabled
    else
    if FState = bsDown then
      Result := tbPushButtonPressed
    else
    if FState = bsHot then
      Result := tbPushButtonHot
    else
      Result := tbPushButtonNormal
  end;

begin
  Result:= ThemeServices.GetElementDetails(ButtonPart)
end;

class function TCustomMocButton.GetControlClassDefaultSize: TPoint;
begin
  Result.X:=23;
  Result.Y:=22;
end;

function TCustomMocButton.GetGlyph : TBitmap;
begin
  Result:= FGlyph.Glyph;
end;

procedure TCustomMocButton.SetShowCaption(const AValue: boolean);
begin
  if FShowCaption = AValue then exit;
  FShowCaption:= AValue;
  invalidate;
end;

function TCustomMocButton.GetNumGlyphs : Integer;
begin
  Result:= TButtonGlyph(fGlyph).NumGlyphs;
end;

procedure TCustomMocButton.GlyphChanged(Sender : TObject);
begin
  Invalidate;
end;

procedure TCustomMocButton.Paint;
var
  PaintRect: TRect;
  GlyphWidth, GlyphHeight: Integer;
  Offset, OffsetCap: TPoint;
  ClientSize, TotalSize, TextSize, GlyphSize: TSize;
  M, S : integer;
begin
  UpdateState(false);
  if FGlyph = nil then exit;

  PaintRect:=ClientRect;
  FLastDrawDetails := GetDrawDetails;
  
  PaintBackground(PaintRect);

  GlyphSize := GetGlyphSize(PaintRect);
  GlyphWidth := GlyphSize.CX;
  if TButtonGlyph(FGlyph).NumGlyphs > 1 then
    GlyphWidth:=GlyphWidth div NumGlyphs;
  GlyphHeight := GlyphSize.CY;

  ClientSize.cx:= PaintRect.Right - PaintRect.Left;
  ClientSize.cy:= PaintRect.Bottom - PaintRect.Top;

  TextSize := GetTextSize(PaintRect);

  if (GlyphWidth = 0) or (GlyphHeight = 0)
  or (TextSize.cx = 0) or (TextSize.cy = 0)
  then
    S:= 0
  else
    S:= Spacing;

  if Margin = -1 then begin
    if S = -1 then begin
      TotalSize.cx:= TextSize.cx + GlyphWidth;
      TotalSize.cy:= TextSize.cy + GlyphHeight;
      if Layout in [blGlyphLeft, blGlyphRight] then
        M:= (ClientSize.cx - TotalSize.cx) div 3
      else
        M:= (ClientSize.cy - TotalSize.cy) div 3;
      S:= M;
    end else begin
      TotalSize.cx:= GlyphWidth + S + TextSize.cx;
      TotalSize.cy:= GlyphHeight + S + TextSize.cy;
      if Layout in [blGlyphLeft, blGlyphRight] then
        M:= (ClientSize.cx - TotalSize.cx) div 2
      else
        M:= (ClientSize.cy - TotalSize.cy) div 2;
    end;
  end else begin
    if S = -1 then begin
      TotalSize.cx:= ClientSize.cx - (Margin + GlyphWidth);
      TotalSize.cy:= ClientSize.cy - (Margin + GlyphHeight);
      if Layout in [blGlyphLeft, blGlyphRight] then
        S:= (TotalSize.cx - TextSize.cx) div 2
      else
        S:= (TotalSize.cy - TextSize.cy) div 2;
    end;
    M:= Margin;
  end;

  case Layout of
    blGlyphLeft : begin
      Offset.X:= M;
      Offset.Y:= (ClientSize.cy - GlyphHeight) div 2;
      OffsetCap.X:= Offset.X + GlyphWidth + S;
      OffsetCap.Y:= (ClientSize.cy - TextSize.cy) div 2;
    end;
    blGlyphRight : begin
      Offset.X:= ClientSize.cx - M - GlyphWidth;
      Offset.Y:= (ClientSize.cy - GlyphHeight) div 2;
      OffsetCap.X:= Offset.X - S - TextSize.cx;
      OffsetCap.Y:= (ClientSize.cy - TextSize.cy) div 2;
    end;
    blGlyphTop : begin
      Offset.X:= (ClientSize.cx - GlyphWidth) div 2;
      Offset.Y:= M;
      OffsetCap.X:= (ClientSize.cx - TextSize.cx) div 2;
      OffsetCap.Y:= Offset.Y + GlyphHeight + S;
    end;
    blGlyphBottom : begin
      Offset.X:= (ClientSize.cx - GlyphWidth) div 2;
      Offset.Y:= ClientSize.cy - M - GlyphHeight;
      OffsetCap.X:= (ClientSize.cx - TextSize.cx) div 2;
      OffsetCap.Y:= Offset.Y - S - TextSize.cy;
    end;
  end;

  DrawGlyph(Canvas, PaintRect, Offset, FState, Transparent, 0);
  if FShowCaption and (Caption <> '') then
  begin
    with PaintRect, OffsetCap do
    begin
      Left := Left + X;
      Top := Top + Y;
    end;

    ThemeServices.DrawText(Canvas, FLastDrawDetails, Caption, PaintRect,
      DT_LEFT or DT_TOP or DT_WordBreak, 0);
  end;

  inherited Paint;
end;

procedure TCustomMocButton.PaintBackground(var PaintRect: TRect);
begin
  if not Transparent and ThemeServices.HasTransparentParts(FLastDrawDetails) then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(PaintRect);
  end;
  ThemeServices.DrawElement(Canvas.Handle, FLastDrawDetails, PaintRect);
  PaintRect := ThemeServices.ContentRect(Canvas.Handle, FLastDrawDetails, PaintRect);
end;

procedure TCustomMocButton.DoBeforeMouseMessage;
begin
  if Application <> nil then
    Application.DoBeforeMouseMessage(Self);
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
  DoBeforeMouseMessage;
  if (csCaptureMouse in ControlStyle) and (mbLeft in CaptureMouseButtons) then
    MouseCapture:= False;
  DoMouseUp(Message, mbLeft);
  // FState:= bsUp;
  if csClicked in ControlState then
  begin
    Exclude(FControlState,csClicked);
    if PtInRect(ClientRect, SmallPointToPoint(Message.Pos)) then
      Click;
  end;
  invalidate;
end;

procedure TCustomMocButton.SetLayout(const Value : TButtonLayout);
begin
  if Value <> FLayout then begin
    FLayout:= Value;
    Invalidate;
  end;
end;

procedure TCustomMocButton.SetTransparent(const AValue: boolean);
const
  MODE: array[Boolean] of TGlyphTransparencyMode = (gtmOpaque, gtmTransparent);
begin
  if AValue = Transparent then Exit;

  if AValue then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
  // FGlyph.SetTransparentMode(MODE[AValue]);
  Invalidate;
end;

procedure TCustomMocButton.LoadGlyphFromLazarusResource(const AName: String);
begin
  Buttons.LoadGlyphFromLazarusResource(FGlyph, AName);
end;

function TCustomMocButton.GetGlyphSize(PaintRect: TRect): TSize;
begin
  Result.CX := FGlyph.Glyph.Width;
  Result.CY := FGlyph.Glyph.Height;
end;

function TCustomMocButton.GetTextSize(PaintRect: TRect): TSize;
var
  TMP: String;
  TXTStyle: TTextStyle;
  Flags: Cardinal;
begin
  if FShowCaption and (Caption <> '') then
  begin
    TMP := Caption;
    TXTStyle:= Canvas.TextStyle;
    TXTStyle.Opaque := False;
    TXTStyle.Clipping := True;
    //TXTStyle.ShowPrefix := ShowAccelChar;
    TXTStyle.Alignment := taLeftJustify;
    TXTStyle.Layout := tlTop;
    TXTStyle.SystemFont := Canvas.Font.IsDefault;//Match System Default Style
    //DeleteAmpersands(TMP);

    Flags := DT_CalcRect;
    // if not TXTStyle.SingleLine then
    // Inc(Flags, DT_WordBreak);

    DrawText(Canvas.Handle, PChar(TMP), Length(TMP), PaintRect, Flags);
    Result.CY := PaintRect.Bottom - PaintRect.Top;
    Result.CX := PaintRect.Right - PaintRect.Left;
  end
  else
  begin
    Result.CY:= 0;
    Result.CX:= 0;
  end;
end;

function TCustomMocButton.GetTransparent: Boolean;
begin
  //if FGlyph.TransparentMode = gtmGlyph
  //then Result := FGlyph.FOriginal.Transparent
  //else Result := FGlyph.TransparentMode = gtmTransparent;
end;

function TCustomMocButton.DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
  const AOffset: TPoint; AState: TButtonState; ATransparent: Boolean;
  BiDiFlags: Longint): TRect;
begin
  if Assigned(FGlyph) then
  begin
    if (AState = bsDown) or (Down = true) then
      Result := FGlyph.Draw(ACanvas, AClient, point(AOffset.x + 1, AOffset.y + 1), AState, ATransparent, BiDiFlags)
    else
      Result := FGlyph.Draw(ACanvas, AClient, AOffset, AState, ATransparent, BiDiFlags);
  end;
end;

procedure TCustomMocButton.CMEnabledChanged(var Message: TLMEssage);
Begin
  //Should create a new glyph based on the new state
  UpdateState(true);
end;


{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}


initialization

end.
