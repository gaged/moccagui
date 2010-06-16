unit mocbtn;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, LCLType, LCLProc, LCLIntf,
  GraphType, Graphics, ActnList, Controls, LMessages, Forms,
  Themes, Buttons, Menus, LResources;

type
  TMocButton = class(TGraphicControl)
  private
    function  GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    procedure CMEnabledChanged(var Message: TLMessage); message CM_ENABLEDCHANGED;
  private
    procedure MouseLeave; override;
    procedure WMLButtonDown(Var Message: TLMLButtonDown); message LM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TLMLButtonUp); message LM_LBUTTONUP;
  protected
    // FState: TButtonState;
    FGlyph: TButtonGlyph;
    FDown: Boolean;
    FState: TButtonState;
    FShowClicks: Boolean;
    FTextStyle: TTextStyle;
    FCommand: string;
    procedure GlyphChanged(Sender: TObject);
    procedure Paint; override;
    procedure SetCommand(Value: string);
    procedure SetDown(Value: Boolean);
    procedure RealSetText(const Value: TCaption); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadGlyphFromLazarusResource(const AName: String);
  published
    property Command: string read FCommand write SetCommand;
    property Down: Boolean read FDown write SetDown default false;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property ShowClicks: boolean read FShowClicks write FShowClicks default false;
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Color;
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
    property ShowHint;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
  end;

const
  MocButtonFrameWidth: integer = 2;

procedure Register;

implementation

constructor TMocButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyph:= TButtonGlyph.Create;
  FGlyph.IsDesigning:= csDesigning in ComponentState;
  FGlyph.ShowMode:= gsmAlways;
  FGlyph.OnChange:= @GlyphChanged;
  SetInitialBounds(0,0,24,48);
  ControlStyle:= ControlStyle + [csCaptureMouse] -[csSetCaption, csOpaque];
  Color:= clBtnFace;
  Caption:= '';
  FTextStyle.Alignment:= taCenter;
  FTextStyle.Layout:= tlCenter;
  FShowClicks:= True;
  FState:= bsUp;
end;

destructor TMocButton.Destroy;
begin
  FreeAndNil(FGlyph);
  inherited Destroy;
end;

procedure TMocButton.SetDown(Value: Boolean);
begin
  if FDown <> Value then
    begin
      FDown:= Value;
      Invalidate;
    end;
end;

procedure TMocButton.SetCommand(Value: string);
begin
  if FCommand <> Value then
    begin
      FCommand:= Value;
      Tag:= 0;
    end;
end;

procedure TMocButton.SetGlyph(Value : TBitmap);
begin
  FGlyph.Glyph:= Value;
  Invalidate;
end;

procedure TMocButton.RealSetText(const Value: TCaption);
begin
  if Caption = Value then Exit;
  inherited RealSetText(Value);
  Invalidate;
end;

function TMocButton.GetGlyph : TBitmap;
begin
  Result:= FGlyph.Glyph;
end;

procedure TMocButton.GlyphChanged(Sender : TObject);
begin
  Invalidate;
end;

procedure TMocButton.Paint;
var
  R: TRect;
  cw,ch,gw,gh: Integer;
  Offset: TPoint;
  OldColor: TColor;
begin
  if FGlyph = nil then Exit;
  R:= ClientRect;
  Canvas.Brush.Color:= Color;
  Canvas.FillRect(R);

  if (FState = bsDown) then
    begin
      if FDown then
        Canvas.Frame3d(R,MocButtonFrameWidth,bvRaised)
      else
        Canvas.Frame3d(R,MocButtonFrameWidth,bvLowered)
    end
  else
    begin
      if FDown then
        Canvas.Frame3d(R,MocButtonFrameWidth,bvLowered)
      else
        Canvas.Frame3d(R,MocButtonFrameWidth,bvRaised);
     end;

  cw:= R.Right - R.Left;
  ch:= R.Bottom - R.Top;

  gw:= FGlyph.Glyph.Width;
  gh:= FGlyph.Glyph.Height;

  Offset.X:= (cw - gw) div 2;
  Offset.Y:= 1;

  if gw > 0 then
    if Caption = '' then
      Offset.Y:= (ch - gh) div 2;

  if FDown then
    begin
      Inc(Offset.X);
      Inc(Offset.Y);
    end;

  FGlyph.Draw(Canvas, R, Offset, FState, True, 0);

  if Caption <> '' then
    begin
      OldColor:= Canvas.Font.Color;
      Canvas.TextStyle:= FTextStyle;
      R.Top:= R.Top + gh + Offset.Y;
      if not Enabled then
        begin
          Canvas.Font.Color:= clBtnHighlight;
          OffsetRect(R, 1, 1);
          Canvas.TextRect(R,0,0,Caption);
          Canvas.Font.Color := clBtnShadow;
          OffsetRect(R, -1, -1);
        end;
      if FDown or (FState = bsDown) then
        begin
          Inc(R.Left);
          Inc(R.Top);
        end;
      Canvas.TextRect(R,0,0,Caption);
      Canvas.Font.Color:= OldColor;
    end;
  inherited Paint;
end;

procedure TMocButton.MouseLeave;
begin
  FState:= bsUp;
  Invalidate;
  inherited MouseLeave;
end;

procedure TMocButton.WMLButtonDown(var Message: TLMLButtonDown);
begin
  FState:= bsDown;
  Invalidate;
  inherited;
end;

procedure TMocButton.WMLButtonUp(var Message: TLMLButtonUp);
begin
  FState:= bsUp;
  Invalidate;
  inherited;
end;

procedure TMocButton.LoadGlyphFromLazarusResource(const AName: String);
begin
  Buttons.LoadGlyphFromLazarusResource(FGlyph, AName);
end;

procedure TMocButton.CMEnabledChanged(var Message: TLMEssage);
begin
  Invalidate;
end;

procedure Register;
begin
  RegisterComponents('Mocca', [TMocButton]);
end;

initialization

end.
