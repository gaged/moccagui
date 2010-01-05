unit glcontext;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, LCLType, LCLIntf, LResources,
  Graphics, LMessages, WSLCLClasses, GlxContext;

type
  TGLControl = class(TWinControl)
  private
    FOnPaint: TNotifyEvent;
  protected
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    Procedure Paint; virtual;
    procedure RealizeBounds; override;
    procedure DoOnPaint; virtual;
    procedure SwapBuffers; virtual;
    function MakeCurrent: boolean; virtual;
    procedure Invalidate; override;
    procedure EraseBackground(DC: HDC); override;
  public
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Enabled;
    property OnChangeBounds;
    property OnClick;
    property OnConstrainedResize;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;

  { TWSOpenGLControl }

  TWSGLControl = class(TWidgetSetWSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
                                const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

implementation

procedure TGLControl.WMPaint(var Message: TLMPaint);
begin
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  Paint;
  Exclude(FControlState, csCustomPaint);
end;

procedure TGLControl.WMSize(var Message: TLMSize);
begin
  //if (Message.SizeType and Size_SourceIsInterface)>0 then
  DoOnResize;
end;

procedure TGLControl.EraseBackground(DC: HDC);
begin
  if DC=0 then ;
end;

constructor TGLControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:=ControlStyle-[csSetCaption];
  FCompStyle:=csNonLCL;
  SetInitialBounds(0, 0, 160, 90);
end;

destructor TGLControl.Destroy;
begin
  inherited Destroy;
end;

procedure TGLControl.Paint;
begin
  if IsVisible and HandleAllocated then begin
    if ([csDesigning,csDestroying]*ComponentState=[]) then begin
      if not MakeCurrent then exit
    end;
    DoOnPaint;
  end;
end;

procedure TGLControl.RealizeBounds;
begin
  inherited RealizeBounds;
end;

procedure TGLControl.DoOnPaint;
begin
  if Assigned(OnPaint) then OnPaint(Self);
end;

procedure TGLControl.SwapBuffers;
begin
  LOpenGLSwapBuffers(Handle);
end;

function TGLControl.MakeCurrent: boolean;
begin
  Result:=LOpenGLMakeCurrent(Handle);
end;

procedure TGLControl.Invalidate;
begin
  if csCustomPaint in FControlState then exit;
  inherited Invalidate;
end;

{ TWSGLControl }

class function TWSGLControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  GlControl: TGLControl;
begin
  GlControl:=AWinControl as TGLControl;
  Result:=LOpenGLCreateContext(GlControl,WSPrivate,True,True,AParams);
end;

class procedure TWSGLControl.DestroyHandle(const AWinControl: TWinControl);
begin
  LOpenGLDestroyContextInfo(AWinControl);
  inherited DestroyHandle(AWinControl);
end;

initialization
  RegisterWSComponent(TGLControl,TWSGLControl);
end.

