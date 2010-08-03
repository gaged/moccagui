unit mocgl; 

{$mode objfpc}{$H+}
{$LinkLib GL}
{$PACKRECORDS C}

interface

uses
  Classes, ExtCtrls, mocbtn, SysUtils, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs,
  LCLProc, LCLType, LCLIntf,
  LMessages, WSLCLClasses, Gtk2WSControls,
  gl, glu, InterfaceBase, GtkDef, gdk2x, glib2, gtk2, Gtk2Int,
  GdkGLExt, GtkGLExt;

type
  TMocGLControl = class(TWinControl)
  private
    FOnPaint: TNotifyEvent;
    // FWidget: PGtkWidget;
  protected
    procedure WMPaint(var Message: TLMPaint); message LM_PAINT;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
  public
    constructor Create(TheOwner: TComponent); override;
    Procedure Paint; virtual;
    procedure RealizeBounds; override;
    procedure DoOnPaint; virtual;
    procedure SwapBuffers; virtual;
    function MakeCurrent: boolean; virtual;
    procedure Invalidate; override;
    procedure EraseBackground(DC: HDC); override;
  public
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property Enabled;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
  end;

type
  TWidgetSetWSWinControl = TGtk2WSWinControl;

  { TWSOpenGLControl }
  TWSGLControl = class(TWidgetSetWSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
                                const AParams: TCreateParams): HWND; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;

implementation

procedure TMocGLControl.WMPaint(var Message: TLMPaint);
begin
  Include(FControlState, csCustomPaint);
  inherited WMPaint(Message);
  Paint;
  Exclude(FControlState, csCustomPaint);
end;

procedure TMocGLControl.WMSize(var Message: TLMSize);
begin
  DoOnResize;
end;

procedure TMocGLControl.EraseBackground(DC: HDC);
begin
  if DC = 0 then ;
end;

constructor TMocGLControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle:= ControlStyle-[csSetCaption];
  FCompStyle:= csNonLCL;
  SetInitialBounds(0, 0, 160, 90);
end;

procedure TMocGLControl.Paint;
begin
  if IsVisible and HandleAllocated then begin
    if ([csDesigning,csDestroying]*ComponentState=[]) then begin
      if not MakeCurrent then exit
    end;
    DoOnPaint;
  end;
end;

procedure TMocGLControl.RealizeBounds;
begin
  inherited RealizeBounds;
end;

procedure TMocGLControl.DoOnPaint;
begin
  if Assigned(OnPaint) then OnPaint(Self);
end;

procedure TMocGLControl.SwapBuffers;
var
  gldrawable: PGdkGLDrawable;
  Widget: PGtkWidget;
begin
  Widget:= PGtkWidget(PtrUInt(Handle));
  gldrawable:= gtk_widget_get_gl_drawable(widget);
  if gdk_gl_drawable_is_double_buffered (gldrawable) then
    gdk_gl_drawable_swap_buffers (gldrawable) else
    glFlush ();
end;

function TMocGLControl.MakeCurrent: boolean;
var
  glcontext: PGdkGLContext;
  gldrawable: PGdkGLDrawable;
  Widget: PGtkWidget;
begin
  Result:= False;
  if Handle = 0 then
    RaiseGDBException('Make Current: Handle = 0');
  Widget:= PGtkWidget(PtrUInt(Handle));
  glcontext:= gtk_widget_get_gl_context(widget);
  gldrawable:= gtk_widget_get_gl_drawable(widget);
  gtk_widget_realize(Widget);
  if not GTK_WIDGET_REALIZED(Widget) then exit;
  Result:= gdk_gl_drawable_make_current(gldrawable,glcontext);
end;

procedure TMocGLControl.Invalidate;
begin
  if csCustomPaint in FControlState then exit;
  inherited Invalidate;
end;

function CreateGtk2Context: PGtkWidget;
var
  glconfig: PGdkGLConfig;
  drawing_area: PGtkWidget;
begin
  Result:= nil;
  glconfig:= gdk_gl_config_new_by_mode(GDK_GL_MODE_RGB or
    GDK_GL_MODE_DEPTH or GDK_GL_MODE_DOUBLE);
  if glconfig = nil then
  begin
    writeln('Cannot find the double-buffered visual.');
    writeln('Trying single-buffered visual.');
    glconfig := gdk_gl_config_new_by_mode(GDK_GL_MODE_RGB or
      GDK_GL_MODE_DEPTH);
    if glconfig = nil then
    begin
      writeln('No appropriate OpenGL-capable visual found.');
      Exit;
    end;
  end;
  drawing_area:= gtk_drawing_area_new;
  gtk_widget_set_gl_capability(drawing_area,glconfig,
    NULL,TRUE,GDK_GL_RGBA_TYPE);
  Result:= drawing_area;
end;

class function TWSGLControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  NewWidget: PGtkWidget;
begin
  if WSPrivate= nil then ;
  NewWidget:= CreateGtk2Context;
  if NewWidget = nil then
    raise Exception.Create('createcontext failed, Widget = nil!');
  Result:= HWND(PtrUInt(Pointer(NewWidget)));
  PGtkobject(NewWidget)^.flags:=PGtkobject(NewWidget)^.flags or GTK_CAN_FOCUS;
  TGTK2WidgetSet(WidgetSet).FinishCreateHandle(AWinControl,NewWidget,AParams);
    //g_signal_connect_after(PGtkObject(NewWidget), 'size-allocate',
    //  TGTKSignalFunc(@gtkglarea_size_allocateCB), AWinControl);
end;

class procedure TWSGLControl.DestroyHandle(const AWinControl: TWinControl);
begin
  // LOpenGLDestroyContextInfo(AWinControl);
  inherited DestroyHandle(AWinControl);
end;

initialization
  RegisterWSComponent(TMocGLControl,TWSGLControl);
end.


