unit glxcontext;

{$mode objfpc}{$H+}
{$LinkLib GL}
{$PACKRECORDS C}

interface

uses
  Classes, SysUtils, LCLProc, LCLType, X, XUtil, XLib, gl, InterfaceBase,
  WSLCLClasses,
  {$IFDEF LCLGTK2}
  LMessages, GtkDef, gdk2x, glib2, gdk2, gtk2, Gtk2Int, Gtk2WSControls,
  {$ENDIF}
  {$IFDEF LCLGTK}
  glib, gdk, gtk, GtkInt, GtkWSControls,
  {$ENDIF}
  Controls;

// gdkgl

const
// enum _GDK_GL_CONFIGS
  GDK_GL_NONE                           = 0;
  GDK_GL_USE_GL                         = 1;
  GDK_GL_BUFFER_SIZE                    = 2;
  GDK_GL_LEVEL                          = 3;
  GDK_GL_RGBA                           = 4;
  GDK_GL_DOUBLEBUFFER                   = 5;
  GDK_GL_STEREO                         = 6;
  GDK_GL_AUX_BUFFERS                    = 7;
  GDK_GL_RED_SIZE                       = 8;
  GDK_GL_GREEN_SIZE                     = 9;
  GDK_GL_BLUE_SIZE                      = 10;
  GDK_GL_ALPHA_SIZE                     = 11;
  GDK_GL_DEPTH_SIZE                     = 12;
  GDK_GL_STENCIL_SIZE                   = 13;
  GDK_GL_ACCUM_RED_SIZE                 = 14;
  GDK_GL_ACCUM_GREEN_SIZE               = 15;
  GDK_GL_ACCUM_BLUE_SIZE                = 16;
  GDK_GL_ACCUM_ALPHA_SIZE               = 17;

  // GLX_EXT_visual_info extension
  GDK_GL_X_VISUAL_TYPE_EXT              = $22;
  GDK_GL_TRANSPARENT_TYPE_EXT           = $23;
  GDK_GL_TRANSPARENT_INDEX_VALUE_EXT    = $24;
  GDK_GL_TRANSPARENT_RED_VALUE_EXT      = $25;
  GDK_GL_TRANSPARENT_GREEN_VALUE_EXT    = $26;
  GDK_GL_TRANSPARENT_BLUE_VALUE_EXT     = $27;
  GDK_GL_TRANSPARENT_ALPHA_VALUE_EXT    = $28;


type
  TGdkGLContext = record end;
  PGdkGLContext = ^TGdkGLContext;

// GLX_EXT_visual_info extension

function gdk_gl_query: boolean;
function gdk_gl_choose_visual(attrlist: Plongint): PGdkVisual;
function gdk_gl_get_config(visual: PGdkVisual; attrib: longint): longint;
function gdk_gl_context_new(visual: PGdkVisual; attrlist: plongint): PGdkGLContext;
function gdk_gl_context_attrlist_new(attrlist: Plongint): PGdkGLContext;
function gdk_gl_context_ref(context: PGdkGLContext): PGdkGLContext;
procedure gdk_gl_context_unref(context:PGdkGLContext);
function gdk_gl_make_current(drawable: PGdkDrawable;
                             context: PGdkGLContext): boolean;
procedure gdk_gl_swap_buffers(drawable: PGdkDrawable);
procedure gdk_gl_wait_gdk;
procedure gdk_gl_wait_gl;

{ glpixmap stuff  }

type
  TGdkGLPixmap = record end;
  PGdkGLPixmap = ^TGdkGLPixmap;
  TGLXContext = pointer;

type
  TGtkGlAreaMakeCurrentType = boolean;

  PGtkGLArea = ^TGtkGLArea;
  TGtkGLArea = record
    darea: TGtkDrawingArea;
    glcontext: PGdkGLContext;
  end;

  PGtkGLAreaClass = ^TGtkGLAreaClass;
  TGtkGLAreaClass = record
    parent_class: TGtkDrawingAreaClass;
  end;

function GTK_TYPE_GL_AREA: TGtkType;
function GTK_GL_AREA(obj: Pointer): PGtkGLArea;
function GTK_GL_AREA_CLASS(klass: Pointer): PGtkGLAreaClass;
function GTK_IS_GL_AREA(obj: Pointer): Boolean;
function GTK_IS_GL_AREA_CLASS(klass: Pointer): Boolean;

function gtk_gl_area_get_type: TGtkType;
function gtk_gl_area_new(attrList: Plongint): PGtkWidget;
function gtk_gl_area_make_current(glarea: PGtkGLArea): boolean;
function gtk_gl_area_begingl(glarea: PGtkGLArea): boolean;
procedure gtk_gl_area_swap_buffers(gl_area: PGtkGLArea);

procedure LOpenGLSwapBuffers(Handle: HWND);
function  LOpenGLMakeCurrent(Handle: HWND): boolean;
function  LOpenGLCreateContext(AWinControl: TWinControl;
  WSPrivate: TWSPrivateClass; DoubleBuffered, RGBA, IsDirect: boolean;
  const AParams: TCreateParams): HWND;
procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);
function CreateOpenGLContextAttrList(DoubleBuffered: boolean;
  RGBA: boolean): PInteger;

type
  {$IFDEF LCLGTK}
  TWidgetSetWSWinControl = TGtkWSWinControl;
  {$ENDIF}
  {$IFDEF LCLGTK2}
  TWidgetSetWSWinControl = TGtk2WSWinControl;
  {$ENDIF}

implementation

var
  gl_area_type: TGtkType = 0;
  parent_class: Pointer = nil;
  direct: TBool;

type
  TGdkGLContextPrivate = record
    xdisplay: PDisplay;
    glxcontext: TGLXContext;
    ref_count: gint;
  end;
  PGdkGLContextPrivate = ^TGdkGLContextPrivate;

type
  //PGLXPixmap = ^GLXPixmap;
  GLXPixmap = TXID;

  //PGLXDrawable = ^GLXDrawable;
  GLXDrawable = TXID;

{ GLX 1.0 functions. }

function glXChooseVisual(dpy:PDisplay; screen:longint; attrib_list:Plongint):PXVisualInfo;cdecl;external;
procedure glXCopyContext(dpy:PDisplay; src:TGLXContext; dst:TGLXContext; mask: cardinal);cdecl;external;
function glXCreateContext(dpy:PDisplay; vis:PXVisualInfo; share_list:TGLXContext; direct:TBool):TGLXContext;cdecl;external;
function glXCreateGLXPixmap(dpy:PDisplay; vis:PXVisualInfo; pixmap:TPixmap):GLXPixmap;cdecl;external;
procedure glXDestroyContext(dpy:PDisplay; ctx:TGLXContext);cdecl;external;
procedure glXDestroyGLXPixmap(dpy:PDisplay; pix:GLXPixmap);cdecl;external;
function glXGetConfig(dpy:PDisplay; vis:PXVisualInfo; attrib:longint; value:Plongint):longint;cdecl;external;
function glXGetCurrentContext:TGLXContext;cdecl;external;
function glXGetCurrentDrawable:GLXDrawable;cdecl;external;
function glXIsDirect(dpy:PDisplay; ctx:TGLXContext):TBool;cdecl;external;
function glXMakeCurrent(dpy:PDisplay; drawable:GLXDrawable; ctx:TGLXContext):TBool;cdecl;external;
function glXQueryExtension(dpy:PDisplay; error_base:Plongint; event_base:Plongint):TBool;cdecl;external;
function glXQueryVersion(dpy:PDisplay; major:Plongint; minor:Plongint):TBool;cdecl;external;
procedure glXSwapBuffers(dpy:PDisplay; drawable:GLXDrawable);cdecl;external;
procedure glXUseXFont(font:TFont; first:longint; count:longint; list_base:longint);cdecl;external;
procedure glXWaitGL;cdecl;external;
procedure glXWaitX;cdecl;external;

procedure g_return_if_fail(b: boolean; const Msg: string);
begin
  if not b then raise Exception.Create(Msg);
end;

procedure g_return_if_fail(b: boolean);
begin
  g_return_if_fail(b,'');
end;

function DefaultScreen(ADisplay: PDisplay): longint;
begin
  Result:= XDefaultScreen(ADisplay);
end;

function g_new(BaseSize, Count: integer): Pointer;
begin
  Result:= g_malloc(BaseSize*Count);
end;

function GetDefaultXDisplay: PDisplay;
begin
  Result:= GDK_DISPLAY;
end;

function get_xvisualinfo(visual: PGdkVisual): PXVisualInfo;
// IMPORTANT: remember to XFree returned XVisualInfo !!!
var
  vinfo_template: TXVisualInfo;
  dpy: PDisplay;
  nitems_return: integer;
  vi: PXVisualInfo;
begin
  dpy := GetDefaultXDisplay;
  {$IFDEF LCLGTK2}
  RaiseGDBException('GetDefaultXDisplay not implemented for gtk2');
  {$ENDIF}

  // 'GLX uses VisualInfo records because they uniquely identify
  // a (VisualID,screen,depth) tuple.'
  vinfo_template.bits_per_rgb:=0;
  FillChar(vinfo_template,SizeOf(vinfo_template),0);
  vinfo_template.visual   := GDK_VISUAL_XVISUAL({$IFDEF LCLGTK}
                                                PGdkVisualPrivate(visual)
                                                {$ELSE}
                                                visual
                                                {$ENDIF});
  vinfo_template.visualid := XVisualIDFromVisual(vinfo_template.visual);
  vinfo_template.depth    := PGdkVisualPrivate(visual)^.visual.depth;
  vinfo_template.screen   := DefaultScreen(GetDefaultXDisplay);
  vi := XGetVisualInfo(dpy, VisualIDMask or VisualDepthMask or VisualScreenMask,
                       @vinfo_template, @nitems_return);
  DebugLn('get_xvisualinfo nitems_return=',dbgs(nitems_return));
  // visualinfo needs to be unique
  if (vi=nil) then raise Exception.Create('get_xvisualinfo vi=nil');
  if (nitems_return<>1) then raise Exception.Create('get_xvisualinfo nitems_return='+dbgs(nitems_return));

  Result:=vi;
end;

procedure gtk_gl_area_destroy(obj: PGtkObject); cdecl;
var
  gl_area: PGtkGLArea;
begin
  g_return_if_fail (obj <>nil,'');
  g_return_if_fail (GTK_IS_GL_AREA(obj),'');
  gl_area := GTK_GL_AREA(obj);
  gdk_gl_context_unref(gl_area^.glcontext);
  if Assigned(GTK_OBJECT_CLASS(parent_class)^.destroy) then
    GTK_OBJECT_CLASS(parent_class)^.destroy(obj);
end;

procedure gtk_gl_area_class_init(klass: Pointer); cdecl;
var
  object_class: PGtkObjectClass;
begin
  parent_class := gtk_type_class(gtk_drawing_area_get_type());
  g_return_if_fail(parent_class<>nil,'gtk_gl_area_class_init parent_class=nil');
  object_class := PGtkObjectClass(klass);
  g_return_if_fail(object_class<>nil,'gtk_gl_area_class_init object_class=nil');
  object_class^.destroy := @gtk_gl_area_destroy;
end;

function gdk_gl_query: boolean;
begin
  Result:= boolean(glXQueryExtension(GetDefaultXDisplay,nil,nil){$IFDEF VER2_2}=true{$ENDIF});
end;

function gdk_gl_choose_visual(attrlist: Plongint): PGdkVisual;
var
  dpy: PDisplay;
  vi: PXVisualInfo;
  visual: PGdkVisual;
begin
  {$IFDEF LCLGTK2}
  RaiseGDBException('gdk_gl_choose_visual not implemented yet for gtk2');
  {$ENDIF}
  if attrList=nil then begin
    Result:=nil;
    exit;
  end;
  dpy := GetDefaultXDisplay;
  vi := glXChooseVisual(dpy,DefaultScreen(dpy), attrlist);
  if (vi=nil) then begin
    Result:=nil;
    exit;
  end;
  visual := gdkx_visual_get(vi^.visualid);
  XFree(vi);
  Result:=visual;
end;

function gdk_gl_get_config(visual: PGdkVisual; attrib: longint): longint;
var
  dpy: PDisplay;
  vi: PXVisualInfo;
  value: integer;
begin
  Result:=-1;
  if visual=nil then exit;
  dpy := GetDefaultXDisplay;
  vi := get_xvisualinfo(visual);
  if (glXGetConfig(dpy, vi, attrib, @value) = 0) then begin
    XFree(vi);
    Result:=value;
  end else
    XFree(vi);
end;

function gdk_gl_context_new(visual: PGdkVisual; attrlist: plongint): PGdkGLContext;
var
  dpy: PDisplay;
  vi: PXVisualInfo;
  PrivateContext: PGdkGLContextPrivate;
  glxcontext: TGLXContext;
begin
  Result:=nil;
  dpy := GetDefaultXDisplay;
  {$IFDEF LCLGTK2}
  if visual=nil then ;
  vi:=glXChooseVisual(dpy, DefaultScreen(dpy), @attrList[0]);
  {$ELSE}
  if visual=nil then exit;
  vi := get_xvisualinfo(visual);
  {$ENDIF}
  if vi=nil then
    raise Exception.Create('gdk_gl_context_new no visual found');

  glxcontext := glXCreateContext(dpy, vi, nil, direct);

  XFree(vi);
  if (glxcontext = nil) then exit;

  PrivateContext := g_new(SizeOf(TGdkGLContextPrivate), 1);
  PrivateContext^.xdisplay := dpy;
  PrivateContext^.glxcontext := glxcontext;
  PrivateContext^.ref_count := 1;

  Result := PGdkGLContext(PrivateContext);
end;

function gdk_gl_context_attrlist_new(attrlist: Plongint): PGdkGLContext;
var
  visual: PGdkVisual;
begin
  {$IFDEF LCLGTK2}
  visual:= nil;
  Result:= gdk_gl_context_new(visual, attrlist);
  {$ELSE}
  visual:= gdk_gl_choose_visual(attrlist);
  if (visual <> nil) then
    Result := gdk_gl_context_new(visual, attrlist)
  else
    Result := nil;
  {$ENDIF}
end;

function gdk_gl_context_ref(context: PGdkGLContext): PGdkGLContext;
var
  PrivateContext: PGdkGLContextPrivate;
begin
  Result:=nil;
  if context=nil then exit;
  PrivateContext := PGdkGLContextPrivate(context);
  inc(PrivateContext^.ref_count);
  Result:=context;
end;

procedure gdk_gl_context_unref(context: PGdkGLContext);
var
  PrivateContext: PGdkGLContextPrivate;
begin
  g_return_if_fail(context<>nil,'');
  PrivateContext:=PGdkGLContextPrivate(context);
  dec(PrivateContext^.ref_count);
  if (PrivateContext^.ref_count = 0) then begin
    if (PrivateContext^.glxcontext = glXGetCurrentContext()) then
      glXMakeCurrent(PrivateContext^.xdisplay, None, nil);
    glXDestroyContext(PrivateContext^.xdisplay, PrivateContext^.glxcontext);
    PrivateContext^.glxcontext:=nil;
    g_free(PrivateContext);
  end;
end;

function gdk_gl_make_current(drawable: PGdkDrawable;
  context: PGdkGLContext): boolean;
var
  PrivateContext: PGdkGLContextPrivate;
begin
  Result:=false;
  if  drawable=nil then exit;
  if context=nil then exit;
  PrivateContext := PGdkGLContextPrivate(context);

  Result:=boolean(glXMakeCurrent(PrivateContext^.xdisplay,
                                 {$IFDEF LCLGTK}
                                 GDK_WINDOW_XWINDOW(PGdkWindowPrivate(drawable)),
                                 {$ELSE}
                                 GDK_WINDOW_XWINDOW(drawable),
                                 {$ENDIF}
                                 PrivateContext^.glxcontext)
                                 {$IFDEF VER2_2}=true{$ENDIF});
end;

procedure gdk_gl_swap_buffers(drawable: PGdkDrawable);
begin
  g_return_if_fail(drawable <> nil);

  glXSwapBuffers({$IFDEF LCLGTK}
                 GDK_WINDOW_XDISPLAY(PGdkWindowPrivate(drawable)),
                 GDK_WINDOW_XWINDOW(PGdkWindowPrivate(drawable))
                 {$ELSE}
                 GDK_WINDOW_XDISPLAY(drawable),
                 GDK_WINDOW_XWINDOW(drawable)
                 {$ENDIF}
                 );
end;

procedure gdk_gl_wait_gdk;
begin
  glXWaitX;
end;

procedure gdk_gl_wait_gl;
begin
  glXWaitGL;
end;

procedure gtk_gl_area_init(
  {$IFDEF LCLGTK}
  gl_area, theClass: Pointer
  {$ELSE}
  gl_area: PGTypeInstance; theClass: gpointer
  {$ENDIF}
  ); cdecl;
begin
  if theClass=nil then ;
  //DebugLn(['gtk_gl_area_init START']);
  PGtkGLArea(gl_area)^.glcontext:=nil;
  {$IFDEF LCLGTK2}
  gtk_widget_set_double_buffered(PGtkWidget(gl_area),gdkFALSE);
  GTK_WIDGET_UNSET_FLAGS(PGtkWidget(gl_area),GTK_NO_WINDOW);
  {$ENDIF}
  //DebugLn(['gtk_gl_area_init END']);
end;

function GTK_TYPE_GL_AREA: TGtkType;
const
  gl_area_type_name = 'GtkGLArea';
  gl_area_info: TGtkTypeInfo = (
    type_name: gl_area_type_name;
    object_size: SizeOf(TGtkGLArea);
    class_size:  SizeOf(TGtkGLAreaClass);
    class_init_func:  @gtk_gl_area_class_init;
    object_init_func: @gtk_gl_area_init;
    reserved_1: nil;
    reserved_2: nil;
    base_class_init_func: nil;
  );
begin
  if (gl_area_type=0) then begin
    gl_area_type:=gtk_type_unique(gtk_drawing_area_get_type(),@gl_area_info);
  end;
  Result:=gl_area_type;
end;

function GTK_GL_AREA(obj: Pointer): PGtkGLArea;
begin
  g_return_if_fail(GTK_IS_GL_AREA(obj),'');
  Result:=PGtkGLArea(obj);
end;

function GTK_GL_AREA_CLASS(klass: Pointer): PGtkGLAreaClass;
begin
  g_return_if_fail(GTK_IS_GL_AREA_CLASS(klass),'');
  Result:=PGtkGLAreaClass(klass);
end;

function GTK_IS_GL_AREA(obj: Pointer): Boolean;
begin
  {$IFDEF LCLGTK}
  Result := Assigned(obj) and GTK_IS_GL_AREA_CLASS(PGtkTypeObject(obj)^.klass);
  {$ELSE}
  GTK_IS_GL_AREA:=GTK_CHECK_TYPE(obj,GTK_TYPE_GL_AREA);
  {$ENDIF}
end;

function GTK_IS_GL_AREA_CLASS(klass: Pointer): Boolean;
begin
  {$IFDEF LCLGTK}
  Result := Assigned(klass) and (PGtkTypeClass(klass)^.thetype = GTK_TYPE_GL_AREA);
  {$ELSE}
  GTK_IS_GL_AREA_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_GL_AREA);
  {$ENDIF}
end;

function gtk_gl_area_get_type: TGtkType;
begin
  Result:=GTK_TYPE_GL_AREA;
end;

function gtk_gl_area_new(attrList: Plongint): PGtkWidget;
var
  visual: PGdkVisual;
  glcontext: PGdkGLContext;
  gl_area: PGtkGLArea;
begin
  Result := nil;
  {$IFDEF LCLGTK2}
  visual := nil;
  {$ELSE}
  visual := gdk_gl_choose_visual(attrlist);
  if (visual = nil) then exit;
  {$ENDIF}

  glcontext := gdk_gl_context_new(visual,attrlist);
  if (glcontext = nil) then exit;

  if visual <> nil then begin
    // use colormap and visual suitable for OpenGL rendering
    gtk_widget_push_colormap(gdk_colormap_new(visual, gtk_TRUE));
    gtk_widget_push_visual(visual);
  end;

  gl_area := gtk_type_new (gtk_gl_area_get_type);
  gl_area^.glcontext := glcontext;

  if visual<>nil then begin
    // pop back defaults
    gtk_widget_pop_visual;
    gtk_widget_pop_colormap;
  end;
  Result:=PGtkWidget(gl_area);
end;

function gtk_gl_area_make_current(glarea: PGtkGLArea): boolean;
begin
  Result:=false;
  if glarea=nil then exit;
  if not GTK_IS_GL_AREA(glarea) then exit;
  if not GTK_WIDGET_REALIZED(PGtkWidget(glarea)) then exit;
  //DebugLn(['gtk_gl_area_make_current START']);
  Result:=gdk_gl_make_current(PGtkWidget(glarea)^.window, glarea^.glcontext);
  //DebugLn(['gtk_gl_area_make_current END']);
end;

function gtk_gl_area_begingl(glarea: PGtkGLArea): boolean;
begin
  Result:=gtk_gl_area_make_current(glarea);
end;

procedure gtk_gl_area_swap_buffers(gl_area: PGtkGLArea);
begin
  g_return_if_fail(gl_area <> nil);
  g_return_if_fail(GTK_IS_GL_AREA(gl_area));
  g_return_if_fail(GTK_WIDGET_REALIZED(PGtkWidget(gl_area)));

  gdk_gl_swap_buffers(GTK_WIDGET(gl_area)^.window);
end;

procedure LOpenGLSwapBuffers(Handle: HWND);
begin
  gtk_gl_area_swap_buffers(PGtkGLArea(Handle));
end;

function LOpenGLMakeCurrent(Handle: HWND): boolean;
var
  Widget: PGtkWidget;
  glarea: PGtkGLArea;
begin
  if Handle=0 then
    RaiseGDBException('LOpenGLSwapBuffers Handle=0');
  Result:=false;

  Widget:=PGtkWidget(PtrUInt(Handle));
  glarea:=PGtkGLArea(Widget);
  if not GTK_IS_GL_AREA(glarea) then
    RaiseGDBException('LOpenGLSwapBuffers not a PGtkGLArea');

  // make sure the widget is realized
  gtk_widget_realize(Widget);
  if not GTK_WIDGET_REALIZED(Widget) then exit;

  // make current
  Result:=gtk_gl_area_make_current(glarea);
end;

{$IFDEF LCLGTK2}
function gtkglarea_size_allocateCB(Widget: PGtkWidget; Size: pGtkAllocation;
  Data: gPointer): GBoolean; cdecl;
const
  CallBackDefaultReturn = {$IFDEF LCLGTK2}false{$ELSE}true{$ENDIF};
var
  SizeMsg: TLMSize;
  GtkWidth, GtkHeight: integer;
  LCLControl: TWinControl;
begin
  Result := CallBackDefaultReturn;
  if not GTK_WIDGET_REALIZED(Widget) then begin
    // the widget is not yet realized, so this GTK resize was not a user change.
    // => ignore
    exit;
  end;
  if Size=nil then ;
  LCLControl:=TWinControl(Data);
  if LCLControl=nil then exit;
  gtk_widget_get_size_request(Widget, @GtkWidth, @GtkHeight);
  SizeMsg.Msg:=0;
  FillChar(SizeMsg,SizeOf(SizeMsg),0);
  with SizeMsg do
  begin
    Result := 0;
    Msg := LM_SIZE;
    SizeType := Size_SourceIsInterface;
    Width := SmallInt(GtkWidth);
    Height := SmallInt(GtkHeight);
  end;
  LCLControl.WindowProc(TLMessage(SizeMsg));
end;
{$ENDIF}

function LOpenGLCreateContext(AWinControl: TWinControl;
  WSPrivate: TWSPrivateClass; DoubleBuffered, RGBA, IsDirect: boolean;
  const AParams: TCreateParams): HWND;
var
  NewWidget: PGtkWidget;
  AttrList: PInteger;
begin
  Direct:= TBool(IsDirect);
  if WSPrivate= nil then ;
  AttrList:= CreateOpenGLContextAttrList(DoubleBuffered,RGBA);
  try
    NewWidget:=gtk_gl_area_new(AttrList);
    Result:=HWND(PtrUInt(Pointer(NewWidget)));
    PGtkobject(NewWidget)^.flags:=PGtkobject(NewWidget)^.flags or GTK_CAN_FOCUS;
    {$IFDEF LCLGTK}
    TGTKWidgetSet(WidgetSet).FinishCreateHandle(AWinControl,NewWidget,AParams);
    {$ELSE}
    TGTK2WidgetSet(WidgetSet).FinishCreateHandle(AWinControl,NewWidget,AParams);
    g_signal_connect_after(PGtkObject(NewWidget), 'size-allocate',
                       TGTKSignalFunc(@gtkglarea_size_allocateCB), AWinControl);
    {$ENDIF}
  finally
    FreeMem(AttrList);
  end;
end;

procedure LOpenGLDestroyContextInfo(AWinControl: TWinControl);
begin
  if not AWinControl.HandleAllocated then exit;
  // nothing to do
end;

function CreateOpenGLContextAttrList(DoubleBuffered: boolean; RGBA: boolean
  ): PInteger;
var
  p: integer;
  
  procedure Add(i: integer);
  begin
    if Result<>nil then
      Result[p]:=i;
    inc(p);
  end;
  
  procedure CreateList;
  begin
    if DoubleBuffered then
      Add(GDK_GL_DOUBLEBUFFER);
    if RGBA then
      Add(GDK_GL_RGBA);
    Add(GDK_GL_RED_SIZE);  Add(1);
    Add(GDK_GL_GREEN_SIZE);  Add(1);
    Add(GDK_GL_BLUE_SIZE);  Add(1);
    Add(GDK_GL_DEPTH_SIZE);  Add(1);
    Add(GDK_GL_STENCIL_SIZE); Add(1);
    Add(GDK_GL_None);
  end;
  
begin
  Result:=nil;
  p:=0;
  CreateList;
  GetMem(Result,SizeOf(integer)*p);
  p:=0;
  CreateList;
end;

initialization

direct:= TBool(1);

end.

