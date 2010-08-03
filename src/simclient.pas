unit simclient;

{$I mocca.inc}

interface

uses
  Classes, Menus, SysUtils, LResources, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Buttons, ExtCtrls,gllist,glu,gl,
  mocglb,
  {$IFNDEF OWNGL}
  OpenGlContext;
  {$ELSE}
  mocgl;
  {$ENDIF}

type
  TViewModes = (vmPerspective,vmZ,vmY,vmX,vmZDown);

  TViewModeParams = record
    xrot: single;
    yrot: single;
    zrot: single;
  end;

const
  ViewRotation : Array[TViewModes] of TViewModeParams =
    ((xrot: -45; yrot: 0; zrot: 0),
     (xrot: 0; yrot: 0; zrot: 0),
     (xrot: -90; yrot: 0; zrot: 0),
     (xrot: -90; yrot: 0; zrot: -90),
     (xrot: 90; yrot: 0; zrot: 0));

var
  InitialViewMode: TViewModes;

type
  { TSimClientForm }
  TSimClientForm = class(TForm)
    MItem3D: TMenuItem;
    MItemInfo: TMenuItem;
    MItemReset: TMenuItem;
    Popup: TPopupMenu;
    sbH: TScrollBar;
    sbV: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MItem3DClick(Sender: TObject);
    procedure sbHChange(Sender: TObject);
    procedure sbVChange(Sender: TObject);
    procedure OglPaint(Sender: TObject);
    procedure OglResize(Sender: TObject);
    procedure OglMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure OglMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OglMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure OglMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    {$IFNDEF OWNGL}
    ogl: TOpenGlControl;
    {$ELSE}
    ogl: TMocGlControl;
    {$ENDIF}

    FShowLivePlot: Boolean;
    FShowDimensions: Boolean;
    FViewMode: TViewModes;
    FFileName: string;
    FUnitCode: string;
    FInitCode: string;

    RotationX,RotationY,RotationZ: double;
    PanX,PanY,PanZ: double;
    CenterX,CenterY,CenterZ: double;
    EyeX,EyeY,EyeZ: Double;
    ConeX,ConeY,ConeZ: Double;
    ConeRX,ConeRY,ConeRZ: Double;
    ExtX,ExtY,ExtZ: double;
    L: TExtents;
    DimDrawFlag: Boolean;
    DimScale: Double;
    DimDist: Double;
    Offset: tlo;
    ConeL,LimitsL,CoordsL,ListL: gluInt;
    MouseX,MouseY: integer;
    Moving: Boolean;
    AreaInitialized: Boolean;
    ZoomMax: integer;
    View3D: Boolean;

    procedure MakeLimits;
    procedure MakeList;
    procedure MakeCoords;
    procedure MakeMillCone;
    procedure MakeLatheCone;
    procedure MakeCone;
    procedure MoveCone(X,Y,Z,A,B,C: Double);
    procedure DrawCone;
    procedure Pan(DX,DY: integer);
    procedure UpdateView;
    procedure UpdateDim;
    procedure DrawDim;
    procedure DrawDimX;
    procedure DrawDimY;
    procedure DrawDimZ;
    procedure ResetView;
    procedure RotateZ(Angle: integer);
    procedure RotateX(Angle: integer);
    procedure SetViewMode(Value: TViewModes);
  public
    procedure Show3D;
    procedure LoadFile(FileName,UnitCode,InitCode: string);
    procedure ReloadFile;
    procedure ClearFile;
    procedure UpdateTool(AToolNo: integer);
    procedure Zoom(ADir: Integer);
    procedure ClearPlot;
    procedure UpdateSelf;
    procedure InvalidateView;
    property  ShowLivePlot: Boolean read FShowLivePlot write FShowLivePlot;
    property  ShowDimensions: Boolean read FShowDimensions write FShowDimensions;
    property  ViewMode: TViewModes read FViewMode write SetViewMode;
  end;
  
var
  clSim: TSimClientForm;

implementation

uses
  math,mocjoints,emc2pas, glcanon,
  logger, glfont, simulator;

type
  TGlVector3 = array[0..2] of glDouble;

var
  Tool: TTool;

function v3distsq(a,b: TGlVector3): glDouble;
var
  d: TglVector3;
begin
  d[0]:= a[0] - b[0];
  d[1]:= a[1] - b[1];
  d[2]:= a[2] - b[2];
  Result:= d[0]*d[0] + d[1]*d[1] + d[2]*d[2]
end;

procedure TSimClientForm.Show3D;
begin
  if Assigned(Renderer) then
    if FFileName <> '' then
      if Assigned(ogl) and AreaInitialized then
        try
          if View3D then
            begin
              ResetView;
              with GlColors.bg do
                glClearColor(r,g,b,1);
              Exit;
            end;
          View3D:= Show3DPreviewDlg(ListL);
          if View3d then
            glClearColor(0,0,0,1);
          ogl.Invalidate;
        except
          on E:Exception do
            writeln('Render : ' + E.Message);
        end;
end;

procedure TSimClientForm.ClearFile;
begin
  if Assigned(Renderer) then
    Renderer.Clear;
  FFileName:= '';
  UpdateView;
 end;

procedure TSimCLientForm.InvalidateView;
begin
  if Assigned(ogl) then
    ogl.Invalidate;
end;

procedure TSimClientForm.ClearPlot;
begin
  LoggerClear;
  InvalidateView;
end;

procedure TSimClientForm.LoadFile(FileName,UnitCode,InitCode: string);
begin
  FFileName:= FileName;
  FUnitCode:= UnitCode;
  FInitCode:= InitCode;
  ReloadFile;
end;

procedure TSimClientForm.ReloadFile;
var
  Error: integer;
begin
  if (not Assigned(Renderer)) then
    raise Exception.Create('cannot show preview without a gl_renderer');
  if FFileName <> '' then
    begin
      Renderer.Clear;
      Error:= ParseGCode(FFileName,FUnitCode,FInitCode);
      if Error <> 0 then
        LastError:= GetGCodeError(Error);
    end;
  CanonInitOffset;
  Offset:= CanonOffset;
  UpdateView;
end;

procedure TSimClientForm.UpdateSelf;
var
  x,y,z: double;
begin
  if Visible and AreaInitialized and Assigned(Joints) then
    begin
      //X:= GetRelPos(0);
      //Y:= GetRelPos(1);
      //Z:= GetRelPos(2);
      //A:= GetRelPos(3); // GetRelPos(3);
      //B:= GetRelPos(4); // GetRelPos(4);
      //C:= GetRelPos(5);

      // Neu, Test
      x:= GetLoggerPos(0);
      y:= GetLoggerPos(1);
      z:= GetLoggerPos(2);
      MoveCone(x,y,z,0,0,0);
    end;
end;

procedure TSimClientForm.sbHChange(Sender: TObject);
begin
  if AreaInitialized then RotateZ(sbH.Position);
end;

procedure TSimClientForm.sbVChange(Sender: TObject);
begin
  if AreaInitialized then RotateX(sbV.Position);
end;

procedure TSimClientForm.FormCreate(Sender: TObject);
begin

  AreaInitialized:= False;

  if Vars.IsLathe then
    FViewMode:= vmZDown
  else
    FViewMode:= vmPerspective;

  FShowLivePlot:= True;
  FShowDimensions:= True;
  FFileName:= '';

  ConeL:= 1;
  LimitsL:= 2;
  CoordsL:= 3;
  ListL:= 4;

  ZoomMax:= 10;

  DimScale:= 1;
  DimDist:= 1;
  DimDrawFlag:= True;

  View3D:= False;

  sbV.setParams(Round(ViewRotation[FViewMode].xrot),-180,180,1);
  sbH.SetParams(Round(ViewRotation[FViewMode].yrot),-180,180,1);

  if not Assigned(Renderer) then
    Renderer:= TGlRenderer.Create;
  if not Assigned(Renderer) then
    RaiseError('could not create gllist');
  {$IFNDEF OWNGL}
  ogl:= TOpenGlControl.Create(Self);
  ogl.AutoResizeViewPort:= True;
  ogl.RGBA:= GlSettings.UseRGBA;
  ogl.DoubleBuffered:= GlSettings.UseDoubleBuffered;
  {$ELSE}
  ogl:= TMocGlControl.Create(Self);
  {$ENDIF}
  if not Assigned(ogl) then
    RaiseError('could not create opengl-control');
  ogl.Parent:= self;
  ogl.OnPaint:= @self.OglPaint;
  ogl.OnResize:= @self.OglResize;
  ogl.OnMouseWheel:= @self.OglMouseWheel;
  ogl.OnMouseDown:= @self.OglMouseDown;
  ogl.OnMouseMove:= @self.OglMouseMove;
  ogl.OnMouseUp:= @self.OglMouseUp;
  ogl.PopupMenu:= Self.PopUp;
  CanonInitOffset;
  Offset:= CanonOffset;
end;

procedure TSimClientForm.FormDestroy(Sender: TObject);
begin
  try
    if Assigned(ogl) then
      FreeAndNil(ogl);
    if Assigned(Renderer) then
      FreeAndNil(Renderer);
  except
    on E:Exception do
      writeln('Destroy Renderer: ' + E.Message);
  end;
end;

procedure TSimClientForm.FormResize(Sender: TObject);
begin
  if (sbV.Height > 0) and (sbH.Width > 0) then
    Ogl.SetBounds(sbH.Left,sbV.Top,sbH.Width,sbV.Height);
  if Assigned(ogl) then
    OglResize(nil);
end;

procedure TSimClientForm.FormShow(Sender: TObject);
begin
  FormResize(nil);
  UpdateView;
end;

procedure TSimClientForm.MItem3DClick(Sender: TObject);
begin
  if Assigned(Renderer) then
    Show3D;
end;

procedure TSimClientForm.RotateZ(Angle: integer);
begin
  if Round(RotationZ) <> Angle then
    begin
      RotationZ:= Angle;
      if Assigned(ogl) then
        ogl.Invalidate;
    end;
end;

procedure TSimClientForm.RotateX(Angle: integer);
begin
  if Round(RotationX) <> Angle then
    begin
      RotationX:= Angle;
      if Assigned(ogl) then
        ogl.Invalidate;
    end;
end;

procedure TSimClientForm.SetViewMode(Value: TViewModes);

  procedure SetRot(x,y,z: single);
  begin
    rotationX:= x; rotationY:= y; rotationZ:= z;
  end;

begin
  if (not Assigned(ogl)) or (not AreaInitialized) then
    Exit;
  if (Value <> FViewMode) then
    begin
      FViewMode:= Value;
      ResetView;
      with ViewRotation[FViewMode] do
        begin
          RotationX:= xrot;
          RotationY:= yrot;
          RotationZ:= zrot;
        end;
      sbV.setParams(Round(RotationX),-90,90);
      sbH.SetParams(Round(RotationZ),-90,90);
      ogl.Invalidate;
    end;
end;

procedure TSimClientForm.Pan(dx,dy: integer);
var
  V: TViewPortArray;
  P,M : T16DArray;
  ObjC: TGlVector3;
  Obj: TGlVector3;
  WinX,WinY,WinZ: glDouble;
  WinHeight: integer;
  d: glDouble;
  Scale: double;
begin
  if not AreaInitialized then Exit;
  WinHeight:= ogl.Height;
  if WinHeight < 1 then WinHeight:= 1;
  glGetDoublev(GL_MODELVIEW_MATRIX, @M);
  glGetDoublev(GL_PROJECTION_MATRIX, @P);
  glGetIntegerv(GL_VIEWPORT, @V);
  ObjC[0]:= CenterX; ObjC[1]:= CenterY; ObjC[2]:= CenterZ;
  gluProject(ObjC[0],ObjC[1],ObjC[2],M,P,V,@WinX,@WinY,@WinZ);
  gluUnProject(WinX,WinY + 0.5 * WinHeight, WinZ,M,P,V,@Obj[0],@Obj[1],@Obj[2]);
  d:= sqrt(v3distsq(Obj,ObjC));
  scale:= abs( d / (0.5 * WinHeight));
  PanX:= dx * scale;
  PanY:= -dy * scale;
end;

procedure TSimClientForm.OglMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not AreaInitialized then Exit;
  MouseX:= X;
  MouseY:= Y;
  PanX:= 0;
  PanY:= 0;
  Moving:= True;
end;

procedure TSimClientForm.OglMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Dx,Dy: integer;
begin
  if not AreaInitialized then Exit;
  if Moving then
    begin
      dx:= MouseX - X;
      dy:= MouseY - Y;
      Pan(dx,dy);
      Ogl.Invalidate;
    end;
end;

procedure TSimClientForm.OglMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Moving:= False;
  MouseX:= 0;
  MouseY:= 0;
  if not AreaInitialized then Exit;
  EyeX:= EyeX + PanX;
  EyeY:= EyeY + PanY;
  PanX:= 0;
  PanY:= 0;
end;

procedure TSimClientForm.Zoom(ADir: integer);
begin
  if Assigned(Ogl) and AreaInitialized then
    begin
      if ADir < 0 then
        EyeZ:= EyeZ / 0.8
      else
      if ADir > 0 then
        EyeZ:= EyeZ * 0.8
      else
        EyeZ:= 1;
      if EyeZ < 0.1 then EyeZ:= 0.1;
      if EyeZ > ZoomMax then EyeZ:= ZoomMax;
      Ogl.Invalidate;
    end;
end;

procedure TSimClientForm.MoveCone(x,y,z,a,b,c: Double);
var
  cx,cy,cz,ra,rb,rc: Double;
begin
  if not Assigned(ogl) then Exit; 
  if not AreaInitialized then Exit;
  cx:= ToCanonPos(x,0);
  cy:= ToCanonPos(y,1);
  cz:= ToCanonPos(z,2);
  ra:= ToCanonPos(a,3);
  rb:= ToCanonPos(b,4);
  rc:= ToCanonPos(c,5);
  if (cx <> ConeX) or (cy <> ConeY) or (cz <> ConeZ) or
    (ra <> ConeRX) or (rb <> ConeRY) or (rc <> ConeRZ) then
    begin
      ConeX:= cx; ConeY:= cy; ConeZ:= cz;
      ConeRX:= ra; ConeRY:= rb; ConeRZ:= rc;
      if FShowLivePlot then
        LoggerAddPoint(cx,cy,cz);
      Ogl.Invalidate;
    end;
end;

procedure TSimClientForm.UpdateDim;
var
  ExtMax: Double;
begin
  ExtMax:= 1;
  if ExtX > ExtMax then ExtMax:= ExtX;
  if ExtY > ExtMax then ExtMax:= ExtY;
  if ExtZ > ExtMax then ExtMax:= ExtZ;
  DimScale:= ExtMax / 25;
  DimDist:= 1.3 * DimScale;
end;

procedure DimColor(Value,Limit: Double; IsMax: Boolean);
var
  f: Boolean;
begin
  if IsMax then
    f:= Value > Limit
  else
    f:= Value < Limit;
  if f then
    SetGlColor3(glColors.dim2)
end;

procedure TSimClientForm.DrawDimX;
var
  i: integer;
  w,tw,x,y,z: double;
  s: string;
begin
  if DimScale = 0 then Exit;
  // draw the X-Dimension
  if Vars.ShowMetric then
    w:= ExtX * 25.4
  else
    w:= ExtX;
  s:= PosToString(w);
  y:= L.MinY - DimDist;
  z:= L.MinZ;
  glPushMatrix;
  glBegin(GL_LINES);
  glVertex3f(L.MinX,y,z);
  glVertex3f(L.MaxX,y,z);
  glEnd;
  glBegin(GL_LINES);
  glVertex3f(L.MinX,y - DimDist,z);
  glVertex3f(L.MinX,y + DimDist,z);
  glEnd;
  glBegin(GL_LINES);
  glVertex3f(L.MaxX,y - DimDist,z);
  glVertex3f(L.MaxX,y + DimDist,z);
  glEnd;
  tw:= (Length(s) * GlFontDist) * DimScale;
  if tw < ExtX then
    x:= L.MinX + (ExtX / 2) - (tw / 2)
  else
    x:= L.MaxX + DimDist;
  y:= L.MinY - (3 * DimDist);
  glTranslateF(x,y,z);
  glScaleF(DimScale,DimScale,DimScale);
  for i:= 1 to Length(s) do
    begin
      DrawGlDigit(s[i]);
      glTranslateF(GlFontDist,0,0);
    end;
  glPopMatrix;
  // Draw the Min-X Limit
  if Vars.ShowMetric then
    w:= L.MinX * 25.4
  else
    w:= L.MinX;
  s:= PosToString(w);
  tw:= (Length(s) * GlFontDist) * DimScale;
  y:= L.MinY - (2 * DimDist) - tw;
  x:= L.MinX - (DimDist / 2);
  glPushMatrix;
  glTranslateF(x,y,z);
  glRotateF(90,0,0,1);
  glScaleF(DimScale,DimScale,DimScale);
  for i:= 1 to Length(s) do
    begin
      DrawGlDigit(S[i]);
      glTranslateF(GlFontDist,0,0);
    end;
  glPopMatrix;
  // Draw the Max-X Limit
  if Vars.ShowMetric then
    w:= L.MaxX * 25.4
  else
    w:= L.MaxX;
  s:= PosToString(w);
  tw:= (Length(s) * GlFontDist) * DimScale;
  y:= L.MinY - (2 * DimDist) - tw;
  x:= L.MaxX + (DimDist / 2) + DimScale;
  glPushMatrix;
  glTranslateF(x,y,z);
  glRotateF(90,0,0,1);
  glScaleF(DimScale,DimScale,DimScale);
  for i:= 1 to Length(s) do
    begin
      DrawGlDigit(S[i]);
      glTranslateF(GlFontDist,0,0);
    end;
  glPopMatrix;
end;

procedure TSimClientForm.DrawDimY;
var
  i: integer;
  w,tw,x,y,z: double;
  s: string;
begin
  if DimScale = 0 then Exit;
  if Vars.ShowMetric then
    w:= ExtY * 25.4
  else
    w:= ExtY;
  s:= PosToString(w);
  if s = '' then Exit;
  x:= L.MinX - DimDist;
  z:= L.MinZ;
  glPushMatrix;
  glBegin(GL_LINES);
  glVertex3f(x,L.MinY,z);
  glVertex3f(x,L.MaxY,z);
  glEnd;
  glBegin(GL_LINES);
  glVertex3f(x - DimDist,L.MaxY,z);
  glVertex3f(x + DimDist,L.MaxY,z);
  glEnd;
  glBegin(GL_LINES);
  glVertex3f(x - DimDist,L.MinY,z);
  glVertex3f(x + DimDist,L.MinY,z);
  glEnd;
  tw:= (Length(s) * GlFontDist) * DimScale;
  if tw < ExtY then
    y:= L.MinY + (ExtY / 2) - (tw / 2)
  else
    y:= L.MaxY + DimDist;
  x:= L.MinX - (2 * DimDist);
  glTranslateF(x,y,z);
  glRotateF(90,0,0,1);
  glScaleF(DimScale,DimScale,DimScale);
  for i:= 1 to Length(S) do
    begin
      DrawGlDigit(S[i]);
      glTranslateF(GlFontDist,0,0);
    end;
  glPopMatrix;
  // Draw the Min-Y Limit
  if Vars.ShowMetric then
    w:= L.MinY * 25.4
  else
    w:= L.MinY;
  s:= PosToString(w);
  tw:= (Length(s) * GlFontDist) * DimScale;
  x:= L.MinX - (2 * DimDist) - tw;
  y:= L.MinY - (DimDist / 2) - DimScale;
  glPushMatrix;
  glTranslateF(x,y,z);
  glScaleF(DimScale,DimScale,DimScale);
  for i:= 1 to Length(s) do
    begin
      DrawGlDigit(S[i]);
      glTranslateF(GlFontDist,0,0);
    end;
  glPopMatrix;
  // Draw the Max-Y Limit
  if Vars.ShowMetric then
    w:= L.MaxY * 25.4
  else
    w:= L.MaxY;
  s:= PosToString(w);
  tw:= (Length(s) * GlFontDist) * DimScale;
  x:= L.MinX - (2 * DimDist) - tw;
  y:= L.MaxY + (DimDist / 2);
  glPushMatrix;
  glTranslateF(x,y,z);
  glScaleF(DimScale,DimScale,DimScale);
  for i:= 1 to Length(s) do
    begin
      DrawGlDigit(S[i]);
      glTranslateF(GlFontDist,0,0);
    end;
  glPopMatrix;
end;

procedure TSimClientForm.DrawDimZ;
var
  i: integer;
  w,tw,x,y,z: double;
  s: string;
begin
  if DimScale = 0 then Exit;
  if Vars.ShowMetric then
    w:= ExtZ * 25.4
  else
    w:= ExtZ;
  s:= PosToString(w);
  if s = '' then Exit;
  x:= L.MinX - DimDist;
  y:= L.MinY;
  glPushMatrix;
  glBegin(GL_LINES);
  glVertex3f(x,y,L.MinZ);
  glVertex3f(x,y,L.MaxZ);
  glEnd;
  glBegin(GL_LINES);
  glVertex3f(x - DimDist,y,L.MaxZ);
  glVertex3f(x + DimDist,y,L.MaxZ);
  glEnd;
  glBegin(GL_LINES);
  glVertex3f(x - DimDist,y,L.MinZ);
  glVertex3f(x + DimDist,y,L.MinZ);
  glEnd;
  tw:= (Length(s) * GlFontDist) * DimScale;
  if tw < ExtZ then
    z:= L.MinZ + (ExtZ / 2) - (tw / 2)
  else
    z:= L.MaxZ; //  - DimDist;
  x:= L.MinX - (2 * DimDist);
  glTranslateF(x,y,z);
  glRotateF(90,1,0,0);
  glRotateF(90,0,0,1);
  glScaleF(DimScale,DimScale,DimScale);
  for i:= 1 to Length(S) do
    begin
      DrawGlDigit(S[i]);
      glTranslateF(GlFontDist,0,0);
    end;
  glPopMatrix;
   // Draw the Min-Z Limit
  if Vars.ShowMetric then
    w:= L.MinZ * 25.4
  else
    w:= L.MinZ;
  s:= PosToString(w);
  tw:= (Length(s) * GlFontDist) * DimScale;
  x:= L.MinX - (2 * DimDist) - tw;
  z:= L.MinZ - DimDist - DimScale;
  glPushMatrix;
  glTranslateF(x,y,z);
  glRotateF(90,1,0,0);
  glScaleF(DimScale,DimScale,DimScale);
  for i:= 1 to Length(s) do
    begin
      DrawGlDigit(S[i]);
      glTranslateF(GlFontDist,0,0);
    end;
  glPopMatrix;
  // Draw the Max-Z Limit
  if Vars.ShowMetric then
    w:= L.MaxZ * 25.4
  else
    w:= L.MaxZ;
  s:= PosToString(w);
  tw:= (Length(s) * GlFontDist) * DimScale;
  x:= L.MinX - (2 * DimDist) - tw;
  z:= L.MaxZ + (DimDist / 2);
  glPushMatrix;
  glTranslateF(x,y,z);
  glRotateF(90,1,0,0);
  glScaleF(DimScale,DimScale,DimScale);
  for i:= 1 to Length(s) do
    begin
      DrawGlDigit(S[i]);
      glTranslateF(GlFontDist,0,0);
    end;
  glPopMatrix;
end;

procedure TSimClientForm.DrawDim;
begin
  if Assigned(ogl) and AreaInitialized then
    begin
      if (Vars.AxisMask AND 1) <> 0 then DrawDimX;
      if (Vars.AxisMask AND 2) <> 0 then DrawDimY;
      if (Vars.AxisMask AND 4) <> 0 then DrawDimZ;
    end;
end;

procedure TSimClientForm.UpdateView;

  procedure CalcExtents;
  begin
    ExtX:= (L.maxX - L.minX);
    ExtY:= (L.maxY - L.minY);
    ExtZ:= (L.maxZ - L.minZ);
  end;

begin
  L:= SetExtents(0,0,0,0,0,0);
  if (Assigned(Renderer)) and (FFileName <> '') then
    Renderer.GetExtents(L)
  else
    L:= Vars.MLimits;
  CalcExtents;
  if (ExtX < 0.0001) and (ExtY < 0.0001) and (ExtZ < 0.0001) then
    begin
      Writeln('Preview is too small. Using Machine Limits...');
      L:= Vars.MLimits;
      CalcExtents;
    end;
  Offset:= CanonOffset;
  UpdateDim;
  ResetView;
end;

procedure TSimClientForm.ResetView;
begin
  Centerx:= L.maxX - (ExtX / 2); // + offset.x;
  Centery:= L.maxY - (ExtY / 2); // + offset.y;
  Centerz:= L.maxZ - (ExtZ / 2); // + offset.z;
  // writeln(Format('%s %f %f %f',['Center: ',CenterX,CenterY,CenterZ]));
  // writeln(Format('%s %f %f %f',['Extents: ',ExtX,ExtY,ExtZ]));
  PanX:= 0; PanY:= 0; PanZ:= 0; EyeX:= 0; EyeY:= 0;
  View3D:= False;
  if FViewMode = vmPerspective then
    begin
      with ViewRotation[vmPerspective] do
        begin
          RotationX:= xrot;
          RotationY:= yrot;
          RotationZ:= zrot;
        end;
      if ExtX < ExtY then
        EyeZ:= (CenterZ + ExtY) * 1.2
      else
        EyeZ:= (CenterZ + ExtX) * 1.2;
      ZoomMax:= Round(EyeZ*100);
    end
  else
    begin
      if ExtX < ExtY then
        EyeZ:= CenterZ + Sqr(ExtY)
      else
        EyeZ:= CenterZ + Sqr(ExtX);
      ZoomMax:= Round(EyeZ*20);
    end;
  if ZoomMax > 10000 then ZoomMax:= 10000 else
    if ZoomMax < 10 then ZoomMax:= 10;
  if Round(EyeZ) < 1 then EyeZ:= 1;

  if not Assigned(ogl) then Exit;

  if AreaInitialized then
    begin
      if Ogl.MakeCurrent then
        begin
          MakeCone;
          MakeCoords;
          MakeLimits;
          MakeList;
        end;
      Ogl.Invalidate;
    end;
end;

procedure TSimClientForm.MakeList;
begin
  if not Assigned(ogl) then Exit;
  if not AreaInitialized then Exit;
  if Assigned(Renderer) then
    Renderer.MakeList(ListL);
end;

type
  Float4 = Array[0..3] of GLFloat;

function Glf4(v1,v2,v3,v4: GlFloat): Float4;
begin
  Result[0]:= v1;
  Result[1]:= v2;
  Result[2]:= v3;
  Result[3]:= v4;
end;

procedure Lightning(x,y,z: double);
begin
   glClearColor(0,0,0,1);
   glEnable(GL_NORMALIZE);
   glLightfv(GL_LIGHT0, GL_POSITION,Glf4(x,y,z,0));
   glLightfv(GL_LIGHT0, GL_AMBIENT, Glf4(0.2,0.2,0.3,0));
   glLightfv(GL_LIGHT0, GL_DIFFUSE,Glf4(0.4,0.4,0.6,0));
   //glLightfv(GL_LIGHT0+1, GL_POSITION,Glf4(0,0,-500, 0));
   //glLightfv(GL_LIGHT0+1, GL_AMBIENT, Glf4(0.8,0,0,1));
   //glLightfv(GL_LIGHT0+1, GL_DIFFUSE, Glf4(0.8,0,0,0));
   //glPopMatrix;
   glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE,Glf4 (1,1,1,1));
   glEnable(GL_LIGHTING);
   glEnable(GL_LIGHT0);
   //glEnable(GL_LIGHT0+1);
   glDepthFunc(GL_LESS);
   glEnable(GL_DEPTH_TEST);
   glEnable(GL_CULL_FACE);
   glDisable(GL_NORMALIZE);
end;

procedure TSimClientForm.OglPaint(sender: TObject);

const GLInitialized: boolean = false;
var
  k,n: double;
  x,y,z: double;

procedure InitGL;
begin
  if GLInitialized then Exit;
  GLInitialized:= True;
  with GlColors.bg do
    glClearColor(r,g,b,1);
  glClearDepth(1.0);
  if Assigned(Ogl) then
    if (ogl.Height > 0) and (ogl.Width > 0) then
      glViewPort(0,0,ogl.Width,ogl.Height);
  EyeX:= 0;
  EyeY:= 0;
  MakeCone;
  MakeCoords;
  MakeLimits;
  BuildGlFont;
end;

begin
  if Sender = nil then ;
  if not AreaInitialized then
    begin
      InitGL;
      AreaInitialized:= True;
    end;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  if FViewMode = vmPerspective then
    gluPerspective(60,ogl.Width/ogl.Height,0.1,ExtZ * 100)
  else
    begin
      k:= sqrt(abs(EyeZ));
      n:= k * ogl.height / ogl.width;
      glOrtho(-k,k,-n,n, -1000, 1000);
    end;
  glTranslatef(-eyex -panx,-eyey -pany, -eyez);
  glMatrixMode(GL_MODELVIEW);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;

  {$IFDEF LINESMOOTH}
  glEnable(GL_LINE_SMOOTH);
  {$ENDIF}

  if Vars.IsLathe then
    begin
      glRotatef(-90,0,0,1);
      glRotatef(-90,1,0,0);
      //glRotatef(90,1.0,0.0,0.0);
    end
  else
    begin
      glRotatef(RotationX,1.0,0.0,0.0);
      glRotatef(RotationY,0.0,1.0,0.0);
      glRotatef(RotationZ,0.0,0.0,1.0);
    end;

  glTranslatef(-centerx,-centery,-centerz);

  if not View3D then
    begin
      glDisable(GL_LIGHTING);
      glDisable(GL_CULL_FACE);
      with GlColors.bg do
        glClearColor(r,g,b,1);
      glCallList(LimitsL);
      glPushMatrix;
      //Offset:= CanonOffset;
      //glTranslatef(offset.x,offset.y,offset.z);
      x:= ToCanonUnits(GetOrigin(0));
      y:= ToCanonUnits(GetOrigin(1));
      z:= ToCanonUnits(GetOrigin(2));
      glTranslatef(x,y,z);
      // end test

      glCallList(CoordsL);
      glPopMatrix;
    end
  else
    begin
      Lightning(0,0,-ExtZ * 5);
    end;

  glCallList(ListL);

  if not View3D then
    begin
      DrawCone;
      SetGlColor3(GlColors.toolpath);
      if FShowLivePlot then
        LoggerCall;
      if FShowDimensions then
      DrawDim;
    end;
  {$IFDEF LINESMOOTH}
  glDisable(GL_LINE_SMOOTH);
  {$ENDIF}
  ogl.SwapBuffers;
end;

procedure TSimClientForm.OglMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(ogl) then
    if Assigned(Renderer) then
      begin
        if WheelDelta < 0 then
          EyeZ:= EyeZ / 1.2
        else
          EyeZ:= EyeZ * 1.2;
        if EyeZ > ZoomMax then
          EyeZ:= ZoomMax
        else
        if EyeZ < 1 then EyeZ:= 1;
        Handled:= True;
        ogl.Invalidate;
      end;
end;

procedure TSimClientForm.OglResize(Sender: TObject);
begin
  if Assigned(ogl) then
    if ogl.Height > 0 then
      if AreaInitialized then
        glViewport(0,0,ogl.Width,ogl.Height);
end;

procedure TSimClientForm.UpdateTool(AToolNo: integer);
begin
  if (AToolNo < 1) or (AToolNo > CANON_TOOL_MAX) then
    Tool:= EmptyTool
  else
    Tool:= Tools[AToolNo];
  if not Assigned(ogl) then Exit;
  if AreaInitialized then
    begin 
      if Ogl.MakeCurrent then
        MakeCone;
      Ogl.Invalidate;
    end;
end;

procedure TSimClientForm.MakeLatheCone;
const
  lathe_shapes : Array[1..9] of TPoint =
    ((x:1;y:-1),(x:1;y:1),(x:-1;y:1),(x:-1;y:-1),(x:0;y:-1),
     (x:1;y:0),(x:0;y:1),(x:-1;y:0),(x:0;y:0));
var
  t,r: double;
  i,dx,dy: integer;
  min_angle,max_angle: double;
  sinmax,sinmin,cosmax,cosmin: double;
  circleminangle,circlemaxangle: double;
  sz: double;
  Orient: integer;
  fa,ba: double;
begin
  orient:= Tool.Orientation;
  if orient < 0 then orient:= 0;
  if orient > 9 then orient:= 9;
  r:= ToCanonUnits(Tool.diameter) / 2;
  //if r < DEFAULT_TOOL_DIA / 2 then
  //  r:= DEFAULT_TOOL_DIA / 2;
  fa:= Tool.frontangle;
  ba:= Tool.backangle;

  glEnable(GL_BLEND);
  SetGlColor4(GlColors.Cone);
  glDepthFunc(GL_ALWAYS);
  glBegin(GL_LINES);
  glVertex3f(-r/2,0,0);
  glVertex3f( r/2,0,0);
  glVertex3f(0,0,-r/2);
  glVertex3f(0,0,r/2);
  glEnd();

  glNormal3f(0,1,0);
  if Orient = 9 then
    begin
      glBegin(GL_TRIANGLE_FAN);
      for i:= 0 to 37 do
        begin
          t:= i * pi / 18;
          glVertex3f(r * cos(t),0, r * sin(t));
        end;
      glEnd();
    end
  else
    begin
      dx:= Lathe_Shapes[Orient].X;
      dy:= Lathe_Shapes[Orient].Y;

      min_angle:= min(ba,fa) * pi / 180;
      max_angle:= max(ba,fa) * pi / 180;

      sinmax:= sin(max_angle); cosmax:= cos(max_angle);
      sinmin:= sin(min_angle); cosmin:= cos(min_angle);

      circleminangle:= - pi/2 + min_angle;
      circlemaxangle:= - 3*pi/2 + max_angle;

      sz:= min(3/8,3*r);

      glBegin(GL_TRIANGLE_FAN);
      glVertex3f(r*dx + r*sin(circleminangle) + sz*sinmin,
        0, r*dy + r*cos(circleminangle) + sz*cosmin);
      for i:= 0 to 37 do
        begin
          t:= circleminangle + i * (circlemaxangle - circleminangle) / 36;
          glVertex3f(r*dx + r*sin(t), 0.0, r*dy + r*cos(t));
          glVertex3f(r*dx + r*sin(circlemaxangle) + sz*sinmax,
            0, r*dy + r*cos(circlemaxangle) + sz*cosmax);
        end;
      glEnd();
    end;
  glDepthFunc(GL_LESS);
  glDisable(GL_BLEND);
end;

procedure TSimClientForm.MakeMillCone;
var
  q: PGLUquadric;
  d,r,tl: double;
  // DefaultCone: Boolean;
begin
  //DefaultCone:= Tool.toolno < 1;
  d:= ToCanonUnits(Tool.diameter);
  if d < DEFAULT_TOOL_DIA then
    d:= DEFAULT_TOOL_DIA;
  r:= d / 2;
  tl:= ToCanonUnits(Tool.zoffset);
  if tl < DEFAULT_TOOL_LENGTH then
    tl:= DEFAULT_TOOL_LENGTH;
  q:= gluNewQuadric();
  glEnable(GL_BLEND);
  SetGlColor4(GlColors.Cone);
  //if DefaultCone then
    gluCylinder(q,r/10,r,tl,12,1);
  //else
  //  gluCylinder(q,r,r,tl,12,1);
  glBegin(GL_LINES);
  glVertex3f(0,0,0);
  glVertex3f(0,0,tl + 5);
  glEnd;
  glDisable(GL_BLEND);
  gluDeleteQuadric(q);
end;

procedure TSimClientForm.MakeCone;
begin
  if not Assigned(ogl) then Exit;
  glDeleteLists(ConeL,1);
  glNewList(ConeL, GL_COMPILE);
  if Vars.IsLathe and (Tool.toolno > 0) then
    MakeLatheCone
  else
    MakeMillCone;
  glEndList;
end;

procedure TSimClientForm.DrawCone;
var
  r: double;
begin
  glPushMatrix;
  glTranslatef(ConeX,ConeY,ConeZ);
  if not Vars.IsLathe then
    begin
      if Vars.Axis[3].Geometry <> 0 then
        begin
          r:= ConeRX * Vars.Axis[3].Geometry;
          glRotatef(r, 1, 0, 0);
        end;
      if Vars.Axis[4].Geometry <> 0 then
        begin
          r:= ConeRY * Vars.Axis[4].Geometry;
          glRotatef(r, 0, 1, 0);
        end;
      if Vars.Axis[5].Geometry <> 0 then
        begin
          r:= ConeRZ * Vars.Axis[5].Geometry;
          glRotatef(r, 1,0,0);
          //writeln(FloatToStrF(r,ffFixed,6,4));
        end;
    end
  else
    glRotatef(90,0,1,0);
  glCallList(ConeL);
  glPopMatrix;
end;

procedure TSimClientForm.MakeCoords;
begin
  if not Assigned(ogl) then Exit;
  glDeleteLists(CoordsL,1);
  glNewList(CoordsL,GL_COMPILE);
  glBegin(GL_LINES);
    glColor3f(1,0,0);
    glVertex3f(0,0,0);
    glVertex3f(2,0,0);
  glEnd;
  glBegin(GL_LINES);
    glVertex3f(1.5,-0.25,0);
    glVertex3f(2,0,0);
    glVertex3f(2,0,0);
    glVertex3f(1.5,0.25,0);
  glEnd;
  glBegin(GL_LINES);
    glColor3f(0,10,0);
    glVertex3f(0,0,0);
    glVertex3f(0,2,0);
  glEnd;
  glBegin(GL_LINES);
    glVertex3f(-0.25,1.5,0);
    glVertex3f(0,2,0);
    glVertex3f(0,2,0);
    glVertex3f(0.25,1.5,0);
  glEnd;
  glBegin(GL_LINES);
    glColor3f(0,0,10);
    glVertex3f(0,0,0);
    glVertex3f(0,0,2);
  glEnd;
  glBegin(GL_LINES);
    glVertex3f(0,-0.25,1.5);
    glVertex3f(0,0,2);
    glVertex3f(0,0,2);
    glVertex3f(0,0.25,1.5);
  glEnd;
  glEndList;
end;

procedure TSimClientForm.MakeLimits;
const
  pattern = $5555;
var
  W,H,D: double;
  ML: TExtents;
begin
  if not Assigned(ogl) then Exit;
  ML:= Vars.MLimits;
  glDeleteLists(LimitsL,1);
  glNewList(LimitsL, GL_COMPILE);
  w:= ML.MaxX - ML.MinX;
  h:= ML.MaxY - ML.MinY;
  d:= ML.MaxZ - ML.MinZ;
  if (W <> 0) and (H <> 0) and (D <> 0) then
    begin
      if not View3D then
        begin
          glEnable(GL_BLEND);
          glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
          SetGlColor4(GlColors.table);
          glColor4f(0,0,0.9, 0.1);
          glBegin(GL_QUADS);
            glVertex3f(ML.minX,ML.minY,ML.minZ);
            glVertex3f(ML.maxX,ML.minY,ML.minZ);
            glVertex3f(ML.maxX,ML.maxY,ML.minZ);
            glVertex3f(ML.minX,ML.maxY,ML.minZ);
          glEnd();
          glDisable(GL_BLEND);
        end;
      if GlSettings.UseStipple then
        begin
          glEnable(GL_LINE_STIPPLE);
          glLineStipple(10, pattern);
        end;
      glBegin(GL_LINE_STRIP);
        SetGlColor3(GlColors.limits);
        glVertex3f(ML.minX,ML.minY,ML.minZ);
        glVertex3f(ML.maxX,ML.minY,ML.minZ);
        glVertex3f(ML.maxX,ML.maxY,ML.minZ);
        glVertex3f(ML.minX,ML.maxY,ML.minZ);
        glVertex3f(ML.minX,ML.minY,ML.minZ);
        glVertex3f(ML.minX,ML.minY,ML.maxZ);
        glVertex3f(ML.minX,ML.maxY,ML.maxZ);
        glVertex3f(ML.maxX,ML.maxY,ML.maxZ);
        glVertex3f(ML.maxX,ML.minY,ML.maxZ);
        glVertex3f(ML.maxX,ML.minY,ML.minZ);
      glEnd;
      glBegin(GL_LINES);
        glVertex3f(ML.minX,ML.maxY,ML.minZ);
        glVertex3f(ML.minX,ML.maxY,ML.maxZ);
      glEnd;
      glBegin(GL_LINES);
        glVertex3f(ML.maxX,ML.maxY,ML.maxZ);
        glVertex3f(ML.maxX,ML.maxY,ML.minZ);
      glEnd;
      if GlSettings.UseStipple then
        glDisable(GL_LINE_STIPPLE);
    end;
  glEndList;
end;


initialization
  {$I simclient.lrs}

end.

