unit simclient;

{$I mocca.inc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Buttons, ExtCtrls,gllist,glu,gl,
  mocglb,
  {$IFNDEF OWNGL}
  OpenGlContext;
  {$ELSE}
  glcontext;
  {$ENDIF}

type

  { TSimClientForm }
  TSimClientForm = class(TForm)
    sbH: TScrollBar;
    sbV: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
    ogl: TGlControl;
    {$ENDIF}
    FShowLivePlot: Boolean;
    RotationX,RotationY,RotationZ: double;
    PanX,PanY,PanZ: double;
    CenterX,CenterY,CenterZ: double;
    EyeX,EyeY,EyeZ: Double;
    ConeX,ConeY,ConeZ: Double;
    ExtX,ExtY,ExtZ: double;
    L: TExtents;
    LimitIsPart: Boolean;
    TextScale: Double;
    TextWidth: Double;
    Offset: tlo;
    ConeL,LimitsL,CoordsL,ListL: gluInt;
    MouseX,MouseY: integer;
    Moving: Boolean;
    FViewMode: integer;
    ToolRad,ToolLen: double;
    AreaInitialized: Boolean;
    ZoomMax: integer;
    FFileName: string;
    FUnitCode: string;
    FInitCode: string;

    procedure MakeLimits;
    procedure MakeCoords;
    procedure MakeCone;
    procedure MakeList;
    procedure Pan(DX,DY: integer);
    procedure UpdateView;
    procedure UpdatePart;
    procedure DrawPart;
    procedure MoveCone(X,Y,Z: Double);
    procedure ResetView;
    procedure RotateZ(Angle: integer);
    procedure RotateX(Angle: integer);
    procedure SetViewMode(AMode: integer);
  public
    procedure LoadFile(FileName,UnitCode,InitCode: string);
    procedure ReloadFile;
    procedure ClearFile;
    procedure SetTool(ToolNo: integer);
    procedure Zoom(ADir: Integer);
    procedure ClearPlot;
    procedure UpdateSelf;
    property  ShowLivePlot: Boolean read FShowLivePlot write FShowLivePlot;
    property  ViewMode: integer read FViewMode write SetViewMode;
  end;
  
var
  clSim: TSimClientForm;

implementation

uses
  mocjoints,emc2pas,glcanon,
  logger, glfont;

const
  InitialRotX = -60; InitialRotY = 0; InitialRotZ = 0;

procedure SetGlColor3(const c: TGlColorItem);
begin
  glColor3f(c.r,c.g,c.b);
end;

procedure SetGlColor4(const c: TGlColorItem);
begin
  glColor4f(c.r,c.g,c.b,c.a);
end;

procedure TSimClientForm.ClearPlot;
begin
  LoggerClear;
  if Assigned(ogl) then
    ogl.Invalidate;
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
  Dist: Double;
begin
  if (not Assigned(MyGlList)) then
    raise Exception.Create('cannot show preview without a list');
  if FFileName <> '' then
    begin
      Error:= ParseGCode(FFileName,FUnitCode,FInitCode);
      if Error <> 0 then
        LastError:= GetGCodeError(Error);
      end;
  CanonInitOffsets;
  UpdateView;
  Ogl.Invalidate;
end;

procedure TSimClientForm.ClearFile;
begin
  if Assigned(MyGlList) then
    MyGlList.Clear;
  FFileName:= '';
  UpdateView;
  Ogl.Invalidate;
 end;

procedure TSimClientForm.UpdateSelf;
var
  ix,iy,iz: integer;
  X,Y,Z: double;
begin
  if Visible and AreaInitialized then
    if Assigned(Joints) then
      begin
        ix:= Joints.AxisByChar('X');
        iy:= Joints.AxisByChar('Y');
        iz:= Joints.AxisByChar('Z');
        if ix >= 0 then X:= GetRelPos(ix) else Exit;
        if iy >= 0 then Y:= GetRelPos(iy) else Exit;
        if iz >= 0 then Z:= GetRelPos(iz) else Exit;
        MoveCone(x,y,z);
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
  // ReadStyle(Self);
  AreaInitialized:= False;
  FViewMode:= 0;
  ConeL:= 1;
  LimitsL:= 2;
  CoordsL:= 3;
  ListL:= 4;
  ToolRad:= 0.2;
  ToolLen:= 0.5;
  ZoomMax:= 10;
  FShowLivePlot:= True;
  FFileName:= '';
  sbV.setParams(InitialRotX,-180,180,1);
  sbH.SetParams(InitialRotZ,-180,180,1);
  if not Assigned(MyGlList) then
    MyGlList:= TGlList.Create;
  if not Assigned(MyGlList) then
    RaiseError('could not create gllist');
  {$IFNDEF OWNGL}
  ogl:= TOpenGlControl.Create(Self);
  ogl.AutoResizeViewPort:= True;
  ogl.RGBA:= GlSettings.UseRGBA;
  ogl.DoubleBuffered:= GlSettings.UseDoubleBuffered;
  {$ELSE}
  GlRGBA:= GlSettings.UseRGBA;
  GlDirect:= GlSettings.UseDirect;
  GlDoubleBuffered:= GlSettings.UseDoubleBuffered;
  ogl:= TGlControl.Create(Self);
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
  CanonInitOffsets;
  GetCanonOffset(Offset);
end;

procedure TSimClientForm.FormDestroy(Sender: TObject);
begin
  if Assigned(ogl) then
    FreeAndNil(ogl);
  if Assigned(MyGlList) then
    FreeAndNil(MyGlList);
end;

procedure TSimClientForm.FormResize(Sender: TObject);
begin
  if (sbV.Height > 0) and (sbH.Width > 0) then
    Ogl.SetBounds(sbH.Left,sbV.Top,sbH.Width,sbV.Height);
  OglResize(nil);
end;

procedure TSimClientForm.FormShow(Sender: TObject);
begin
  FormResize(nil);
  UpdateView;
end;

procedure TSimClientForm.RotateZ(Angle: integer);
begin
  if Round(RotationZ) <> Angle then
    begin
      RotationZ:= Angle;
      Ogl.Invalidate;
    end;
end;

procedure TSimClientForm.RotateX(Angle: integer);
begin
  if Round(RotationX) <> Angle then
    begin
      RotationX:= Angle;
      Ogl.Invalidate;
    end;
end;

procedure TSimClientForm.SetViewMode(AMode: integer);
begin
  if (not Assigned(ogl)) or (not AreaInitialized) then
    Exit;
  if AMode <> FViewMode then
    begin
      if (AMode < 0) or (AMode > 3) then
        Exit;
      FViewMode:= AMode;
      ResetView;
      if AMode = 0 then
        begin
          rotationX:= -45;
          rotationY:= 0;
          rotationZ:= 0;
        end
      else
      if AMode = 1 then
        begin
          rotationX:= 0;
          rotationY:= 0;
          rotationZ:= 0;
        end
      else
      if AMode = 2 then
        begin
          rotationX:= -90;
          rotationY:= 0;
          rotationZ:= 0;
        end;
      if AMode = 3 then
        begin
          rotationX:= -90;
          rotationY:= 0;
          rotationZ:= -90;
        end;
      sbV.setParams(Round(RotationX),-180,180);
      sbH.SetParams(Round(RotationZ),-180,180);
      Ogl.Invalidate;
    end;
end;


type
  TGlVector3 = array[0..2] of glDouble;

function v3distsq(a,b: TGlVector3): glDouble;
var
  d: TglVector3;
begin
  d[0]:= a[0] - b[0];
  d[1]:= a[1] - b[1];
  d[2]:= a[2] - b[2];
  Result:= d[0]*d[0] + d[1]*d[1] + d[2]*d[2]
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

procedure TSimClientForm.MoveCone(x,y,z: Double);
var
  cx,cy,cz: Double;

begin
  if not AreaInitialized then Exit;
  cx:= ToCanonPos(x,0);
  cy:= ToCanonPos(y,1);
  cz:= ToCanonPos(z,2);
  if (cx <> ConeX) or (cy <> ConeY) or (cz <> ConeZ) then
    begin
      ConeX:= cx; ConeY:= cy; ConeZ:= cz;
      if FShowLivePlot then
        LoggerAddPoint(cx,cy,cz);
      ogl.Invalidate;
    end;
end;

procedure TSimClientForm.UpdatePart;
var
  ExtMax: Double;
begin
  if not LimitIsPart then Exit;
  ExtMax:= 1;
  if ExtX > ExtMax then ExtMax:= ExtX;
  if ExtY > ExtMax then ExtMax:= ExtY;
  if ExtZ > ExtMax then ExtMax:= ExtZ;
  TextScale:= ExtMax / 25;
  TextWidth:= 6 * TextScale;
end;

procedure TSimClientForm.DrawPart;
var
  x,y,z: Double;
  DispX,DispY,DispZ: Double;
begin
  if not LimitIsPart then Exit;
  if Vars.Metric then
    begin
      DispX:= ExtX * 25.4;
      DispY:= ExtY * 25.4;
      DispZ:= ExtZ * 25.4;
    end
  else
    begin
      DispX:= ExtX;
      DispY:= ExtY;
      DispZ:= ExtZ;
    end;
  // X-Coord
  x:= L.MinX + (ExtX / 2) - (TextWidth / 2);
  y:= L.MinY - (1.2 * TextScale);
  z:= L.MinZ;
  DrawGlText(x,y,z,glfX,TextScale,PosToString(DispX));
  // Y-Coord
  x:= L.MinX + (ExtX / 2) - (TextWidth / 2);
  y:= - L.MinY; //  - (1.2 * TextScale);
  z:= L.MinZ;
  DrawGlText(x,y,z,glfY,TextScale,PosToString(DispY));
  // Z-Coord
  x:= L.MinX - TextWidth - 1; //  - (TextWidth / 2);
  y:= L.MinZ + (ExtZ / 2); //- L.MinY; //  - (1.2 * TextScale);
  z:= - (L.MinY + (ExtY / 2)); //  (ExtZ / 2);
  DrawGlText(x,y,z,glfZ,TextScale,PosToString(DispZ));
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
  LimitIsPart:= False;
  if (Assigned(MyGlList)) and (FFileName <> '') then
    begin
      MyGlList.GetExtents(L);
      LimitIsPart:= True;
    end
  else
    L:= Vars.MLimits;
  CalcExtents;
  if (ExtX < 0.0001) and (ExtY < 0.0001) and (ExtZ < 0.0001) then
    begin
      Writeln('Preview is too small. Using Machine Limits...');
      L:= Vars.MLimits;
      CalcExtents;
      LimitIsPart:= False;
    end;
  GetCanonOffset(Offset);
  UpdatePart;
  ResetView;
end;

procedure TSimClientForm.ResetView;
begin
  Centerx:= L.maxX - (ExtX / 2); //  + offset.x;
  Centery:= L.maxY - (ExtY / 2); // + offset.y;
  Centerz:= L.maxZ - (ExtZ / 2); // + offset.z;
  writeln(Format('%s %f %f %f',['Center: ',CenterX,CenterY,CenterZ]));
  writeln(Format('%s %f %f %f',['Extents: ',ExtX,ExtY,ExtZ]));
  PanX:= 0; PanY:= 0; PanZ:= 0; EyeX:= 0; EyeY:= 0;
  if FViewMode = 0 then
    begin
      RotationX:= InitialRotX;
      RotationY:= InitialRotY;
      RotationZ:= InitialRotZ;
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

  if Ogl.MakeCurrent then
    begin
      MakeCone;
      MakeCoords;
      MakeLimits;
      MakeList;
    end;
  Ogl.Invalidate;
end;

procedure TSimClientForm.MakeList;
var
  P: PListItem;
begin
  if not Assigned(ogl) then Exit;
  glDeleteLists(ListL,1);
  glNewList(ListL,GL_COMPILE);
  if Assigned(MyGlList) then
    begin
      MyGlList.First;
      P:= MyGlList.Get;
      glLineWidth(3);
      while (P <> nil) do
        begin
          if (P^.ltype = ltFeed) or (P^.ltype = ltArcFeed) then
            begin
              glBegin(GL_LINES);
              SetGlColor3(GlColors.feed);
              // with GlSettings.clFeed do glColor3f(r,g,b);
              glVertex3f(P^.l1.x,P^.l1.y,P^.l1.z);
              glVertex3f(P^.l2.x,P^.l2.y,P^.l2.z);
              glEnd();
            end;
          P:= MyGlList.Get;
        end;
      MyGlList.First;
      P:= MyGlList.Get;
      glLineWidth(1);
      while (P <> nil) do
        begin
          if (P^.ltype = ltTraverse) or (P^.ltype = ltDwell) then
            begin
              glBegin(GL_LINES);
              SetGlColor3(GlColors.traverse);
              //with GlSettings.clTraverse do glColor3f(r,g,b);
              glVertex3f(P^.l1.x,P^.l1.y,P^.l1.z);
              glVertex3f(P^.l2.x,P^.l2.y,P^.l2.z);
              glEnd();
            end;
          P:= MyGlList.Get;
        end;
    end;
  glLineWidth(1);
  glEndList;
end;

procedure TSimClientForm.OglPaint(sender: TObject);

const GLInitialized: boolean = false;
var
  k,n: double;

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
  if sender = nil then ;
  if not AreaInitialized then
    begin
      InitGL;
      AreaInitialized:=true;
    end;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  if FViewMode = 0 then
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
  glRotatef(RotationX,1.0,0.0,0.0);
  glRotatef(RotationY,0.0,1.0,0.0);
  glRotatef(RotationZ,0.0,0.0,1.0);
  glTranslatef(-centerx,-centery,-centerz);
  glCallList(LimitsL);
  glPushMatrix;
  glTranslatef(offset.x,offset.y,offset.z);
  glCallList(CoordsL);
  glPopMatrix;
  glCallList(ListL);
  glPushMatrix;
  glTranslatef(ConeX,ConeY,ConeZ);
  glCallList(ConeL);
  glPopMatrix;
  SetGlColor3(GlColors.toolpath);
  if FShowLivePlot then
    LoggerCall;
  DrawPart;
  //DrawText(1,0,0,0.05,'0123456789');
  {$IFDEF LINESMOOTH}
  glDisable(GL_LINE_SMOOTH);
  {$ENDIF}
  ogl.SwapBuffers;
end;

procedure TSimClientForm.OglMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(ogl) then
    if Assigned(MyGlList) then
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
        // UpdateLabels;
        // SbZoom.Position:= Round(EyeZ);
        ogl.Invalidate;
      end;
end;

procedure TSimClientForm.OglResize(Sender: TObject);
begin
  if Assigned(ogl) then
    if ogl.Height > 0 then
      if AreaInitialized then
        glViewport(0,0,ogl.Width,ogl.Height);
      //else
      //  ogl.MakeCurrent;
end;

procedure TSimClientForm.SetTool(ToolNo: integer);
begin
  if not Assigned(ogl) then Exit;
  ToolRad:= GetToolDiameter(ToolNo) / 2;
  ToolLen:= GetToolLength(ToolNo);
  MakeCone;
end;

procedure TSimClientForm.MakeCone;
var
  q: PGLUquadric;
begin
  if not Assigned(ogl) then Exit;
  q := gluNewQuadric();
  glDeleteLists(ConeL,1);
  glNewList(ConeL, GL_COMPILE);
  glEnable (GL_BLEND);
  //glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  SetGlColor4(GlColors.cone);
  // glColor4f(r,g,b,0.5);
  gluCylinder(q,ToolRad/10,ToolRad,ToolLen,12,1);
  glBegin(GL_LINES);
    glVertex3f(0,0,0);
    glVertex3f(0,0,ToolLen + 5);
  glEnd;
  glEndList;
  gluDeleteQuadric(q);
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
      glEnable (GL_BLEND);
      glBlendFunc (GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      SetGlColor4(GlColors.table);
      glColor4f(0,0,0.9, 0.1);
      glBegin(GL_QUADS);
        glVertex3f(ML.minX,ML.minY,ML.minZ);
        glVertex3f(ML.maxX,ML.minY,ML.minZ);
        glVertex3f(ML.maxX,ML.maxY,ML.minZ);
        glVertex3f(ML.minX,ML.maxY,ML.minZ);
      glEnd();

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

