unit glview;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, OpenGLContext, SysUtils,
  gllist,gl,glu;

type
  TGlVector3 = array[0..2] of glDouble;
  
type
  TGlView = class
    constructor Create(AParent,AOwner: TWinControl);
    destructor Destroy; override;
    procedure Paint(Sender: TObject);
    procedure Resize(Sender: TObject);
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    procedure MakeLimits;
    procedure MakeCoords;
    procedure MakeCone;

    procedure Pan(DX,DY: integer);
    procedure DrawCone;

  private

    RotationX: double;
    RotationY: double;
    RotationZ: double;

    PanX: double;
    PanY: double;
    PanZ: double;

    CenterX,CenterY,CenterZ: double;
    EyeX,EyeY,EyeZ: Double;
    ConeX,ConeY,ConeZ: Double;

    LimW: double;
    LimH: double;
    LimD: double;

    ogl: TOpenGlControl;

    AreaInitialized: Boolean;

    L, ML, DL: TExtents;  // L= limits  ML= machine limits  DL= drawing limits

    ConeL: integer;
    LimitsL: integer;
    CoordsL: integer;
    ListL: integer;

    MouseX: integer;
    MouseY: integer;
    Moving: Boolean;

  public
    procedure GetLimits(var E: TExtents);
    procedure SetMachineLimits(E: TExtents);
    procedure UpdateView;
    procedure Invalidate;
    procedure MakeList;
    procedure MoveCone(X,Y,Z: Double);
    procedure SetBounds(X,Y,W,H: Integer);
    procedure ResetView;
    procedure RotateZ(Angle: integer);
    procedure RotateX(Angle: integer);
  end;
  
var
  MyGlView: TGlView;
  
const
  InitialRotX = -60;
  InitialRotY = 0;
  InitialRotZ = 0;

implementation

uses
  glCanon;

constructor TGlView.Create(AParent,AOwner: TWinControl);
begin
  AreaInitialized:= False;
  ogl:= TOpenGlControl.Create(AOwner);
  ogl.Parent:= AParent;
  ogl.SetBounds(0,0,500,500);
  ogl.DoubleBuffered:= True;
  ogl.AutoResizeViewPort:= False;
  ogl.OnPaint:= @self.Paint;
  ogl.OnResize:= @self.Resize;
  ogl.OnMouseWheel:= @self.MouseWheel;
  ogl.OnMouseDown:= @self.MouseDown;
  ogl.OnMouseMove:= @self.MouseMove;
  ogl.OnMouseUp:= @self.MouseUp;
end;

destructor TGlView.Destroy;
begin
  writeln('glview destroy');
  if Assigned(ogl) then ogl.Free;
  inherited;
end;

procedure TglView.RotateZ(Angle: integer);
begin
  if Round(RotationZ) <> Angle then
    begin
      RotationZ:= Angle;
      Invalidate;
    end;
end;

procedure TglView.RotateX(Angle: integer);
begin
  if Round(RotationX) <> Angle then
    begin
      RotationX:= Angle;
      Invalidate;
    end;
end;

function v3distsq(a,b: TGlVector3): glDouble;
var
  d: TglVector3;
begin
  d[0]:= a[0] - b[0];
  d[1]:= a[1] - b[1];
  d[2]:= a[2] - b[2];
  Result:= d[0]*d[0] + d[1]*d[1] + d[2]*d[2]
end;

procedure TGlView.Pan(dx,dy: integer);
var
  V: array[0..3] of glInt;
  P,M : array[0..15] of glDouble;
  ObjC: TGlVector3;
  Obj: TGlVector3;
  WinX,WinY,WinZ: glDouble;
  WinHeight: integer;
  d: glDouble;
  Scale: double;
begin
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

procedure TGlView.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseX:= X;
  MouseY:= Y;
  PanX:= 0;
  PanY:= 0;
  Moving:= True;
end;

procedure TGlView.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Dx,Dy: integer;
begin
  if Moving then
    begin
      dx:= MouseX - X;
      dy:= MouseY - Y;
      Pan(dx,dy);
      ogl.Invalidate;
    end;
end;

procedure TGlView.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Moving:= False;
  MouseX:= 0;
  MouseY:= 0;
  CenterX:= CenterX + PanX;
  CenterY:= CenterY + PanY;
  EyeX:= EyeX + PanX;
  EyeY:= EyeY + PanY;
  PanX:= 0;
  PanY:= 0;
end;

procedure TGlView.DrawCone;
begin
  glPushMatrix;
  glTranslatef(ConeX,ConeY,ConeZ);
  glCallList(ConeL);
  glPopMatrix;
end;

procedure TGlView.MoveCone(x,y,z: Double);
var
  cx,cy,cz: Double;
begin
  cx:= ToInternalUnits(x);
  cy:= ToInternalUnits(y);
  cz:= ToInternalUnits(z);
  if (cx <> ConeX) or (cy <> ConeY) or (cz <> ConeZ) then
    begin
      ConeX:= cx; ConeY:= cy; ConeZ:= cz;
      ogl.Invalidate;
    end;
end;

procedure TGlView.SetBounds(X,Y,W,H: Integer);
begin
  if Assigned(ogl) then
    ogl.SetBounds(X,Y,W,H);
end;

procedure TGlView.GetLimits(var E: TExtents);
begin
  E:= L;
end;

procedure TglView.Invalidate;
begin
  if Assigned(ogl) then
    if Assigned(MyGlList) then
      ogl.Invalidate;
end;

procedure TGlView.UpdateView;
var
  DW,DH,DD: Double;
begin
  with L do
    begin
      MinX:= 0; MaxX:= 0;
      MinY:= 0; MaxY:= 0;
      MinZ:= 0; MaxZ:= 0;
    end;

  with DL do
    begin
      MinX:= 0; MaxX:= 0;
      MinY:= 0; MaxY:= 0;
      MinZ:= 0; MaxZ:= 0;
    end;
    
  if Assigned(MyGlList) then
    MyGlList.GetExtents(DL);

  DW:= (DL.maxX - DL.minX);
  DH:= (DL.maxY - DL.minY);
  DD:= (DL.maxZ - DL.minZ);

  if (DW <> 0) and (DH <> 0) and (DD <> 0) then
    begin
      L:= DL;
    end
  else
    begin
      if DL.MaxX > ML.MaxX then L.MaxX:= DL.MaxX else L.MaxX:= ML.MaxX;
      if DL.MinX < ML.MinX then L.MinX:= DL.MinX else L.MinX:= ML.MinX;
      if DL.MaxY > ML.MaxY then L.MaxY:= DL.MaxY else L.MaxY:= ML.MaxY;
      if DL.MinY < ML.MinY then L.MinY:= DL.MinY else L.MinY:= ML.MinY;
      if DL.MaxZ > ML.MaxZ then L.MaxZ:= DL.MaxZ else L.MaxZ:= ML.MaxZ;
      if DL.MinZ < ML.MinZ then L.MinZ:= DL.MinZ else L.MinZ:= ML.MinZ;
    end;

  LimW:= (L.maxX - L.minX);
  LimH:= (L.maxY - L.minY);
  LimD:= (L.maxZ - L.minZ);

  ResetView;
  
  Resize(nil);

  ogl.MakeCurrent;
  if Assigned(MyGlList) then
    MakeList;
  Invalidate;
end;

procedure TGlView.ResetView;
begin

  Centerx:= L.maxX - (limW / 2);
  Centery:= L.maxY - (limH / 2);
  Centerz:= L.maxZ - (limD / 2);

  PanX:= 0;
  PanY:= 0;
  PanZ:= 0;

  EyeX:= CenterX;
  EyeY:= CenterY;
  
  RotationX:= InitialRotX;
  RotationY:= InitialRotY;
  RotationZ:= InitialRotZ;

  if limW < limH then
    EyeZ:= CenterZ + LimH
  else
    EyeZ:= CenterZ + LimW;
  AreaInitialized:= False;
end;


procedure TGlView.SetMachineLimits(E: TExtents);
begin
  ML:= E;
  UpdateView;
end;

procedure TGlView.MakeList;
var
  P: PListItem;
begin
  ListL:= glGenLists(4);
  glNewList(ListL, GL_COMPILE);
  if Assigned(MyGlList) then
    begin
      MyGlList.First;
      P:= MyGlList.Get;
      while (P <> nil) do
        begin
          glBegin(GL_LINES);
          if (P^.ltype = ltFeed) or (P^.ltype = ltArcFeed) then
            glColor3f(1,1,1)
          else
            glColor3f(0.5,0.5,0.5);
          glVertex3f(P^.l1.x,P^.l1.y,P^.l1.z);
          glVertex3f(P^.l2.x,P^.l2.y,P^.l2.z);
          glEnd;
          P:= MyGlList.Get;
        end;
    end;
  glEndList;
end;

procedure TGlView.Paint(sender: TObject);

const GLInitialized: boolean = false;

procedure InitGL;
begin
  if GLInitialized then Exit;
  GLInitialized:= True;
  glClearColor(0,0,0,1);
  glClearDepth(1.0);
  EyeX:= CenterX;
  EyeY:= CenterY;
  MakeCone;
  MakeCoords;
  MakeLimits;
  MakeList;
end;

begin
  if not ogl.MakeCurrent then Exit;
  if not AreaInitialized then
    begin
      InitGL;
      glMatrixMode (GL_PROJECTION);
      glLoadIdentity;
      gluPerspective(60,ogl.Width/ogl.Height,0.1,LimD * 100);
      glMatrixMode (GL_MODELVIEW);
      glViewport (0,0,ogl.Width,ogl.Height);
      AreaInitialized:=true;
    end;
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;

  gluLookAt(EyeX+PanX,EyeY+PanY,EyeZ+centerZ+PanZ,centerX+PanX,centerY+PanY,centerZ+PanZ,0,1,0);
  glRotatef(RotationX,1.0,0.0,0.0);
  glRotatef(RotationY,0.0,1.0,0.0);
  glRotatef(RotationZ,0.0,0.0,1.0);

   //glPushMatrix;
   // glTranslatef (-centerx + PanX,-centery + PanY ,-centerz + PanZ);
   //glPopMatrix;

  DrawCone;
  glCallList(CoordsL);
  glCallList(LimitsL);
  glCallList(ListL);
  ogl.SwapBuffers;
end;

procedure TGlView.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(ogl) then
    if Assigned(MyGlList) then
      begin
        if WheelDelta < 0 then
          EyeZ:= EyeZ / 1.2
        else
          EyeZ:= EyeZ * 1.2;
        Handled:= True;
        ogl.Invalidate;
      end;
end;

procedure TGlView.Resize(Sender: TObject);
begin
 if (AreaInitialized) and ogl.MakeCurrent then
   glViewport(0,0,ogl.Width,ogl.Height);
end;

procedure TGlView.MakeCone;
var
  q: PGLUquadric;
begin
  q := gluNewQuadric();
  ConeL:= glGenLists(1);
  glNewList(ConeL, GL_COMPILE);
  glColor3f(1,0.1,0.1);
  // gluPartialDisk(q, 1, 2, 12, 4, 0, 310);
  gluCylinder(q, 0,0.2,0.5,12,1);
  glEndList;
  gluDeleteQuadric(q);
end;

procedure TGlView.MakeCoords;
begin
  CoordsL:= glGenLists(2);
  glNewList(CoordsL, GL_COMPILE);
  glBegin(GL_LINES);
    glColor3f(1,0,0);
    glVertex3f(0,0,0);
    glVertex3f(10,0,0);
  glEnd;
  glBegin(GL_LINES);
    glColor3f(0,10,0);
    glVertex3f(0,0,0);
    glVertex3f(0,10,0);
  glEnd;
  glBegin(GL_LINES);
    glColor3f(0,0,10);
    glVertex3f(0,0,0);
    glVertex3f(0,0,10);
  glEnd;
  glEndList;
end;

procedure TGlView.MakeLimits;
const
  pattern = $5555;
begin
  //Stippling aktivieren
  LimitsL:= glGenLists(3);
  glNewList(LimitsL, GL_COMPILE);
  glEnable(GL_LINE_STIPPLE);
  glLineStipple(10, pattern);
  glBegin(GL_LINE_STRIP);
    glColor3f(0.7,0.1,0.1);
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
    //glVertex3f(ML.minX,ML.maxY,ML.maxZ);
    //glVertex3f(ML.maxX,ML.minY,ML.maxZ);
    //glVertex3f(ML.minX,ML.minY,ML.maxZ);
  glDisable(GL_LINE_STIPPLE);
  glEndList;
end;

end.

