unit gltools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,glu,gl,
  mocglb;

var
  DimScale,DimDist: double;

procedure Lightning(x,y,z: double);
procedure PanView(dx,dy: integer; cx,cy,cz: double; wh: integer; var px,py: double);
procedure DrawDimXLathe(ExtX: double; L: TExtents);
procedure DrawDimZLathe(ExtZ: double; L: TExtents);
procedure DrawDimX(ExtX: double; L: TExtents);
procedure DrawDimY(ExtY: double; L: TExtents);
procedure DrawDimZ(ExtZ: double; L: TExtents);

procedure MakeLimits(View3D: Boolean; var LimitsL: gluInt);
procedure MakeCoords(var CoordsL: gluInt);
procedure MakeMillCone(var ConeL: gluInt; r,tl: double);
procedure MakeLatheCone(var ConeL: gluInt; orient: integer; r,fa,ba: double);

implementation

uses
  math,glfont,gllist;

type
  Float4 = Array[0..3] of GLFloat;
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
   glMaterialfv(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE,Glf4 (1,1,1,1));
   glEnable(GL_LIGHTING);
   glEnable(GL_LIGHT0);
   glDepthFunc(GL_LESS);
   glEnable(GL_DEPTH_TEST);
   glEnable(GL_CULL_FACE);
   glDisable(GL_NORMALIZE);
end;

procedure PanView(dx,dy: integer; cx,cy,cz: double; wh: integer; var px,py: double);
var
  V: TViewPortArray;
  P,M : T16DArray;
  ObjC: TGlVector3;
  Obj: TGlVector3;
  WinX,WinY,WinZ: glDouble;
  d: glDouble;
  Scale: double;
begin
  if wh < 1 then Exit;
  glGetDoublev(GL_MODELVIEW_MATRIX, @M);
  glGetDoublev(GL_PROJECTION_MATRIX, @P);
  glGetIntegerv(GL_VIEWPORT, @V);
  ObjC[0]:= cx; ObjC[1]:= cy; ObjC[2]:= cz;
  gluProject(ObjC[0],ObjC[1],ObjC[2],M,P,V,@WinX,@WinY,@WinZ);
  gluUnProject(WinX,WinY + 0.5 * wh, WinZ,M,P,V,@Obj[0],@Obj[1],@Obj[2]);
  d:= sqrt(v3distsq(Obj,ObjC));
  scale:= abs( d / (0.5 * wh));
  px:= dx * scale;
  py:= -dy * scale;
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

procedure DrawDimXLathe(ExtX: double; L: TExtents);
var
  i: integer;
  w,tw,x,z: double;
  s: string;
begin
  if DimScale = 0 then Exit;
  // draw the X-Dimension
  if Vars.ShowMetric then
    w:= ExtX * 25.4
  else
    w:= ExtX;
  s:= PosToString(w);
  z:= L.MinZ;
  glPushMatrix;
  glBegin(GL_LINES);
  glVertex3f(L.MinX,0,z);
  glVertex3f(L.MaxX,0,z);
  glEnd;
  glBegin(GL_LINES);
  glVertex3f(L.MinX,0,z);
  glVertex3f(L.MinX,0,z-DimDist);
  glEnd;
  glBegin(GL_LINES);
  glVertex3f(L.MaxX,0,z);
  glVertex3f(L.MaxX,0,z-DimDist);
  glEnd;
  tw:= (Length(s) * GlFontDist) * DimScale;
  if tw < ExtX then
    x:= L.MinX + (ExtX / 2) - (tw / 2)
  else
    x:= L.MaxX + DimDist;
  z:= L.MinZ - (2 * DimDist);
  glTranslateF(x,0,z);
  glRotateF(90,1,0,0);
  glScaleF(DimScale,DimScale,DimScale);
  for i:= 1 to Length(s) do
    begin
      DrawGlDigit(s[i]);
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
  z:= L.MinZ - (2 * DimDist) - tw;
  x:= L.MinX - (DimDist / 2);
  glPushMatrix;
  glTranslateF(x,0,z);
  glRotateF(90,0,0,1);
  glRotateF(-90,0,1,0);
  glScaleF(DimScale,DimScale,DimScale);
  for i:= 1 to Length(s) do
    begin
      DrawGlDigit(S[i]);
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
  z:= L.MinZ - (2 * DimDist) - tw;
  x:= L.MaxX + (DimDist / 2) + DimScale;
  glPushMatrix;
  glTranslateF(x,0,z);
  glRotateF(90,0,0,1);
  glRotateF(-90,0,1,0);
  glScaleF(DimScale,DimScale,DimScale);
  for i:= 1 to Length(s) do
    begin
      DrawGlDigit(S[i]);
      glTranslateF(GlFontDist,0,0);
    end;
  glPopMatrix;
end;

procedure DrawDimZLathe(ExtZ: double; L: TExtents);
var
  i: integer;
  w,tw,x,z: double;
  s: string;
begin
  if DimScale = 0 then Exit;
  // draw the Z-Dimension
  if Vars.ShowMetric then
    w:= ExtZ * 25.4
  else
    w:= ExtZ;
  s:= PosToString(w);
  x:= L.MinX;
  glPushMatrix;
  glBegin(GL_LINES);
  glVertex3f(x,0,L.MinZ);
  glVertex3f(x,0,L.MaxZ);
  glEnd;
  glBegin(GL_LINES);
  glVertex3f(x,0,L.MinZ);
  glVertex3f(x-DimDist,0,L.MinZ);
  glEnd;
  glBegin(GL_LINES);
  glVertex3f(x,0,L.MaxZ);
  glVertex3f(x-DimDist,0,L.MaxZ);
  glEnd;
  tw:= (Length(s) * GlFontDist) * DimScale;
  if tw < ExtZ then
    z:= L.MinZ + (ExtZ / 2) - (tw / 2)
  else
    z:= L.MaxZ + DimDist;
  x:= L.MinX - (2 * DimDist);
  glTranslateF(x,0,z);
  glRotateF(90,0,0,1);
  glRotateF(-90,0,1,0);
  glScaleF(DimScale,DimScale,DimScale);
  for i:= 1 to Length(s) do
    begin
      DrawGlDigit(s[i]);
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
  z:= L.MinZ - (DimDist / 2);
  glPushMatrix;
  glTranslateF(x,0,z);
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
  glTranslateF(x,0,z);
  glRotateF(90,1,0,0);
  glScaleF(DimScale,DimScale,DimScale);
  for i:= 1 to Length(s) do
    begin
      DrawGlDigit(S[i]);
      glTranslateF(GlFontDist,0,0);
    end;
  glPopMatrix;
end;


procedure DrawDimX(ExtX: double; L: TExtents);
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
  if Vars.IsLathe then
    begin
      glRotateF(90,1,0,0);
    end;
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
  if Vars.IsLathe then
    begin
      glRotateF(90,1,0,0);
    end;
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
  if Vars.IsLathe then
    begin
      glRotateF(90,1,0,0);
    end;
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

procedure DrawDimY(ExtY: double; L: TExtents);
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

procedure DrawDimZ(ExtZ: double; L: TExtents);
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

procedure MakeLimits(View3D: Boolean; var LimitsL: gluInt);
const
  pattern = $5555;
var
  W,H,D: double;
  ML: TExtents;
begin
  ML:= Vars.MLimits;
  w:= ML.MaxX - ML.MinX;
  h:= ML.MaxY - ML.MinY;
  d:= ML.MaxZ - ML.MinZ;
  if LimitsL <> 0 then glDeleteLists(LimitsL, 1);
  LimitsL:= glGenLists(1);
  glNewList(LimitsL, GL_COMPILE);
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

procedure MakeCoords(var CoordsL: gluInt);
begin
  if CoordsL <> 0 then glDeleteLists(CoordsL,1);
  CoordsL:= glGenLists(1);
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
  if not Vars.IsLathe then
    begin
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
  end;
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

procedure MakeMillCone(var ConeL: gluInt; r,tl: double);
var
  q: PGLUquadric;
begin
  if ConeL <> 0 then
    glDeleteLists(ConeL,1);
  ConeL:= glGenLists(1);
  glNewList(ConeL, GL_COMPILE);
  q:= gluNewQuadric();
  glEnable(GL_BLEND);
  SetGlColor4(GlColors.Cone);
  gluCylinder(q,r/10,r,tl,12,1);
  glBegin(GL_LINES);
  glVertex3f(0,0,0);
  glVertex3f(0,0,tl + 5);
  glEnd;
  glDisable(GL_BLEND);
  gluDeleteQuadric(q);
  glEndList;
end;

procedure MakeLatheCone(var ConeL: gluInt; orient: integer; r,fa,ba: double);
const
  lathe_shapes : Array[1..9] of TPoint =
    ((x:1;y:-1),(x:1;y:1),(x:-1;y:1),(x:-1;y:-1),(x:0;y:-1),
     (x:1;y:0),(x:0;y:1),(x:-1;y:0),(x:0;y:0));
var
  t: double;
  i,dx,dy: integer;
  min_angle,max_angle: double;
  sinmax,sinmin,cosmax,cosmin: double;
  circleminangle,circlemaxangle: double;
  sz: double;
begin
  if ConeL <> 0 then
    glDeleteLists(ConeL,1);
  ConeL:= glGenLists(1);
  glNewList(ConeL, GL_COMPILE);
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

      sz:= max(3/8,3*r);

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
  glEndList;
end;



end.

