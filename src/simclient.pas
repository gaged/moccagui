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
    BtnCLog: TButton;
    LabelZoom: TLabel;
    LabelL1: TLabel;
    sbH: TScrollBar;
    sbV: TScrollBar;
    BtnP: TSpeedButton;
    BtnX: TSpeedButton;
    BtnY: TSpeedButton;
    BtnZ: TSpeedButton;
    SbZoom: TScrollBar;
    procedure BtnClearClick(Sender: TObject);
    procedure BtnPClick(Sender: TObject);
    procedure BtnXClick(Sender: TObject);
    procedure BtnYClick(Sender: TObject);
    procedure BtnZClick(Sender: TObject);
    procedure BtnCLogClick(Sender: TObject);
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
    procedure SbZoomChange(Sender: TObject);
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
    LimX,LimY,LimZ: double;
    L: TExtents;
    Offset: tlo;
    ConeL,LimitsL,CoordsL,ListL: gluInt;
    MouseX,MouseY: integer;
    Moving: Boolean;
    Mode: integer;
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
    procedure UpdateLabels;
    procedure MoveCone(X,Y,Z: Double);
    procedure ResetView;
    procedure RotateZ(Angle: integer);
    procedure RotateX(Angle: integer);
    procedure ViewMode(AMode: integer);
  public
    procedure LoadFile(FileName,UnitCode,InitCode: string);
    procedure ReloadFile;
    procedure ClearFile;
    procedure SetTool(ToolNo: integer);
    procedure UpdateSelf;
    property  ShowLivePlot: Boolean read FShowLivePlot write FShowLivePlot;
  end;
  
var
  clSim: TSimClientForm;

implementation

uses
  mocjoints,emc2pas,glcanon,
  logger;

const
  InitialRotX = -60; InitialRotY = 0; InitialRotZ = 0;


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

procedure TSimClientForm.UpdateLabels;
begin
  LabelZoom.Caption:= FloatToStr(EyeZ);
end;

procedure TSimClientForm.UpdateSelf;
var
  ix,iy,iz: integer;
  X,Y,Z: double;
begin
  if Visible and FShowLivePlot and AreaInitialized then
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
  AreaInitialized:= False;
  Mode:= 0;
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
  ogl.AutoResizeViewPort:= true;
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

procedure TSimClientForm.BtnPClick(Sender: TObject);
begin
  if AreaInitialized then ViewMode(0);
end;

procedure TSimClientForm.BtnClearClick(Sender: TObject);
begin
  LoggerClear;
  if Assigned(ogl) then
    ogl.Invalidate;
end;

procedure TSimClientForm.BtnXClick(Sender: TObject);
begin
  if AreaInitialized then ViewMode(1);
end;

procedure TSimClientForm.BtnYClick(Sender: TObject);
begin
  if AreaInitialized then ViewMode(2);
end;

procedure TSimClientForm.BtnZClick(Sender: TObject);
begin
  if AreaInitialized then ViewMode(3);
end;

procedure TSimClientForm.BtnCLogClick(Sender: TObject);
begin

end;

procedure TSimClientForm.FormDestroy(Sender: TObject);
begin
  if Assigned(ogl) then
    FreeAndNil(ogl);
  if Assigned(MyGlList) then
    FreeAndNil(MyGlList);
end;

procedure TSimClientForm.FormResize(Sender: TObject);
var
  CW,CH: integer;
begin
  if (sbV.Height > 0) and (sbH.Width > 0) then
    Ogl.SetBounds(sbH.Left,sbV.Top,sbH.Width,sbV.Height);
end;

procedure TSimClientForm.FormShow(Sender: TObject);
begin
  UpdateView;
  AreaInitialized:= False;
  Ogl.Invalidate;
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

procedure TSimClientForm.ViewMode(AMode: integer);
begin
  if (not Assigned(ogl)) or (not AreaInitialized) then
    Exit;
  if AMode <> Mode then
    begin
      Mode:= AMode;
      ResetView;
      if Mode = 0 then
        begin
          rotationX:= -45;
          rotationY:= 0;
          rotationZ:= 0;
        end
      else
      if Mode = 1 then
        begin
          rotationX:= 0;
          rotationY:= 0;
          rotationZ:= 0;
        end
      else
      if Mode = 2 then
        begin
          rotationX:= -90;
          rotationY:= 0;
          rotationZ:= 0;
        end;
      if Mode = 3 then
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

procedure TSimClientForm.SbZoomChange(Sender: TObject);
begin
  if Assigned(Ogl) and AreaInitialized then
    begin
      EyeZ:= SbZoom.Position;
      UpdateLabels;
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
      LoggerAddPoint(cx,cy,cz);
      ogl.Invalidate;
    end;
end;

procedure TSimClientForm.UpdateView;

  procedure CalcExtents;
  begin
    LimX:= (L.maxX - L.minX);
    LimY:= (L.maxY - L.minY);
    LimZ:= (L.maxZ - L.minZ);
  end;

begin
  L:= SetExtents(0,0,0,0,0,0);
  if (Assigned(MyGlList)) and (FFileName <> '') then
    MyGlList.GetExtents(L)
  else
    L:= Vars.MLimits;
  CalcExtents;
  if (LimX < 0.0001) and (LimY < 0.0001) and (LimZ < 0.0001) then
    begin
      L:= Vars.MLimits;
      CalcExtents;
    end;
  GetCanonOffset(Offset);
  ResetView;
end;

procedure TSimClientForm.ResetView;
begin
  Centerx:= L.maxX - (limX / 2); //  + offset.x;
  Centery:= L.maxY - (limY / 2); // + offset.y;
  Centerz:= L.maxZ - (limZ / 2); // + offset.z;
  //writeln(Format('%s %f %f %f',['Center: ',CenterX,CenterY,CenterZ]));
  //writeln(Format('%s %f %f %f',['Extents: ',LimX,LimY,LimZ]));
  PanX:= 0; PanY:= 0; PanZ:= 0; EyeX:= 0; EyeY:= 0;
  if Mode = 0 then
    begin
      RotationX:= InitialRotX;
      RotationY:= InitialRotY;
      RotationZ:= InitialRotZ;
    end;
  if Mode = 0 then
    begin
      if limX < limY then EyeZ:= (CenterZ + LimY) * 1.2
        else EyeZ:= (CenterZ + LimX) * 1.2;
      ZoomMax:= Round(EyeZ*10);
    end
  else
    begin
      if limX < limY then EyeZ:= CenterZ + Sqr(LimY)
        else EyeZ:= CenterZ + Sqr(LimX);
      ZoomMax:= Round(EyeZ*2);
    end;
  if ZoomMax > 10000 then ZoomMax:= 10000 else
    if ZoomMax < 10 then ZoomMax:= 10;
  if Round(EyeZ) < 1 then EyeZ:= 1;
  SbZoom.SetParams(Round(EyeZ),1,ZoomMax,1);

  if Ogl.MakeCurrent then
    begin
      MakeCone;
      MakeCoords;
      MakeLimits;
      MakeList;
    end;
  UpdateLabels;
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
              glColor3f(0,0,1);
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
              glColor3f(0.5,0.5,0.5);
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
  glClearColor(1,1,1,1);
  glClearDepth(1.0);
  EyeX:= 0;
  EyeY:= 0;
  MakeCone;
  MakeCoords;
  MakeLimits;
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
  if Mode = 0 then
    gluPerspective(60,ogl.Width/ogl.Height,0.1,LimZ * 100)
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
  LoggerCall;
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
        UpdateLabels;
        SbZoom.Position:= Round(EyeZ);
        ogl.Invalidate;
      end;
end;

procedure TSimClientForm.OglResize(Sender: TObject);
begin
  if Sender = nil then ;
  if AreaInitialized then
    if Assigned(ogl) then
      if ogl.Visible then
        if ogl.Height > 0 then
          glViewport(0,0,ogl.Width,ogl.Height);
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
  glColor4f(1,0.5,0,0.5);
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
      glColor4f(0,0,0.9, 0.1);
      glBegin(GL_QUADS);
        glVertex3f(ML.minX,ML.minY,ML.minZ);
        glVertex3f(ML.maxX,ML.minY,ML.minZ);
        glVertex3f(ML.maxX,ML.maxY,ML.minZ);
        glVertex3f(ML.minX,ML.maxY,ML.minZ);
      glEnd();

      glEnable(GL_LINE_STIPPLE);
      glLineStipple(10, pattern);
      glBegin(GL_LINE_STRIP);
        // glColor3f(0.7,0.1,0.1);
        glColor3f(0.7,0.7,0.6);
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
      glDisable(GL_LINE_STIPPLE);
    end;
  glEndList;
end;


initialization
  {$I simclient.lrs}

end.

