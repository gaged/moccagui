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
    MItemReload: TMenuItem;
    Popup: TPopupMenu;
    sbH: TScrollBar;
    sbV: TScrollBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MItem3DClick(Sender: TObject);
    procedure MItemReloadClick(Sender: TObject);
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
    DefaultCone: Boolean;
    ExtX,ExtY,ExtZ: double;
    L: TExtents;
    Offset: tlo;
    ConeL,LimitsL,CoordsL,ListL: gluInt;
    MouseX,MouseY: integer;
    Moving: Boolean;
    AreaInitialized: Boolean;
    ZoomMax: integer;
    View3D: Boolean;

    procedure MakeList;
    procedure MakeCone;
    procedure MoveCone(X,Y,Z,A,B,C: Double);
    procedure DrawCone;
    procedure Pan(DX,DY: integer);
    procedure UpdateView;
    procedure UpdateDim;
    procedure DrawDim;
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
  mocjoints,emc2pas, glcanon,
  logger, gltools, glfont, simulator;

var
  Tool: TTool;

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
          View3D:= Show3DPreviewDlg(@ListL);
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

  ConeL:= 0; LimitsL:= 0; CoordsL:= 0; ListL:= 0;

  ZoomMax:= 10;

  DimScale:= 1;
  DimDist:= 1;

  View3D:= False;

  if Vars.IsLathe then
    begin
      sbV.Visible:= False;
      sbH.Visible:= False;
    end
  else
    begin
      sbV.setParams(Round(ViewRotation[FViewMode].xrot),-180,180,1);
      sbH.SetParams(Round(ViewRotation[FViewMode].yrot),-180,180,1);
    end;

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
  if Vars.IsLathe then
    Ogl.SetBounds(0,0,Self.ClientWidth,Self.ClientHeight)
  else
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

procedure TSimClientForm.MItemReloadClick(Sender: TObject);
begin
  ReloadFile;
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
begin
  if not AreaInitialized then Exit;
  PanView(dx,dy,CenterX,CenterY,CenterZ,Ogl.Height,PanX,PanY);
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

procedure TSimClientForm.DrawDim;
begin
  if Assigned(ogl) and AreaInitialized then
    begin
      if (Vars.AxisMask AND 1) <> 0 then
        if Vars.IsLathe then DrawDimXLathe(ExtX,L)
      else
        DrawDimX(ExtX,L);
      if (Vars.AxisMask AND 2) <> 0 then DrawDimY(ExtY,L);
      if (Vars.AxisMask AND 4) <> 0 then DrawDimZ(ExtZ,L);
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
  Centerx:= L.maxX - (ExtX / 2);
  Centery:= L.maxY - (ExtY / 2);
  Centerz:= L.maxZ - (ExtZ / 2);
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
      // Neu: Lathe Y = X
      if Vars.IsLathe then
        begin
          if ExtZ > ExtX then
            ExtY:= ExtZ
          else
            ExtY:= ExtX;
        end;
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
          MakeCoords(CoordsL);
          MakeLimits(View3D,LimitsL);
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
  EyeX:= 0;
  EyeY:= 0;
  if Assigned(Ogl) then
    begin
      if (ogl.Height > 0) and (ogl.Width > 0) then
        glViewPort(0,0,ogl.Width,ogl.Height);
      MakeCone;
      MakeCoords(CoordsL);
      MakeLimits(View3D,LimitsL);
      BuildGlFont;
    end;
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
      if LimitsL > 0 then
        glCallList(LimitsL);

      glPushMatrix;
      x:= ToCanonUnits(GetOrigin(0));
      y:= ToCanonUnits(GetOrigin(1));
      z:= ToCanonUnits(GetOrigin(2));
      glTranslatef(x,y,z);
      if CoordsL > 0 then
        glCallList(CoordsL);
      glPopMatrix;
    end
  else
    begin
      Lightning(0,0,-ExtZ * 5);
    end;

  if ListL > 0 then
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

procedure TSimClientForm.MakeCone;
var
  orient: integer;
  d,r,tl: double;
begin
  if not Assigned(ogl) then Exit;
  if Vars.IsLathe and (Tool.toolno > 0) and (Tool.Orientation > 0) then
    begin
      orient:= Tool.Orientation;
      if orient > 9 then orient:= 9;
      r:= ToCanonUnits(Tool.diameter) / 2;
      MakeLatheCone(ConeL,orient,r,Tool.frontangle,Tool.backangle);
      DefaultCone:= False;
    end
  else
    begin
      d:= ToCanonUnits(Tool.diameter);
      if d < DEFAULT_TOOL_DIA then
      d:= DEFAULT_TOOL_DIA;
      r:= d / 2;
      tl:= ToCanonUnits(Tool.zoffset);
      if tl < DEFAULT_TOOL_LENGTH then
        tl:= DEFAULT_TOOL_LENGTH;
      MakeMillCone(ConeL,r,tl);
      DefaultCone:= True;
    end;
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
        end;
    end
  else
    if DefaultCone then
      glRotatef(90,0,1,0);
  if ConeL > 0 then
    glCallList(ConeL);
  glPopMatrix;
end;

initialization
  {$I simclient.lrs}

end.

