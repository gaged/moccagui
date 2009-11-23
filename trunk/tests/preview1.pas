unit preview1;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils,LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  OpenGLContext, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnOpen: TButton;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    OpenDialog: TOpenDialog;
    ogl: TOpenGLControl;
    sbZ: TScrollBar;
    sbY: TScrollBar;
    procedure BtnOpenClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure oglMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure oglMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure oglPaint(Sender: TObject);
    procedure oglResize(Sender: TObject);
    procedure sbYChange(Sender: TObject);
    procedure sbZChange(Sender: TObject);
  private
    procedure DrawList;
    procedure DrawLimits;
    procedure SetSoftLimits(x1,y1,z1,x2,y2,z2: Double);
    // procedure Reset;
  end;

var
  Form1: TForm1; 
  
implementation

{ TForm1 }

uses
  math,linecode,sim,gl,glu;

var
  AreaInitialized: Boolean;

var
  rotX,rotY,rotZ: glFloat;
  panX,panY,panZ: glFloat;

  oldx,oldy,oldz: glFloat;

  limW,limH,limD: Double;

  lminx, lmaxx,
  lminy, lmaxy,
  lminz, lmaxz,

  centerx, centery, centerz: Double;

  Dist: Double;
  

procedure TForm1.SetSoftLimits(x1,y1,z1,x2,y2,z2: Double);
begin
  lminx:= x1; lmaxx:= x2;
  lminy:= y1; lmaxy:= y2;
  lminz:= z1; lmaxz:= z2;
  limW:= (lmaxx - lminx);
  limH:= (lmaxy - lminy);
  limD:= (lmaxz - lminz);
  centerx:= lmaxx - (limW / 2);
  centery:= lmaxy - (limH / 2);
  centerz:= lmaxz - (limD / 2);
  if limW < limH then Dist:= limH * 10 else Dist:= limW * 10;
end;

procedure TForm1.DrawLimits;
const
  pattern = $5555;
begin
  //Stippling aktivieren
  glEnable(GL_LINE_STIPPLE);
  glLineStipple(10, pattern);
  glBegin(GL_LINE_STRIP);
    glColor3f(0.7,0.1,0.1);
    glVertex3f(lminx,lminy,lminz);
    glVertex3f(lmaxx,lminy,lminz);
    glVertex3f(lmaxx,lmaxy,lminz);
    glVertex3f(lminx,lmaxy,lminz);
    glVertex3f(lminx,lminy,lminz);
    glVertex3f(lminx,lminy,lmaxz);
    glVertex3f(lmaxx,lminy,lmaxz);
    glVertex3f(lmaxx,lmaxy,lmaxz);
    glVertex3f(lminx,lmaxy,lmaxz);
    glVertex3f(lminx,lminy,lmaxz);
    glVertex3f(lminx,lminy,lminz);
  glEnd;
  glBegin(GL_LINES);
    glVertex3f(lminx,lmaxy,lmaxz);
    glVertex3f(lminx,lmaxy,lminz);
    glVertex3f(lmaxx,lmaxy,lmaxz);
    glVertex3f(lmaxx,lmaxy,lminz);
    glVertex3f(lmaxx,lminy,lmaxz);
    glVertex3f(lmaxx,lminy,lminz);
  glEnd;
  glDisable(GL_LINE_STIPPLE);
end;

procedure TForm1.DrawList;
var
 i: integer;
 P: PLineItem;
begin
  if not Assigned(LineItems) then Exit;
  if LineItems.Count < 1 then Exit;
  // glEnable(GL_LINE_SMOOTH);
  glBegin(GL_LINES);
    for i:= 0 to LineItems.Count - 1 do
    begin
      P:= PLineItem(LineItems[i]);
      if P = nil then
        Break;
      if P^.ltype = ltFeed then
        glColor3f(1,1,1)
      else
        if P^.ltype = ltArc then
          glColor3f(1,0,1)
       else
         glColor3f(0.2,0.2,0.3);
      glVertex3f(oldx,oldy,oldz);
      glVertex3f(P^.x,P^.y,P^.z);
      oldx:= P^.x;
      oldy:= P^.y;
      oldz:= P^.z;
    end;
  // glLineWidth(1);
  glEnd;
end;


procedure ZoomLimits;
begin
  if limW < limH then Dist:= limH * 2 else Dist:= limW * 2;
  rotY:= 0;
  rotZ:= 0;
  rotX:= 0;
  PanX:= CenterX;
  PanY:= CenterY;
  PanZ:= CenterZ;
  AreaInitialized:= False;
end;

procedure TForm1.BtnOpenClick(Sender: TObject);
var
 s: string;
 i: integer;
begin
  if OpenDialog.Execute then
    begin
      s:= OpenDialog.FileName;
      minx:= 0; maxx:= 0;
      miny:= 0; maxy:= 0;
      minz:= 0; maxz:= 0;
      i:= ParseFile(PChar(s));
      Writeln('Parsefile: ' + intToStr(i));
      S:= GetError(i);
      Writeln(S);
      SetSoftLimits(minx,miny,minz,maxx,maxy,maxz);
      ZoomLimits;
      ogl.Invalidate;
    end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Label1.Caption:= Format('%n %n %n',[centerx,centery,centerz]);
  Label2.Caption:= Format('%n %n %n',[limw,limh,limd]);
  Label3.Caption:= Format('%n %n %n %n %n %n',[lminx,lmaxx,lminy,lmaxy,lminz,lmaxz]);
end;

procedure TForm1.FormCreate(Sender: TObject);
const
  toolfile: PChar = '/home/gtom/mocca/tests/sim_mm.tbl';
var
  i: integer;
begin
  if settool(0,0,0,0.1,0,0,0,0) = 0 then
  if settool(1,1,0,0.1,0,0,0,0) = 0 then
  if settool(2,2,0,0.1,0,0,0,0) = 0 then
  if settool(3,3,0,0.1,0,0,0,0) = 0 then
  if settool(4,4,0,0.1,0,0,0,0) = 0 then
  if settool(5,5,0,0.2,0,0,0,0) = 0 then
    begin
      writeln('Tool set');
      writeln(IntToStr(active_slot));
    end;
  SetSoftLimits(-0,-0,-0,10,10,10);
  ZoomLimits;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(LineItems) then
    LineItems.Free;
end;

procedure TForm1.oglMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Dist:= Dist / 1.2;
  Handled:= True;
  ogl.Invalidate;
end;

procedure TForm1.oglMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  Dist:= Dist * 1.2;
  Handled:= True;
  ogl.Invalidate;
end;

procedure TForm1.oglPaint(Sender: TObject);

const GLInitialized: boolean = false;

procedure InitGL;
begin
  if GLInitialized then
    Exit;
  GLInitialized:= True;
  ZoomLimits;
  sbY.Position:= round(rotX);
  sbZ.Position:= round(rotY);
  glClearColor(0,0,0,1);
  glClearDepth(1.0);
end;

begin
  if ogl.MakeCurrent then
    begin
      if not AreaInitialized then
        begin
          InitGL;
          glMatrixMode (GL_PROJECTION);    { prepare for and then }
          glLoadIdentity;                  { define the projection }
          gluPerspective(45,1,0.5,LimD * 100);
          glMatrixMode (GL_MODELVIEW);  { back to modelview matrix }
          glViewport (0,0,ogl.Width,ogl.Height); { define the viewport }
          AreaInitialized:=true;
        end;

      glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

      glLoadIdentity;             { clear the matrix }

      gluLookAt (0,0,Dist,0,0,0,0,1,0);
      
      // glTranslatef (0,0,-Dist - abs(limD));
      glRotatef(rotX,1.0,0.0,0.0);
      glRotatef(rotY,0.0,1.0,0.0);
      glRotatef(rotZ,0.0,0.0,1.0);
      glTranslatef (-centerx,-centery,-centerz); { viewing transformation }
      
      glPushMatrix;

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

      DrawLimits;
      DrawList;

      glPopMatrix;

      ogl.SwapBuffers;
    end;
end;

procedure TForm1.oglResize(Sender: TObject);
begin
 if (AreaInitialized) and ogl.MakeCurrent then
    glViewport (0, 0, ogl.Width, ogl.Height);
end;

procedure TForm1.sbYChange(Sender: TObject);
begin
   rotX:= sbY.Position;
  ogl.Invalidate;
end;

procedure TForm1.sbZChange(Sender: TObject);
begin
  rotZ:= sbZ.Position;
  //Dist:= 100;
  //AngXY:= sbZ.Position * (pi/180);
  //CalcView;
  ogl.Invalidate;
end;


initialization
  {$I preview1.lrs}

end.

procedure TForm1.oglMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseLastX:= X;
  MouseLastY:= Y;
  MouseDragging:= True;
end;

procedure TForm1.oglMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  dx,dy: integer;
  FRepaint: Boolean;
begin
  if not MouseDragging then
    Exit;
  FRepaint:= False;
  dx:= MouseLastX - X;
  dy:= Y - MouseLastY;
  if (dx <> 0) then
    begin
      tx:= dx / ZoomValue;
      FRepaint:= True;
    end;
  if (dy <> 0) then
    begin
      ty:= dy / ZoomValue;
      FRepaint:= True;
    end;
 if FRepaint then
   ogl.Invalidate;
end;

procedure TForm1.oglMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  MouseDragging:= False;
end;


