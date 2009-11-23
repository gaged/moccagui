unit simclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  OpenGLContext, StdCtrls, Buttons;

type

  { TSimClientForm }

  TSimClientForm = class(TForm)
    ogl: TOpenGLControl;
    sbX: TScrollBar;
    sbY: TScrollBar;
    spBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure oglDblClick(Sender: TObject);
    procedure oglMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure oglPaint(Sender: TObject);
    procedure oglResize(Sender: TObject);
    procedure sbXChange(Sender: TObject);
    procedure sbYChange(Sender: TObject);
    procedure spBtnClick(Sender: TObject);
  private
    procedure DrawList;
    procedure DrawLimits;
    procedure ZoomLimits;
    procedure SetSoftLimits(x1,y1,z1,x2,y2,z2: Double);
  public
    procedure Reset;
    procedure Update;
    procedure ParseFile(const AFileName: PChar);
  end;
  
var
  clSim: TSimClientForm;

implementation

uses
  sim,gl,glu;

const
  LINE_LEN = 256;

var
  LastError: string;
  Buffer: Array[0..LINE_LEN-1] of Char;

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
  
function GetError(Code: integer): string;
var
  s: string;
begin
  Buffer[0]:= #0;
  s:= 'Fehler: ' + #13#10;
  interp_errortext(Code,PChar(Buffer),255);
  if Buffer[0] <> #0 then
    begin
      s:= s + PChar(Buffer) + #13#10;
      Buffer[0]:= #0;
      interp_line_text(Buffer,255);
      if Buffer[0] <> #0 then
        s:= s + PChar(Buffer);
    end;
  GetError:= s;
  {ifdef debuglc}
  writeln(s);
  {endif}
end;

procedure TSimClientForm.ParseFile(const AFileName: PChar);
var
  status,i: integer;
begin
  if not Assigned(LineItems) then
    LineItems:= TList.Create
  else
    LineItems.Clear;
  if (AFileName = nil) then
    begin
      {ifdef debuglc}
      writeln('no file to parse!');
      {endif}
      Exit;
    end;
  if not Assigned(LineItems) then
    begin
      {ifdef debuglc}
      writeln('cannot write to stringlist, nil!');
      {endif}
      Exit;
    end;
  status:= interp_init;
  if status <> INTP_OK then
    begin
      LastError:= Geterror(status);
      Exit;
    end;
  status:= interp_open(AFileName);
  if status <> INTP_OK then
    begin
      LastError:= Geterror(status);
      Exit;
    end;
  while status = INTP_OK do
    begin
      status:= interp_read(nil);
      if status = INTP_OK then
        status:= interp_execute;
    end;
  if status <> INTP_OK then
    LastError:= Geterror(status);
  interp_close;
  {ifdef debuglc}
  writeln(IntToStr(LineItems.Count));
  {endif}
end;

procedure TSimClientForm.Reset;
begin
  ResetLimits;
  SetSoftLimits(0,0,0,10,10,10);
end;

procedure TSimClientForm.Update;
var
  L: Array[0..5] of Double;
begin
  Getlimits(L);
  SetSoftLimits(L[0],L[1],L[2],L[3],L[4],L[5]);
  ZoomLimits;
  ogl.Invalidate;
end;

procedure TSimClientForm.ZoomLimits;
begin
  if limW < limH then Dist:= limH * 4 else Dist:= limW * 4;
  rotY:= 0;
  rotZ:= 0;
  rotX:= 0;
  PanX:= CenterX;
  PanY:= CenterY;
  PanZ:= CenterZ;
  AreaInitialized:= False;
end;

procedure TSimClientForm.SetSoftLimits(x1,y1,z1,x2,y2,z2: Double);
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
  if limW < limH then Dist:= limH * 4 else Dist:= limW * 4;
end;

procedure TSimClientForm.DrawLimits;
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

procedure TSimClientForm.DrawList;
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

procedure TSimClientForm.oglResize(Sender: TObject);
begin
  if (AreaInitialized) and ogl.MakeCurrent then
    glViewport (0, 0, ogl.Width, ogl.Height);
end;

procedure TSimClientForm.sbXChange(Sender: TObject);
begin
  RotZ:= SbX.Position;
  ogl.Invalidate;
end;

procedure TSimClientForm.sbYChange(Sender: TObject);
begin
  RotX:= sbY.Position;
  ogl.Invalidate;
end;

procedure TSimClientForm.spBtnClick(Sender: TObject);
begin
  //ModePan:= not ModePan;
  // UpdateMode;
end;

procedure TSimClientForm.oglPaint(Sender: TObject);

const GLInitialized: boolean = false;

procedure InitGL;
begin
  if GLInitialized then
    Exit;
  GLInitialized:= True;
  ZoomLimits;
  sbX.Position:= round(rotX);
  sbY.Position:= round(rotY);
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

procedure TSimClientForm.oglMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta < 0 then
    Dist:= Dist / 1.2
  else
    Dist:= Dist * 1.2;
  Handled:= True;
  ogl.Invalidate;
end;

procedure TSimClientForm.FormCreate(Sender: TObject);
begin
  SetSoftLimits(-5,-5,-5,5,5,5);
  AreaInitialized:= False;
end;

procedure TSimClientForm.FormDestroy(Sender: TObject);
begin
  if Assigned(LineItems) then
    LineItems.Free;
end;

procedure TSimClientForm.oglDblClick(Sender: TObject);
begin
  ZoomLimits;
  ogl.Invalidate;
end;



initialization
  {$I simclient.lrs}

end.

