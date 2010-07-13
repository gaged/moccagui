unit glfont;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, glu,gl;

// The OpenGl Font

type
  TGlFontOrientation = (glfX,glfY,glfZ);

procedure DrawGlText(x,y,z: Double; fo: TGlFontOrientation; Scale: Double; S: string);
procedure DrawGlDigit(C: Char);
procedure BuildGlFont;

const
  GlFontDist = 0.8;

implementation

type
  TSegment = Array[0..3] of single;

const
  Segments: Array[0..6] of TSegment =
    ((0,1,0.5,1),
     (0,1,0,0.5),
     (0.5,1,0.5,0.5),
     (0,0.5,0.5,0.5),
     (0,0,0,0.5),
     (0.5,0,0.5,0.5),
     (0,0,0.5,0));

const
  DigitBase = 10;


var
  DigitsInitialized: Boolean;

procedure AddSegment(SegNo: integer);
var
  S: TSegment;
begin
  if (SegNo < 0) or (SegNo > 6) then Exit;
  S:= Segments[SegNo];
  glBegin(GL_LINES);
  glVertex3f(S[0],S[1],0);
  glVertex3f(S[2],S[3],0);
  glEnd;
end;

procedure MakeDigit(Id: integer; a,b,c,d,e,f,g: Boolean);
var
  ListId: integer;
begin
  if (Id < 0) or (Id > 9) then Exit;
  ListId:= DigitBase + Id;
  glDeleteLists(ListId,1);
  glNewList(ListId,GL_COMPILE);
  if a then AddSegment(0);
  if b then AddSegment(1);
  if c then AddSegment(2);
  if d then AddSegment(3);
  if e then AddSegment(4);
  if f then AddSegment(5);
  if g then AddSegment(6);
  glEndList;
end;

procedure MakeDot;
var
  ListId: integer;
begin
  ListId:= DigitBase + 11;
  glDeleteLists(ListId,1);
  glNewList(ListId,GL_COMPILE);
  glBegin(GL_QUADS);
  glVertex3f(0.2,0,0);
  glVertex3f(0.3,0,0);
  glVertex3f(0.3,0.1,0);
  glVertex3f(0.2,0.1,0);
  glEnd();
  glEndList;
end;

procedure BuildGlFont;
begin
  MakeDigit(0,true,true,true,false,true,true,true);
  MakeDigit(1,false,false,true,false,false,true,false);
  MakeDigit(2,true,false,true,true,true,false,true);
  MakeDigit(3,true,false,true,true,false,true,true);
  MakeDigit(4,false,true,true,true,false,true,false);
  MakeDigit(5,true,true,false,true,false,true,true);
  MakeDigit(6,false,true,false,true,true,true,true);
  MakeDigit(7,true,false,true,false,false,true,false);
  MakeDigit(8,true,true,true,true,true,true,true);
  MakeDigit(9,true,true,true,true,false,true,true);
  MakeDigit(10,false,false,false,true,false,false,false);
  MakeDot;
  DigitsInitialized:= True;
end;

procedure DrawGlDigit(C: Char);
begin
  if not DigitsInitialized then
    Exit;
  case C of
    '0': glCallList(DigitBase + 0);
    '1': glCallList(DigitBase + 1);
    '2': glCallList(DigitBase + 2);
    '3': glCallList(DigitBase + 3);
    '4': glCallList(DigitBase + 4);
    '5': glCallList(DigitBase + 5);
    '6': glCallList(DigitBase + 6);
    '7': glCallList(DigitBase + 7);
    '8': glCallList(DigitBase + 8);
    '9': glCallList(DigitBase + 9);
    '-': glCallList(DigitBase + 10);
    ',','.': glCallList(DigitBase + 11);
  end;
end;

procedure DrawGlText(x,y,z: Double;fo: TGlFontOrientation;Scale: Double; S: string);
var
  i: integer;
  dx,dy,dz: double;
begin
  if (S = '') or (Scale = 0) then Exit;
  dx:= x / Scale;
  dy:= y / Scale;
  dz:= z / Scale;
  glPushMatrix;
  glScaleF(Scale,Scale,Scale);
  if fo = glfX then
    begin
      glTranslateF(dx,dy,dz);
    end
  else
  if fo = glfY then
    begin
      glRotateF(90,0,0,1);
      glTranslateF(dx,dy,dz);
    end
  else
    begin
      glRotateF(90,1,0,0);
      glTranslateF(dx,dy,dz);
    end;
  for i:= 1 to Length(S) do
    begin
      DrawGlDigit(S[i]);
      glTranslateF(1.1,0,0);
    end;
  glPopMatrix;
end;


end.

